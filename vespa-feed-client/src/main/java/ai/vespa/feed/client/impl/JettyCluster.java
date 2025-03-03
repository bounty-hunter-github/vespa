// Copyright Yahoo. Licensed under the terms of the Apache 2.0 license. See LICENSE in the project root.

package ai.vespa.feed.client.impl;

import ai.vespa.feed.client.FeedClientBuilder.Compression;
import ai.vespa.feed.client.HttpResponse;
import org.eclipse.jetty.client.HttpClient;
import org.eclipse.jetty.client.MultiplexConnectionPool;
import org.eclipse.jetty.client.api.Request;
import org.eclipse.jetty.client.api.Response;
import org.eclipse.jetty.client.api.Result;
import org.eclipse.jetty.client.util.AbstractRequestContent;
import org.eclipse.jetty.client.util.BufferingResponseListener;
import org.eclipse.jetty.http.HttpField;
import org.eclipse.jetty.http.HttpHeader;
import org.eclipse.jetty.http.HttpMethod;
import org.eclipse.jetty.http.HttpVersion;
import org.eclipse.jetty.http2.client.HTTP2Client;
import org.eclipse.jetty.http2.client.http.HttpClientTransportOverHTTP2;
import org.eclipse.jetty.io.ClientConnector;
import org.eclipse.jetty.util.Callback;
import org.eclipse.jetty.util.HttpCookieStore;
import org.eclipse.jetty.util.Pool;
import org.eclipse.jetty.util.Promise;
import org.eclipse.jetty.util.SocketAddressResolver;
import org.eclipse.jetty.util.component.AbstractLifeCycle;
import org.eclipse.jetty.util.ssl.SslContextFactory;
import org.eclipse.jetty.util.thread.QueuedThreadPool;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.Inet4Address;
import java.net.InetSocketAddress;
import java.net.URI;
import java.nio.ByteBuffer;
import java.time.Duration;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.zip.GZIPOutputStream;

import static ai.vespa.feed.client.FeedClientBuilder.Compression.auto;
import static ai.vespa.feed.client.FeedClientBuilder.Compression.gzip;
import static java.util.concurrent.TimeUnit.MILLISECONDS;
import static org.eclipse.jetty.http.MimeTypes.Type.APPLICATION_JSON;

/**
 * Client implementation based on Jetty HTTP Client
 *
 * @author bjorncs
 */
class JettyCluster implements Cluster {

    // Socket timeout must be longer than the longest feasible response timeout
    private static final Duration IDLE_TIMEOUT = Duration.ofMinutes(15);

    private final HttpClient client;
    private final List<Endpoint> endpoints;
    private final Compression compression;

    JettyCluster(FeedClientBuilderImpl b) throws IOException {
        this.client = createHttpClient(b);
        this.endpoints = b.endpoints.stream().map(Endpoint::new).collect(Collectors.toList());
        this.compression = b.compression;
    }

    @Override
    public void dispatch(HttpRequest req, CompletableFuture<HttpResponse> vessel) {
        client.getExecutor().execute(() -> {
            try {
                Endpoint endpoint = findLeastBusyEndpoint(endpoints);
                long reqTimeoutMillis = req.timeout() != null
                        ? req.timeout().toMillis() * 11 / 10 + 1000 : IDLE_TIMEOUT.toMillis();
                Request jettyReq = client.newRequest(URI.create(endpoint.uri + req.path()))
                        .version(HttpVersion.HTTP_2)
                        .method(HttpMethod.fromString(req.method()))
                        .headers(hs -> req.headers().forEach((k, v) -> hs.add(k, v.get())))
                        .idleTimeout(IDLE_TIMEOUT.toMillis(), MILLISECONDS)
                        .timeout(reqTimeoutMillis, MILLISECONDS);
                if (req.body() != null) {
                    FeedContent content = new FeedContent(compression, req.body());
                    content.contentEncoding().ifPresent(ce -> jettyReq.headers(hs -> hs.add(ce)));
                    jettyReq.body(content);
                }
                jettyReq.send(new BufferingResponseListener() {
                    @Override
                    public void onComplete(Result result) {
                        if (result.isFailed()) vessel.completeExceptionally(result.getFailure());
                        else vessel.complete(new JettyResponse(result.getResponse(), getContent()));
                    }
                });
            } catch (Exception e) {
                vessel.completeExceptionally(e);
            }
        });
    }

    @Override
    public void close() {
        try {
            client.stop();
        } catch (Exception e) { throw new RuntimeException(e); }
    }

    private static HttpClient createHttpClient(FeedClientBuilderImpl b) throws IOException {
        SslContextFactory.Client clientSslCtxFactory = new SslContextFactory.Client();
        clientSslCtxFactory.setSslContext(b.constructSslContext());
        if (b.hostnameVerifier != null) {
            clientSslCtxFactory.setHostnameVerifier(b.hostnameVerifier);
            // Disable built-in hostname verification in the JDK's TLS implementation
            clientSslCtxFactory.setEndpointIdentificationAlgorithm(null);
        }
        ClientConnector connector = new ClientConnector();
        int threads = Math.max(Math.min(Runtime.getRuntime().availableProcessors(), 20), 8);
        connector.setExecutor(new QueuedThreadPool(threads));
        connector.setSslContextFactory(clientSslCtxFactory);
        HTTP2Client h2Client = new HTTP2Client(connector);
        h2Client.setMaxConcurrentPushedStreams(b.maxStreamsPerConnection);
        // Set the HTTP/2 flow control windows very large to cause TCP congestion instead of HTTP/2 flow control congestion.
        int initialWindow = 128 * 1024 * 1024;
        h2Client.setInitialSessionRecvWindow(initialWindow);
        h2Client.setInitialStreamRecvWindow(initialWindow);
        HttpClientTransportOverHTTP2 transport = new HttpClientTransportOverHTTP2(h2Client);
        transport.setConnectionPoolFactory(dest -> {
            MultiplexConnectionPool pool = new MultiplexConnectionPool(
                    dest, Pool.StrategyType.RANDOM, b.connectionsPerEndpoint, false, dest, Integer.MAX_VALUE);
            pool.preCreateConnections(b.connectionsPerEndpoint);
            return pool;
        });
        HttpClient httpClient = new HttpClient(transport);
        httpClient.setFollowRedirects(false);
        httpClient.setUserAgentField(
                new HttpField(HttpHeader.USER_AGENT, String.format("vespa-feed-client/%s (Jetty)", Vespa.VERSION)));
        httpClient.setConnectTimeout(Duration.ofSeconds(10).toMillis());
        // Stop client from trying different IP address when TLS handshake fails
        httpClient.setSocketAddressResolver(new Ipv4PreferringResolver(httpClient, Duration.ofSeconds(10)));
        httpClient.setCookieStore(new HttpCookieStore.Empty());

        httpClient.setIdleTimeout(IDLE_TIMEOUT.toMillis());
        try {
            httpClient.start();
        } catch (Exception e) {
            throw new IOException(e);
        }
        return httpClient;
    }

    private static Endpoint findLeastBusyEndpoint(List<Endpoint> endpoints) {
        Endpoint leastBusy = endpoints.get(0);
        int minInflight = leastBusy.inflight.get();
        for (int i = 1; i < endpoints.size(); i++) {
            Endpoint endpoint = endpoints.get(i);
            int inflight = endpoint.inflight.get();
            if (inflight < minInflight) {
                leastBusy = endpoint;
                minInflight = inflight;
            }
        }
        return leastBusy;
    }

    private static int portOf(URI u) {
        return u.getPort() == -1 ? u.getScheme().equals("http") ? 80 : 443 : u.getPort();
    }

    private static class JettyResponse implements HttpResponse {
        final Response response;
        final byte[] content;

        JettyResponse(Response response, byte[] content) { this.response = response; this.content = content; }

        @Override public int code() { return response.getStatus(); }
        @Override public byte[] body() { return content; }
        @Override public String contentType() { return response.getHeaders().get(HttpHeader.CONTENT_TYPE); }
    }

    private static class Endpoint {
        final AtomicInteger inflight = new AtomicInteger();
        final String uri;
        Endpoint(URI uri) { this.uri = String.format("%s://%s:%s", uri.getScheme(), uri.getHost(), portOf(uri)); }
    }

    private static class FeedContent extends AbstractRequestContent {
        final Compression compression;
        final byte[] body;

        FeedContent(Compression compression, byte[] body) {
            super(APPLICATION_JSON.asString());
            this.compression = compression;
            this.body = body;
        }

        @Override public boolean isReproducible() { return true; }
        @Override public long getLength() { return shouldCompress() ? -1 : body.length; }
        Optional<HttpField> contentEncoding() {
            return shouldCompress() ? Optional.of(new HttpField(HttpHeader.CONTENT_ENCODING, "gzip")) : Optional.empty();
        }

        @Override
        public Subscription newSubscription(Consumer consumer, boolean emitInitialContent) {
            return new SubscriptionImpl(consumer, emitInitialContent);
        }

        boolean shouldCompress() { return compression == gzip || compression == auto && body.length > 512; }

        class SubscriptionImpl extends AbstractSubscription {
            SubscriptionImpl(Consumer consumer, boolean emitInitialContent) { super(consumer, emitInitialContent); }

            @Override
            protected boolean produceContent(Producer producer) {
                byte[] bytes;
                if (shouldCompress()) {
                    ByteArrayOutputStream buffer = new ByteArrayOutputStream(1 << 10);
                    try (GZIPOutputStream zip = new GZIPOutputStream(buffer)) {
                        zip.write(body);
                    } catch (IOException e) { throw new UncheckedIOException(e); }
                    bytes = buffer.toByteArray();
                } else {
                    bytes = body;
                }
                return producer.produce(ByteBuffer.wrap(bytes), true, Callback.NOOP);
            }
        }
    }

    private static class Ipv4PreferringResolver extends AbstractLifeCycle implements SocketAddressResolver {

        final HttpClient client;
        final Duration timeout;
        SocketAddressResolver.Async instance;

        Ipv4PreferringResolver(HttpClient client, Duration timeout) { this.client = client; this.timeout = timeout; }

        @Override
        protected void doStart() {
            this.instance = new SocketAddressResolver.Async(client.getExecutor(), client.getScheduler(), timeout.toMillis());
        }

        @Override
        public void resolve(String host, int port, Promise<List<InetSocketAddress>> promise) {
            instance.resolve(host, port, new Promise.Wrapper<List<InetSocketAddress>>(promise) {
                @Override
                public void succeeded(List<InetSocketAddress> result) {
                    if (result.size() <= 1) {
                        getPromise().succeeded(result);
                        return;
                    }
                    List<InetSocketAddress> ipv4Addresses = result.stream()
                            .filter(addr -> addr.getAddress() instanceof Inet4Address).collect(Collectors.toList());
                    if (ipv4Addresses.isEmpty()) {
                        getPromise().succeeded(result);
                        return;
                    }
                    getPromise().succeeded(ipv4Addresses);
                }
            });
        }
    }
}
