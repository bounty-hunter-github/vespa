// Copyright Yahoo. Licensed under the terms of the Apache 2.0 license. See LICENSE in the project root.
package com.yahoo.vespa.model.container.http.ssl;

import com.yahoo.config.model.api.EndpointCertificateSecrets;
import com.yahoo.jdisc.http.ConnectorConfig;
import com.yahoo.jdisc.http.ConnectorConfig.Ssl.ClientAuth;
import com.yahoo.security.tls.TlsContext;
import com.yahoo.vespa.model.container.http.ConnectorFactory;

import java.time.Duration;
import java.util.Collection;
import java.util.List;

/**
 * Component specification for {@link com.yahoo.jdisc.http.server.jetty.ConnectorFactory} with hosted specific configuration.
 *
 * @author bjorncs
 */
public class HostedSslConnectorFactory extends ConnectorFactory {

    private static final List<String> INSECURE_WHITELISTED_PATHS = List.of("/status.html");
    private static final String DEFAULT_HOSTED_TRUSTSTORE = "/opt/yahoo/share/ssl/certs/athenz_certificate_bundle.pem";

    private final boolean enforceClientAuth;
    private final boolean enforceHandshakeClientAuth;
    private final Collection<String> tlsCiphersOverride;
    private final boolean enableProxyProtocolMixedMode;
    private final Duration endpointConnectionTtl;

    /**
     * Create connector factory that uses a certificate provided by the config-model / configserver and default hosted Vespa truststore.
     */
    public static HostedSslConnectorFactory withProvidedCertificate(
            String serverName, EndpointCertificateSecrets endpointCertificateSecrets, boolean enforceHandshakeClientAuth,
            Collection<String> tlsCiphersOverride, boolean enableProxyProtocolMixedMode, int port,
            Duration endpointConnectionTtl, boolean enableTokenSupport) {
        CloudSslProvider sslProvider = createConfiguredDirectSslProvider(
                serverName, endpointCertificateSecrets, DEFAULT_HOSTED_TRUSTSTORE, /*tlsCaCertificates*/null, enforceHandshakeClientAuth, enableTokenSupport);
        return new HostedSslConnectorFactory(sslProvider, false, enforceHandshakeClientAuth, tlsCiphersOverride,
                                             enableProxyProtocolMixedMode, port, endpointConnectionTtl);
    }

    /**
     * Create connector factory that uses a certificate provided by the config-model / configserver and a truststore configured by the application.
     */
    public static HostedSslConnectorFactory withProvidedCertificateAndTruststore(
            String serverName, EndpointCertificateSecrets endpointCertificateSecrets, String tlsCaCertificates,
            Collection<String> tlsCiphersOverride, boolean enableProxyProtocolMixedMode, int port,
            Duration endpointConnectionTtl, boolean enableTokenSupport) {
        CloudSslProvider sslProvider = createConfiguredDirectSslProvider(
                serverName, endpointCertificateSecrets, /*tlsCaCertificatesPath*/null, tlsCaCertificates, false, enableTokenSupport);
        return new HostedSslConnectorFactory(sslProvider, true, false, tlsCiphersOverride, enableProxyProtocolMixedMode,
                                             port, endpointConnectionTtl);
    }

    /**
     * Create connector factory that uses the default certificate and truststore provided by Vespa (through Vespa-global TLS configuration).
     */
    public static HostedSslConnectorFactory withDefaultCertificateAndTruststore(String serverName, Collection<String> tlsCiphersOverride,
                                                                                boolean enableProxyProtocolMixedMode, int port,
                                                                                Duration endpointConnectionTtl) {
        return new HostedSslConnectorFactory(new DefaultSslProvider(serverName), true, false, tlsCiphersOverride,
                                             enableProxyProtocolMixedMode, port, endpointConnectionTtl);
    }

    private HostedSslConnectorFactory(SslProvider sslProvider, boolean enforceClientAuth,
                                      boolean enforceHandshakeClientAuth, Collection<String> tlsCiphersOverride,
                                      boolean enableProxyProtocolMixedMode, int port, Duration endpointConnectionTtl) {
        super(new Builder("tls"+port, port).sslProvider(sslProvider));
        this.enforceClientAuth = enforceClientAuth;
        this.enforceHandshakeClientAuth = enforceHandshakeClientAuth;
        this.tlsCiphersOverride = tlsCiphersOverride;
        this.enableProxyProtocolMixedMode = enableProxyProtocolMixedMode;
        this.endpointConnectionTtl = endpointConnectionTtl;
    }

    private static CloudSslProvider createConfiguredDirectSslProvider(
            String serverName, EndpointCertificateSecrets endpointCertificateSecrets, String tlsCaCertificatesPath, String tlsCaCertificates, boolean enforceHandshakeClientAuth, boolean enableTokenSupport) {
        var clientAuthentication = enforceHandshakeClientAuth ? ClientAuth.Enum.NEED_AUTH : ClientAuth.Enum.WANT_AUTH;
        return new CloudSslProvider(
                serverName,
                endpointCertificateSecrets.key(),
                endpointCertificateSecrets.certificate(),
                tlsCaCertificatesPath,
                tlsCaCertificates,
                clientAuthentication,
                enableTokenSupport);
    }

    @Override
    public void getConfig(ConnectorConfig.Builder connectorBuilder) {
        super.getConfig(connectorBuilder);
        if (! enforceHandshakeClientAuth) {
            connectorBuilder
                    .tlsClientAuthEnforcer(new ConnectorConfig.TlsClientAuthEnforcer.Builder()
                            .pathWhitelist(INSECURE_WHITELISTED_PATHS)
                            .enable(enforceClientAuth));
        }
        // Disables TLSv1.3 as it causes some browsers to prompt user for client certificate (when connector has 'want' auth)
        connectorBuilder.ssl.enabledProtocols(List.of("TLSv1.2"));

        if (!tlsCiphersOverride.isEmpty()) {
            connectorBuilder.ssl.enabledCipherSuites(tlsCiphersOverride.stream().sorted().toList());
        } else {
            connectorBuilder.ssl.enabledCipherSuites(TlsContext.ALLOWED_CIPHER_SUITES.stream().sorted().toList());
        }

        connectorBuilder
                .proxyProtocol(new ConnectorConfig.ProxyProtocol.Builder().enabled(true).mixedMode(enableProxyProtocolMixedMode))
                .idleTimeout(Duration.ofSeconds(30).toSeconds())
                .maxConnectionLife(endpointConnectionTtl != null ? endpointConnectionTtl.toSeconds() : 0);
    }
}
