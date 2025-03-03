// Copyright Yahoo. Licensed under the terms of the Apache 2.0 license. See LICENSE in the project root.
package com.yahoo.vespa.hosted.provision.maintenance;

import com.yahoo.concurrent.DaemonThreadFactory;
import com.yahoo.config.provision.ApplicationId;
import com.yahoo.config.provision.Deployer;
import com.yahoo.jdisc.Metric;
import com.yahoo.vespa.hosted.provision.NodeRepository;

import java.time.Duration;
import java.time.Instant;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;

/**
 * @author bratseth
 * @author mpolden
 */
public abstract class ApplicationMaintainer extends NodeRepositoryMaintainer {

    private final Deployer deployer;
    private final Metric metric;
    private final CopyOnWriteArrayList<ApplicationId> pendingDeployments = new CopyOnWriteArrayList<>();

    // Use a fixed thread pool to avoid overload on config servers. Resource usage when deploying varies
    // a lot between applications, so doing one by one avoids issues where one or more resource-demanding
    // deployments happen simultaneously
    private final ThreadPoolExecutor deploymentExecutor = new ThreadPoolExecutor(1, 1,
                                                                                 0L, TimeUnit.MILLISECONDS,
                                                                                 new LinkedBlockingQueue<>(),
                                                                                 new DaemonThreadFactory("node repo application maintainer"));

    protected ApplicationMaintainer(Deployer deployer, Metric metric, NodeRepository nodeRepository, Duration interval) {
        super(nodeRepository, interval, metric);
        this.deployer = deployer;
        this.metric = metric;
    }

    @Override
    protected final double maintain() {
        applicationsNeedingMaintenance().forEach(this::deploy);
        return 1.0;
    }

    protected final Deployer deployer() { return deployer; }

    /** Returns the number of deployments that are pending execution */
    public int pendingDeployments() {
        return pendingDeployments.size();
    }

    /** Returns whether given application should be deployed at this moment in time */
    protected abstract boolean canDeployNow(ApplicationId application);

    /** Returns the applications that should be maintained by this now. */
    protected abstract Map<ApplicationId, String> applicationsNeedingMaintenance();

    /**
     * Redeploy this application.
     *
     * The default implementation deploys asynchronously to make sure we do all applications timely
     * even when deployments are slow.
     */
    protected void deploy(ApplicationId application, String reason) {
        if (pendingDeployments.addIfAbsent(application)) { // Avoid queuing multiple deployments for same application
            deploymentExecutor.execute(() -> deployNow(application, reason));
        }
    }

    /**
     * Redeploy this application. A lock will be taken for the duration of the deployment activation
     */
    protected final void deployNow(ApplicationId application, String reason) {
        try (MaintenanceDeployment deployment = new MaintenanceDeployment(application, deployer, metric, nodeRepository())) {
            if ( ! deployment.isValid()) return; // this will be done at another config server
            if ( ! canDeployNow(application)) return; // redeployment is no longer needed
            log.log(Level.INFO, () -> application + " will be redeployed" +
                                      (reason == null || reason.isBlank() ? "" : " due to " + reason) +
                                      ", last activated " + activationTime(application));
            deployment.activate();
        } finally {
            pendingDeployments.remove(application);
        }
    }

    /** Returns the last time application was activated. Epoch is returned if the application has never been activated. */
    protected final Instant activationTime(ApplicationId application) {
        return deployer.activationTime(application).orElse(Instant.EPOCH);
    }

    @Override
    public void shutdown() {
        super.shutdown();
        deploymentExecutor.shutdown();
    }

    @Override
    public void awaitShutdown() {
        Instant deadline = clock().instant().plus(Duration.ofMinutes(1));
        super.awaitShutdown();
        try {
            long remainder = Duration.between(clock().instant(), deadline).toMillis();
            if (deploymentExecutor.isShutdown()) return;

            // Give deployments in progress some time to complete
            if (remainder < 0 || !deploymentExecutor.awaitTermination(remainder, TimeUnit.MILLISECONDS)) {
                log.log(Level.WARNING, "Failed to shut down deployment executor within deadline");
                deploymentExecutor.shutdownNow();
            }
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }

}
