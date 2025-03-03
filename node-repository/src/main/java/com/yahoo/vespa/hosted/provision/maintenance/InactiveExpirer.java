// Copyright Yahoo. Licensed under the terms of the Apache 2.0 license. See LICENSE in the project root.
package com.yahoo.vespa.hosted.provision.maintenance;

import com.yahoo.jdisc.Metric;
import com.yahoo.vespa.hosted.provision.Node;
import com.yahoo.vespa.hosted.provision.Node.State;
import com.yahoo.vespa.hosted.provision.NodeList;
import com.yahoo.vespa.hosted.provision.NodeRepository;
import com.yahoo.vespa.hosted.provision.node.Agent;
import com.yahoo.vespa.hosted.provision.node.History;
import com.yahoo.vespa.hosted.provision.node.Status;

import java.time.Duration;
import java.util.List;

/**
 * Maintenance job which moves inactive nodes to dirty or parked after timeout.
 *
 * The timeout is in place to provide a grace period in which nodes can be brought back to active
 * if they were deactivated in error. As inactive nodes retain their state
 * they can be brought back to active and correct state faster than a new node.
 *
 * Nodes with following flags set are not reusable and will be moved to parked
 * instead of dirty:
 *
 * - {@link Status#wantToRetire()} (when set by an operator)
 * - {@link Status#wantToDeprovision()}
 *
 * @author bratseth
 * @author mpolden
 */
public class InactiveExpirer extends Expirer {

    private final NodeRepository nodeRepository;
    private final Duration timeout;

    InactiveExpirer(NodeRepository nodeRepository, Duration timeout, Metric metric) {
        super(Node.State.inactive, History.Event.Type.deactivated, nodeRepository, timeout, metric);
        this.nodeRepository = nodeRepository;
        this.timeout = timeout;
    }

    @Override
    protected void expire(List<Node> expired) {
        nodeRepository.nodes().performOn(NodeList.copyOf(expired),
                                         node -> node.state() == State.inactive && isExpired(node),
                                         (node, lock) -> nodeRepository.nodes().deallocate(node, Agent.InactiveExpirer, "Expired by InactiveExpirer"));
    }

    @Override
    protected boolean isExpired(Node node) {
        return super.isExpired(node, timeout(node)) ||
               node.allocation().get().owner().instance().isTester();
    }

    private Duration timeout(Node node) {
        return timeout;
    }

}
