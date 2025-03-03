// Copyright Yahoo. Licensed under the terms of the Apache 2.0 license. See LICENSE in the project root.
package com.yahoo.vespa.hosted.provision;

import com.yahoo.transaction.Mutex;

import java.util.List;
import java.util.Objects;

/**
 * A type-safe wrapper for {@link NodeList}. Callers that have a reference to this can safely be assumed to be holding
 * the allocation lock for the node repository.
 *
 * This is typically used in situations where modifying a node object depends on inspecting a consistent state of all
 * nodes in the repository.
 *
 * @author mpolden
 */
public final class LockedNodeList extends NodeList {

    private final Mutex lock;

    public LockedNodeList(List<Node> nodes, Mutex lock) {
        super(nodes, false);
        this.lock = Objects.requireNonNull(lock, "lock must be non-null");
    }

    /** Returns a new LockedNodeList with the for the same lock. */
    public LockedNodeList childList(List<Node> nodes) {
        return new LockedNodeList(nodes, lock);
    }

}
