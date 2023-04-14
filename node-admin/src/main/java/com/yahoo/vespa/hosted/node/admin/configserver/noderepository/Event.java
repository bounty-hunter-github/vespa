// Copyright Yahoo. Licensed under the terms of the Apache 2.0 license. See LICENSE in the project root.
package com.yahoo.vespa.hosted.node.admin.configserver.noderepository;

import java.time.Instant;
import java.util.Objects;

/**
 * @author freva
 */
public record Event(String agent, String type, Instant at) {
    public Event {
        Objects.requireNonNull(agent);
        Objects.requireNonNull(type);
        Objects.requireNonNull(at);
    }
}
