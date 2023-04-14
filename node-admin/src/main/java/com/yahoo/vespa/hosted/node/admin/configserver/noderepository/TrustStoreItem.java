// Copyright Yahoo. Licensed under the terms of the Apache 2.0 license. See LICENSE in the project root.

package com.yahoo.vespa.hosted.node.admin.configserver.noderepository;

import java.time.Instant;

/**
 * @author mortent
 */
public record TrustStoreItem(String fingerprint, Instant expiry) { }
