// Copyright Yahoo. Licensed under the terms of the Apache 2.0 license. See LICENSE in the project root.
package com.yahoo.vespa.hosted.node.admin.configserver.state.bindings;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Response from /state/v1/health
 *
 * @author hakon
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public record HealthResponse(@JsonProperty("status") Status status) {

    @JsonIgnoreProperties(ignoreUnknown = true)
    public record Status(@JsonProperty("code") String code) { }
}
