// Copyright Yahoo. Licensed under the terms of the Apache 2.0 license. See LICENSE in the project root.
package com.yahoo.vespa.hosted.node.admin.configserver;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.google.common.base.Strings;

/**
 * @author hakonhall
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
public record StandardConfigServerResponse(@JsonProperty("message") String message, @JsonProperty("error-code") String errorCode) {
    public void throwOnError(String detail) {
        if (!Strings.isNullOrEmpty(errorCode))
            throw new ConfigServerException(detail + ": " + message + " " + errorCode);
    }
}
