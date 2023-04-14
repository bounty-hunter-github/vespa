// Copyright Yahoo. Licensed under the terms of the Apache 2.0 license. See LICENSE in the project root.
package com.yahoo.vespa.hosted.node.admin.container;

import ai.vespa.validation.PatternedStringWrapper;

import java.util.regex.Pattern;

/**
 * Type-safe value wrapper for docker container names.
 *
 * @author bakksjo
 */
public class ContainerName extends PatternedStringWrapper<ContainerName> {

    private static final Pattern CONTAINER_NAME_PATTERN = Pattern.compile("^[a-zA-Z0-9_-]+$");

    public ContainerName(String name) {
        super(name, CONTAINER_NAME_PATTERN, "container name");
    }

    public static ContainerName fromHostname(String hostName) {
        return new ContainerName(hostName.split("\\.", 2)[0]);
    }

}
