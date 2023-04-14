// Copyright Yahoo. Licensed under the terms of the Apache 2.0 license. See LICENSE in the project root.
//
package com.yahoo.vespa.hosted.node.admin.container;

import ai.vespa.validation.PatternedStringWrapper;

import java.util.regex.Pattern;

/**
 * The ID of a container.
 *
 * @author hakon
 */
public class ContainerId extends PatternedStringWrapper<ContainerId> {
    private static final Pattern CONTAINER_ID_PATTERN = Pattern.compile("^[0-9a-f]+$");

    public ContainerId(String id) {
        super(id, CONTAINER_ID_PATTERN, "container id");
    }

}
