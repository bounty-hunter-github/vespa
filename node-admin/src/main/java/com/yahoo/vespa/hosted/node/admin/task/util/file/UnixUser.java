// Copyright Yahoo. Licensed under the terms of the Apache 2.0 license. See LICENSE in the project root.
package com.yahoo.vespa.hosted.node.admin.task.util.file;

/**
 * A regular UNIX-style user and its primary group.
 *
 * @author mpolden
 */
public record UnixUser(String name, int uid, String group, int gid) {
    public static final UnixUser ROOT = new UnixUser("root", 0, "root", 0);

    @Override
    public String toString() {
        return "user " + name + ":" + group;
    }
}
