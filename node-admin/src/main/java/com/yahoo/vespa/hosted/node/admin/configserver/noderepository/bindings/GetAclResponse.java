// Copyright Yahoo. Licensed under the terms of the Apache 2.0 license. See LICENSE in the project root.
package com.yahoo.vespa.hosted.node.admin.configserver.noderepository.bindings;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;

/**
 * This class represents a response from the /nodes/v2/acl/ API.
 *
 * @author mpolden
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public record GetAclResponse(List<Node> trustedNodes, List<Network> trustedNetworks, List<Port> trustedPorts, List<Port> trustedUdpPorts) {

    @JsonCreator
    public GetAclResponse(@JsonProperty("trustedNodes") List<Node> trustedNodes,
                          @JsonProperty("trustedNetworks") List<Network> trustedNetworks,
                          @JsonProperty("trustedPorts") List<Port> trustedPorts,
                          @JsonProperty("trustedUdpPorts") List<Port> trustedUdpPorts) {
        this.trustedNodes = trustedNodes == null ? List.of() : List.copyOf(trustedNodes);
        this.trustedNetworks = trustedNetworks == null ? List.of() : List.copyOf(trustedNetworks);
        this.trustedPorts = trustedPorts == null ? List.of() : List.copyOf(trustedPorts);
        this.trustedUdpPorts = trustedUdpPorts == null ? List.of() : List.copyOf(trustedUdpPorts);
    }

    @JsonIgnoreProperties(ignoreUnknown = true)
    public record Node(String hostname, String nodeType, String ipAddress, List<Integer> ports, String trustedBy) {

        @JsonCreator
        public Node(@JsonProperty("hostname") String hostname, @JsonProperty("type") String nodeType,
                    @JsonProperty("ipAddress") String ipAddress, @JsonProperty("ports") List<Integer> ports,
                    @JsonProperty("trustedBy") String trustedBy) {
            this.hostname = hostname;
            this.nodeType = nodeType;
            this.ipAddress = ipAddress;
            this.ports = ports == null ? List.of() : List.copyOf(ports);
            this.trustedBy = trustedBy;
        }
    }

    @JsonIgnoreProperties(ignoreUnknown = true)
    public record Network(@JsonProperty("network") String network, @JsonProperty("trustedBy") String trustedBy) { }

    @JsonIgnoreProperties(ignoreUnknown = true)
    public record Port(@JsonProperty("port") Integer port, @JsonProperty("trustedBy") String trustedBy) { }
}
