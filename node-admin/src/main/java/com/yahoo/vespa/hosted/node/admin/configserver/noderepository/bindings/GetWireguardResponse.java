package com.yahoo.vespa.hosted.node.admin.configserver.noderepository.bindings;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;

/**
 * A response from the /nodes/v2/wireguard api.
 *
 * @author gjoranv
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
public record GetWireguardResponse(@JsonProperty("configservers") List<Configserver> configservers) {

    @JsonIgnoreProperties(ignoreUnknown = true)
        public record Configserver(@JsonProperty("hostname") String hostname,
                                   @JsonProperty("ipAddresses") List<String> ipAddresses,
                                   @JsonProperty("wireguardPubkey") String wireguardPubkey) { }
}
