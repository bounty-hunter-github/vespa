// Copyright Yahoo. Licensed under the terms of the Apache 2.0 license. See LICENSE in the project root.
package com.yahoo.vespa.hosted.node.admin.configserver.noderepository.bindings;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author freva
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
public class NodeRepositoryNode {

    @JsonProperty("state")
    public String state;
    @JsonProperty("hostname")
    public String hostname;
    @JsonProperty("ipAddresses")
    public Set<String> ipAddresses;
    @JsonProperty("additionalIpAddresses")
    public Set<String> additionalIpAddresses;
    @JsonProperty("id")
    public String id;
    @JsonProperty("cloudAccount")
    public String cloudAccount;
    @JsonProperty("flavor")
    public String flavor;
    @JsonProperty("resources")
    public NodeResources resources;
    @JsonProperty("realResources")
    public NodeResources realResources;
    @JsonProperty("membership")
    public Membership membership;
    @JsonProperty("owner")
    public Owner owner;
    @JsonProperty("restartGeneration")
    public Long restartGeneration;
    @JsonProperty("rebootGeneration")
    public Long rebootGeneration;
    @JsonProperty("currentRestartGeneration")
    public Long currentRestartGeneration;
    @JsonProperty("currentRebootGeneration")
    public Long currentRebootGeneration;
    @JsonProperty("vespaVersion")
    public String vespaVersion;
    @JsonProperty("wantedVespaVersion")
    public String wantedVespaVersion;
    @JsonProperty("currentOsVersion")
    public String currentOsVersion;
    @JsonProperty("wantedOsVersion")
    public String wantedOsVersion;
    @JsonProperty("currentFirmwareCheck")
    public Long currentFirmwareCheck;
    @JsonProperty("wantedFirmwareCheck")
    public Long wantedFirmwareCheck;
    @JsonProperty("modelName")
    public String modelName;
    @JsonProperty("failCount")
    public Integer failCount;
    @JsonProperty("environment")
    public String environment;
    @JsonProperty("reservedTo")
    public String reservedTo;
    @JsonProperty("type")
    public String type;
    @JsonProperty("wantedDockerImage")
    public String wantedDockerImage;
    @JsonProperty("currentDockerImage")
    public String currentDockerImage;
    @JsonProperty("parentHostname")
    public String parentHostname;
    @JsonProperty("wantToRetire")
    public Boolean wantToRetire;
    @JsonProperty("wantToDeprovision")
    public Boolean wantToDeprovision;
    @JsonProperty("wantToRebuild")
    public Boolean wantToRebuild;
    @JsonProperty("orchestratorStatus")
    public String orchestratorStatus;
    @JsonProperty("archiveUri")
    public String archiveUri;
    @JsonProperty("exclusiveTo")
    public String exclusiveTo;
    @JsonProperty("history")
    public List<Event> history;
    @JsonProperty("trustStore")
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    public List<TrustStoreItem> trustStore;
    @JsonProperty("wireguardPubkey")
    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    public String wireguardPubkey;

    @JsonProperty("reports")
    public Map<String, JsonNode> reports = null;

    @Override
    public String toString() {
        return "NodeRepositoryNode{" +
               "state='" + state + '\'' +
               ", hostname='" + hostname + '\'' +
               ", ipAddresses=" + ipAddresses +
               ", additionalIpAddresses=" + additionalIpAddresses +
               ", id='" + id + '\'' +
               ", flavor='" + flavor + '\'' +
               ", resources=" + resources +
               ", realResources=" + realResources +
               ", membership=" + membership +
               ", owner=" + owner +
               ", restartGeneration=" + restartGeneration +
               ", rebootGeneration=" + rebootGeneration +
               ", currentRestartGeneration=" + currentRestartGeneration +
               ", currentRebootGeneration=" + currentRebootGeneration +
               ", vespaVersion='" + vespaVersion + '\'' +
               ", wantedVespaVersion='" + wantedVespaVersion + '\'' +
               ", currentOsVersion='" + currentOsVersion + '\'' +
               ", wantedOsVersion='" + wantedOsVersion + '\'' +
               ", currentFirmwareCheck=" + currentFirmwareCheck +
               ", wantedFirmwareCheck=" + wantedFirmwareCheck +
               ", modelName='" + modelName + '\'' +
               ", failCount=" + failCount +
               ", environment='" + environment + '\'' +
               ", reservedTo='" + reservedTo + '\'' +
               ", type='" + type + '\'' +
               ", wantedDockerImage='" + wantedDockerImage + '\'' +
               ", currentDockerImage='" + currentDockerImage + '\'' +
               ", parentHostname='" + parentHostname + '\'' +
               ", wantToRetire=" + wantToRetire +
               ", wantToDeprovision=" + wantToDeprovision +
               ", wantToRebuild=" + wantToRebuild +
               ", orchestratorStatus='" + orchestratorStatus + '\'' +
               ", archiveUri='" + archiveUri + '\'' +
               ", exclusiveTo='" + exclusiveTo + '\'' +
               ", history=" + history +
               ", trustStore=" + trustStore +
               ", wireguardPubkey=" + wireguardPubkey +
               ", reports=" + reports +
               '}';
    }

    @JsonIgnoreProperties(ignoreUnknown = true)
    public record Owner(@JsonProperty("tenant") String tenant,
                        @JsonProperty("application") String application,
                        @JsonProperty("instance") String instance) { }

    @JsonIgnoreProperties(ignoreUnknown = true)
    public record Membership(@JsonProperty("clustertype") String clusterType,
                             @JsonProperty("clusterid") String clusterId,
                             @JsonProperty("group") String group,
                             @JsonProperty("index") int index,
                             @JsonProperty("retired") boolean retired) { }

    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record NodeResources(@JsonProperty("vcpu") Double vcpu,
                                @JsonProperty("memoryGb") Double memoryGb,
                                @JsonProperty("diskGb") Double diskGb,
                                @JsonProperty("bandwidthGbps") Double bandwidthGbps,
                                @JsonProperty("diskSpeed") String diskSpeed,
                                @JsonProperty("storageType") String storageType,
                                @JsonProperty("architecture") String architecture,
                                @JsonProperty("gpuCount") Integer gpuCount,
                                @JsonProperty("gpuMemoryGb") Double gpuMemoryGb) { }

    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record Event(@JsonProperty("event") String event,
                        @JsonProperty("agent") String agent,
                        @JsonProperty("at") Long at) { }

    @JsonIgnoreProperties(ignoreUnknown = true)
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record TrustStoreItem(@JsonProperty("fingerprint") String fingerprint,
                                 @JsonProperty("expiry") long expiry) { }
}
