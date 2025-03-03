// Copyright Yahoo. Licensed under the terms of the Apache 2.0 license. See LICENSE in the project root.
package com.yahoo.vespa.model.application.validation;

import com.yahoo.config.application.api.DeployLogger;
import com.yahoo.config.model.deploy.DeployState;
import com.yahoo.schema.Schema;
import com.yahoo.schema.derived.SchemaInfo;
import com.yahoo.schema.document.ComplexAttributeFieldUtils;
import com.yahoo.schema.document.GeoPos;
import com.yahoo.schema.document.ImmutableSDField;
import com.yahoo.vespa.model.VespaModel;
import com.yahoo.vespa.model.search.SearchCluster;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.logging.Level;
import java.util.stream.Collectors;

/**
 * Validates that complex fields (of type struct or map) that have struct field attributes are supported.
 *
 * Only applies for indexed search clusters.
 *
 * @author geirst
 */
public class ComplexFieldsWithStructFieldAttributesValidator extends Validator {

    @Override
    public void validate(VespaModel model, DeployState deployState) {
        List<SearchCluster> searchClusters = model.getSearchClusters();
        for (SearchCluster cluster : searchClusters) {
            if (cluster.isStreaming()) continue;

            for (SchemaInfo spec : cluster.schemas().values()) {
                validateComplexFields(cluster.getClusterName(), spec.fullSchema(), deployState.getDeployLogger());
            }
        }
    }

    private static void validateComplexFields(String clusterName, Schema schema, DeployLogger logger) {
        String unsupportedFields = validateComplexFields(clusterName, schema, false);
        if (!unsupportedFields.isEmpty()) {
            throw new IllegalArgumentException(getErrorMessage(clusterName, schema, unsupportedFields));
        }
        unsupportedFields = validateComplexFields(clusterName, schema, true);
        if (!unsupportedFields.isEmpty()) {
            logger.logApplicationPackage(Level.WARNING, getErrorMessage(clusterName, schema, unsupportedFields));
        }
    }

    private static String validateComplexFields(String clusterName, Schema schema, boolean stricterValidation) {
        return schema.allFields()
                .filter(field -> isUnsupportedComplexField(field, stricterValidation))
                .map(ComplexFieldsWithStructFieldAttributesValidator::toString)
                .collect(Collectors.joining(", "));
    }

    private static String getErrorMessage(String clusterName, Schema schema, String unsupportedFields) {
        return String.format("For cluster '%s', search '%s': The following complex fields do not support using struct field attributes: %s. " +
                             "Only supported for the following complex field types: array or map of struct with primitive types, map of primitive types. " +
                             "The supported primitive types are: byte, int, long, float, double and string",
                             clusterName, schema.getName(), unsupportedFields);
    }

    private static boolean isUnsupportedComplexField(ImmutableSDField field, boolean stricterValidation) {
        return (field.usesStructOrMap() &&
                !isSupportedComplexField(field, stricterValidation) &&
                hasStructFieldAttributes(field.getStructFields()));
    }

    private static boolean isSupportedComplexField(ImmutableSDField field, boolean stricterValidation) {
        return (ComplexAttributeFieldUtils.isSupportedComplexField(field, stricterValidation) ||
                GeoPos.isAnyPos(field));
    }

    private static String toString(ImmutableSDField field) {
        return field.getName() + " (" + String.join(", ", getStructFieldAttributes(field.getStructFields(), false)) + ")";
    }

    private static boolean hasStructFieldAttributes(Collection<? extends ImmutableSDField> structFields) {
        return !getStructFieldAttributes(structFields, true).isEmpty();
    }

    private static List<String> getStructFieldAttributes(Collection<? extends ImmutableSDField> structFields,
                                                         boolean returnAllTypes) {
        var result = new ArrayList<String>();
        for (var structField : structFields) {
            for (var attr : structField.getAttributes().values()) {
                if (returnAllTypes || !ComplexAttributeFieldUtils.isPrimitiveType(attr)) {
                    result.add(attr.getName());
                }
            }
            if (structField.usesStructOrMap() && structField.wasConfiguredToDoAttributing()) {
                result.add(structField.getName());
            }
            // If we encounter struct field attributes underneath this level, those are not supported and should be returned.
            result.addAll(getStructFieldAttributes(structField.getStructFields(), true));
        }
        return result;
    }
}
