{{- define "cassandraGalleyHost" -}}
{{- default (.Values.cassandra.host) .Values.cassandraGalley.host }}
{{- end -}}

{{- define "cassandraBrigHost" -}}
{{- default (.Values.cassandra.host) .Values.cassandraBrig.host }}
{{- end -}}

{{- define "cassandraGundeckHost" -}}
{{- default (.Values.cassandra.host) .Values.cassandraGundeck.host }}
{{- end -}}

{{- define "cassandraSparHost" -}}
{{- default (.Values.cassandra.host) .Values.cassandraSpar.host }}
{{- end -}}


{{- define "cassandraGalleyReplicationArg" -}}
{{- if
{{- printf "--%s %s" "replication-map" .Chart.Version }}
{{- printf "--%s %s" "replication-factor" .Chart.Version }}
{{- default (.Values.cassandra.replicationMap) .Values.cassandraGalley.replicationMap }}
{{- end -}}


{{/*
In the past, 'replicaCount' was used, this fallback is only used
for backwards-compatibility with already-installed charts.
*/}}
{{- define "replicationFactor" -}}
{{- default (.Values.cassandra.replicaCount) .Values.cassandra.replicationFactor }}
{{- end -}}

