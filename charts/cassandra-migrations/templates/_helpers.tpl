{{- define "cassandraGalleyHost" -}}
{{ $cassandraGalley := default dict .Values.cassandraGalley }}
{{- default (.Values.cassandra.host) $cassandraGalley.host }}
{{- end -}}

{{- define "cassandraBrigHost" -}}
{{ $cassandraBrig := default dict .Values.cassandraBrig }}
{{- default (.Values.cassandra.host) $cassandraBrig.host }}
{{- end -}}

{{- define "cassandraGundeckHost" -}}
{{ $cassandraGundeck := default dict .Values.cassandraGundeck }}
{{- default (.Values.cassandra.host) $cassandraGundeck.host }}
{{- end -}}

{{- define "cassandraSparHost" -}}
{{ $cassandraSpar := default dict .Values.cassandraSpar }}
{{- default (.Values.cassandra.host) $cassandraSpar.host }}
{{- end -}}

{{/*
Note: in the past, 'replicaCount' was used, this fallback is only used
for backwards-compatibility with already-installed charts to not break existing installations.

Thus the order of priority is:

1. cassandraGalley.replicationMap
2. cassandra.replicationMap
3. cassandraGalley.replicationFactor
4. cassandra.replicationFactor
5. cassandra.replicaCount

*/}}

{{- define "cassandraGalleyReplicationArg" -}}
{{ $cassandraGalley := default dict .Values.cassandraGalley }}
{{- if (or .Values.cassandra.replicationMap $cassandraGalley.replicationMap) -}}
{{- default (.Values.cassandra.replicationMap) $cassandraGalley.replicationMap -}}
{{- else -}}
{{- default (default (.Values.cassandra.replicaCount) .Values.cassandra.replicationFactor) $cassandraGalley.replicationFactor -}}
{{- end -}}
{{- end -}}

{{- define "cassandraGalleyReplicationType" -}}
{{ $cassandraGalley := default dict .Values.cassandraGalley }}
{{- if (or .Values.cassandra.replicationMap $cassandraGalley.replicationMap) -}}
{{- printf "--replication-map" -}}
{{- else -}}
{{- printf "--replication-factor" -}}
{{- end -}}
{{- end -}}


{{- define "cassandraGundeckReplicationArg" -}}
{{ $cassandraGundeck := default dict .Values.cassandraGundeck }}
{{- if (or .Values.cassandra.replicationMap $cassandraGundeck.replicationMap) -}}
{{- default (.Values.cassandra.replicationMap) $cassandraGundeck.replicationMap -}}
{{- else -}}
{{- default (default (.Values.cassandra.replicaCount) .Values.cassandra.replicationFactor) $cassandraGundeck.replicationFactor -}}
{{- end -}}
{{- end -}}

{{- define "cassandraGundeckReplicationType" -}}
{{ $cassandraGundeck := default dict .Values.cassandraGundeck }}
{{- if (or .Values.cassandra.replicationMap $cassandraGundeck.replicationMap) -}}
{{- printf "--replication-map" -}}
{{- else -}}
{{- printf "--replication-factor" -}}
{{- end -}}
{{- end -}}


{{- define "cassandraBrigReplicationArg" -}}
{{ $cassandraBrig := default dict .Values.cassandraBrig }}
{{- if (or .Values.cassandra.replicationMap $cassandraBrig.replicationMap) -}}
{{- default (.Values.cassandra.replicationMap) $cassandraBrig.replicationMap -}}
{{- else -}}
{{- default (default (.Values.cassandra.replicaCount) .Values.cassandra.replicationFactor) $cassandraBrig.replicationFactor -}}
{{- end -}}
{{- end -}}

{{- define "cassandraBrigReplicationType" -}}
{{ $cassandraBrig := default dict .Values.cassandraBrig }}
{{- if (or .Values.cassandra.replicationMap $cassandraBrig.replicationMap) -}}
{{- printf "--replication-map" -}}
{{- else -}}
{{- printf "--replication-factor" -}}
{{- end -}}
{{- end -}}


{{- define "cassandraSparReplicationArg" -}}
{{ $cassandraSpar := default dict .Values.cassandraSpar }}
{{- if (or .Values.cassandra.replicationMap $cassandraSpar.replicationMap) -}}
{{- default (.Values.cassandra.replicationMap) $cassandraSpar.replicationMap -}}
{{- else -}}
{{- default (default (.Values.cassandra.replicaCount) .Values.cassandra.replicationFactor) $cassandraSpar.replicationFactor -}}
{{- end -}}
{{- end -}}

{{- define "cassandraSparReplicationType" -}}
{{ $cassandraSpar := default dict .Values.cassandraSpar }}
{{- if (or .Values.cassandra.replicationMap $cassandraSpar.replicationMap) -}}
{{- printf "--replication-map" -}}
{{- else -}}
{{- printf "--replication-factor" -}}
{{- end -}}
{{- end -}}

{{- define "useTlsCertGalley" -}}
{{ $cassandraGalley := default dict .Values.cassandraGalley }}
{{- or .Values.cassandra.tlsCert $cassandraGalley.tlsCert -}}
{{- end -}}

{{- define "tlsCertGalley" -}}
{{ $cassandraGalley := default dict .Values.cassandraGalley }}
{{- if .Values.cassandra.tlsCert -}}
{{ .Values.cassandra.tlsCert }}
{{- else -}}
{{ $cassandraGalley.tlsCert }}
{{- end -}}
{{- end -}}

{{- define "useTlsCertBrig" -}}
{{ $cassandraBrig := default dict .Values.cassandraBrig }}
{{- or .Values.cassandra.tlsCert $cassandraBrig.tlsCert -}}
{{- end -}}

{{- define "tlsCertBrig" -}}
{{ $cassandraBrig := default dict .Values.cassandraBrig }}
{{- if .Values.cassandra.tlsCert -}}
{{ .Values.cassandra.tlsCert }}
{{- else -}}
{{ $cassandraBrig.tlsCert }}
{{- end -}}
{{- end -}}

{{- define "useTlsCertSpar" -}}
{{ $cassandraSpar := default dict .Values.cassandraSpar }}
{{- or .Values.cassandra.tlsCert $cassandraSpar.tlsCert -}}
{{- end -}}

{{- define "tlsCertSpar" -}}
{{ $cassandraSpar := default dict .Values.cassandraSpar }}
{{- if .Values.cassandra.tlsCert -}}
{{ .Values.cassandra.tlsCert }}
{{- else -}}
{{ $cassandraSpar.tlsCert }}
{{- end -}}
{{- end -}}

{{- define "useTlsCertGundeck" -}}
{{ $cassandraGundeck := default dict .Values.cassandraGundeck }}
{{- or .Values.cassandra.tlsCert $cassandraGundeck.tlsCert -}}
{{- end -}}

{{- define "tlsCertGundeck" -}}
{{ $cassandraGundeck := default dict .Values.cassandraGundeck }}
{{- if .Values.cassandra.tlsCert -}}
{{ .Values.cassandra.tlsCert }}
{{- else -}}
{{ $cassandraGundeck.tlsCert }}
{{- end -}}
{{- end -}}

{{/* Allow KubeVersion to be overridden. */}}
{{- define "kubeVersion" -}}
  {{- default .Capabilities.KubeVersion.Version .Values.kubeVersionOverride -}}
{{- end -}}

{{- define "includeSecurityContext" -}}
  {{- (semverCompare ">= 1.24-0" (include "kubeVersion" .)) -}}
{{- end -}}
