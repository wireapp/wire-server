{{- define "cassandraGalleyHost" -}}
{{ $cassandraGalley := default dict .Values.cassandraGalley }}
{{- $cassandra := default dict .Values.cassandra }}
{{- default ($cassandra.host) $cassandraGalley.host }}
{{- end -}}

{{- define "cassandraBrigHost" -}}
{{ $cassandraBrig := default dict .Values.cassandraBrig }}
{{- $cassandra := default dict .Values.cassandra }}
{{- default ($cassandra.host) $cassandraBrig.host }}
{{- end -}}

{{- define "cassandraGundeckHost" -}}
{{ $cassandraGundeck := default dict .Values.cassandraGundeck }}
{{- $cassandra := default dict .Values.cassandra }}
{{- default ($cassandra.host) $cassandraGundeck.host }}
{{- end -}}

{{- define "cassandraSparHost" -}}
{{ $cassandraSpar := default dict .Values.cassandraSpar }}
{{- $cassandra := default dict .Values.cassandra }}
{{- default ($cassandra.host) $cassandraSpar.host }}
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
{{- $cassandra := default dict .Values.cassandra }}
{{- if (or $cassandra.replicationMap $cassandraGalley.replicationMap) -}}
{{- default ($cassandra.replicationMap) $cassandraGalley.replicationMap -}}
{{- else -}}
{{- default (default ($cassandra.replicaCount) $cassandra.replicationFactor) $cassandraGalley.replicationFactor -}}
{{- end -}}
{{- end -}}

{{- define "cassandraGalleyReplicationType" -}}
{{ $cassandraGalley := default dict .Values.cassandraGalley }}
{{- $cassandra := default dict .Values.cassandra }}
{{- if (or $cassandra.replicationMap $cassandraGalley.replicationMap) -}}
{{- printf "--replication-map" -}}
{{- else -}}
{{- printf "--replication-factor" -}}
{{- end -}}
{{- end -}}


{{- define "cassandraGundeckReplicationArg" -}}
{{ $cassandraGundeck := default dict .Values.cassandraGundeck }}
{{- $cassandra := default dict .Values.cassandra }}
{{- if (or $cassandra.replicationMap $cassandraGundeck.replicationMap) -}}
{{- default ($cassandra.replicationMap) $cassandraGundeck.replicationMap -}}
{{- else -}}
{{- default (default ($cassandra.replicaCount) $cassandra.replicationFactor) $cassandraGundeck.replicationFactor -}}
{{- end -}}
{{- end -}}

{{- define "cassandraGundeckReplicationType" -}}
{{ $cassandraGundeck := default dict .Values.cassandraGundeck }}
{{- $cassandra := default dict .Values.cassandra }}
{{- if (or $cassandra.replicationMap $cassandraGundeck.replicationMap) -}}
{{- printf "--replication-map" -}}
{{- else -}}
{{- printf "--replication-factor" -}}
{{- end -}}
{{- end -}}


{{- define "cassandraBrigReplicationArg" -}}
{{ $cassandraBrig := default dict .Values.cassandraBrig }}
{{- $cassandra := default dict .Values.cassandra }}
{{- if (or $cassandra.replicationMap $cassandraBrig.replicationMap) -}}
{{- default ($cassandra.replicationMap) $cassandraBrig.replicationMap -}}
{{- else -}}
{{- default (default ($cassandra.replicaCount) $cassandra.replicationFactor) $cassandraBrig.replicationFactor -}}
{{- end -}}
{{- end -}}

{{- define "cassandraBrigReplicationType" -}}
{{ $cassandraBrig := default dict .Values.cassandraBrig }}
{{- $cassandra := default dict .Values.cassandra }}
{{- if (or $cassandra.replicationMap $cassandraBrig.replicationMap) -}}
{{- printf "--replication-map" -}}
{{- else -}}
{{- printf "--replication-factor" -}}
{{- end -}}
{{- end -}}


{{- define "cassandraSparReplicationArg" -}}
{{ $cassandraSpar := default dict .Values.cassandraSpar }}
{{- $cassandra := default dict .Values.cassandra }}
{{- if (or $cassandra.replicationMap $cassandraSpar.replicationMap) -}}
{{- default ($cassandra.replicationMap) $cassandraSpar.replicationMap -}}
{{- else -}}
{{- default (default ($cassandra.replicaCount) $cassandra.replicationFactor) $cassandraSpar.replicationFactor -}}
{{- end -}}
{{- end -}}

{{- define "cassandraSparReplicationType" -}}
{{ $cassandraSpar := default dict .Values.cassandraSpar }}
{{- $cassandra := default dict .Values.cassandra }}
{{- if (or $cassandra.replicationMap $cassandraSpar.replicationMap) -}}
{{- printf "--replication-map" -}}
{{- else -}}
{{- printf "--replication-factor" -}}
{{- end -}}
{{- end -}}

{{/* NOTE: Cassandra TLS helpers

Cassandra connections can be configured per service or with a general configuration.
Thus, there are three functions per service that fallback to the general
configuration if the specific one does not exist:

- useTls<service-name> -> Bool: Do we use Cassandra TLS connections for this
  service?

- tlsCa<service-name> -> String: TLS CA PEM string (if configured)

- tlsSecretRef<service-name> -> YAML: Dict with keys `name` (name of the
  secret to use) and `key` (name of the entry in the secret)
*/}}

{{- define "useTlsGalley" -}}
{{ $cassandra := default dict .Values.cassandra }}
{{ $cassandraGalley := default $cassandra .Values.cassandraGalley }}
{{- if or $cassandraGalley.tlsCa $cassandraGalley.tlsCaSecretRef -}}
true
{{- else}}
false
{{- end }}
{{- end -}}

{{- define "tlsCaGalley" -}}
{{ $cassandra := default dict .Values.cassandra }}
{{ $cassandraGalley := default $cassandra .Values.cassandraGalley }}
{{- if hasKey $cassandraGalley "tlsCa" -}}
{{- $cassandraGalley.tlsCa }}
{{ else }}
{{- end -}}
{{- end -}}

{{- define "tlsSecretRefGalley" -}}
{{ $cassandra := default dict .Values.cassandra }}
{{ $cassandraGalley := default $cassandra .Values.cassandraGalley }}
{{- if $cassandraGalley.tlsCaSecretRef -}}
{{ $cassandraGalley.tlsCaSecretRef | toYaml }}
{{- else }}
{{- dict "name" "galley-cassandra-cert" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}

{{- define "useTlsBrig" -}}
{{ $cassandra := default dict .Values.cassandra }}
{{ $cassandraBrig := default $cassandra .Values.cassandraBrig }}
{{- if or $cassandraBrig.tlsCa $cassandraBrig.tlsCaSecretRef -}}
true
{{- else}}
false
{{- end }}
{{- end -}}

{{- define "tlsCaBrig" -}}
{{ $cassandra := default dict .Values.cassandra }}
{{ $cassandraBrig := default $cassandra .Values.cassandraBrig }}
{{- if hasKey $cassandraBrig "tlsCa" -}}
{{- $cassandraBrig.tlsCa }}
{{ else }}
{{- end -}}
{{- end -}}

{{- define "tlsSecretRefBrig" -}}
{{ $cassandra := default dict .Values.cassandra }}
{{ $cassandraBrig := default $cassandra .Values.cassandraBrig }}
{{- if $cassandraBrig.tlsCaSecretRef -}}
{{ $cassandraBrig.tlsCaSecretRef | toYaml }}
{{- else }}
{{- dict "name" "brig-cassandra-cert" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}

{{- define "useTlsSpar" -}}
{{ $cassandra := default dict .Values.cassandra }}
{{ $cassandraSpar := default $cassandra .Values.cassandraSpar }}
{{- if or $cassandraSpar.tlsCa $cassandraSpar.tlsCaSecretRef -}}
true
{{- else}}
false
{{- end }}
{{- end -}}

{{- define "tlsCaSpar" -}}
{{ $cassandra := default dict .Values.cassandra }}
{{ $cassandraSpar := default $cassandra .Values.cassandraSpar }}
{{- if hasKey $cassandraSpar "tlsCa" -}}
{{- $cassandraSpar.tlsCa }}
{{ else }}
{{- end -}}
{{- end -}}

{{- define "tlsSecretRefSpar" -}}
{{ $cassandra := default dict .Values.cassandra }}
{{ $cassandraSpar := default $cassandra .Values.cassandraSpar }}
{{- if $cassandraSpar.tlsCaSecretRef -}}
{{ $cassandraSpar.tlsCaSecretRef | toYaml }}
{{- else }}
{{- dict "name" "spar-cassandra-cert" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}

{{- define "useTlsGundeck" -}}
{{ $cassandra := default dict .Values.cassandra }}
{{ $cassandraGundeck := default $cassandra .Values.cassandraGundeck }}
{{- if or $cassandraGundeck.tlsCa $cassandraGundeck.tlsCaSecretRef -}}
true
{{- else}}
false
{{- end }}
{{- end -}}

{{- define "tlsCaGundeck" -}}
{{ $cassandra := default dict .Values.cassandra }}
{{ $cassandraGundeck := default $cassandra .Values.cassandraGundeck }}
{{- if hasKey $cassandraGundeck "tlsCa" -}}
{{- $cassandraGundeck.tlsCa }}
{{ else }}
{{- end -}}
{{- end -}}

{{- define "tlsSecretRefGundeck" -}}
{{ $cassandra := default dict .Values.cassandra }}
{{ $cassandraGundeck := default $cassandra .Values.cassandraGundeck }}
{{- if $cassandraGundeck.tlsCaSecretRef -}}
{{ $cassandraGundeck.tlsCaSecretRef | toYaml }}
{{- else }}
{{- dict "name" "gundeck-cassandra-cert" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}

{{/* Allow KubeVersion to be overridden. */}}
{{- define "kubeVersion" -}}
  {{- default .Capabilities.KubeVersion.Version .Values.kubeVersionOverride -}}
{{- end -}}

{{- define "includeSecurityContext" -}}
  {{- (semverCompare ">= 1.24-0" (include "kubeVersion" .)) -}}
{{- end -}}
