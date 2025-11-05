
{{/* Allow KubeVersion to be overridden. */}}
{{- define "kubeVersion" -}}
  {{- default .Capabilities.KubeVersion.Version .Values.kubeVersionOverride -}}
{{- end -}}

{{- define "includeSecurityContext" -}}
  {{- (semverCompare ">= 1.24-0" (include "kubeVersion" .)) -}}
{{- end -}}

{{- define "useGundeckCassandraTLS" -}}
{{ or (hasKey .cassandra "tlsCa") (hasKey .cassandra "tlsCaSecretRef") }}
{{- end -}}

{{- define "useBrigCassandraTLS" -}}
{{ or (hasKey .cassandraBrig "tlsCa") (hasKey .cassandraBrig "tlsCaSecretRef") }}
{{- end -}}

{{- define "useGalleyCassandraTLS" -}}
{{ or (hasKey .cassandraGalley "tlsCa") (hasKey .cassandraGalley "tlsCaSecretRef") }}
{{- end -}}

{{- define "gundeckTlsSecretRef" -}}
{{- if .cassandra.tlsCaSecretRef -}}
{{ .cassandra.tlsCaSecretRef | toYaml }}
{{- else }}
{{- dict "name" "background-worker-cassandra-gundeck" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}

{{- define "brigTlsSecretRef" -}}
{{- if .cassandraBrig.tlsCaSecretRef -}}
{{ .cassandraBrig.tlsCaSecretRef | toYaml }}
{{- else }}
{{- dict "name" "background-worker-cassandra-brig" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}

{{/* Return a Dict of TLS CA secret name and key
This is used to switch between provided secret (e.g. by cert-manager) and
created one (in case the CA is provided as PEM string.)
*/}}
{{- define "galleyTlsSecretRef" -}}
{{- if and .cassandraGalley .cassandraGalley.tlsCaSecretRef -}}
{{ .cassandraGalley.tlsCaSecretRef | toYaml }}
{{- else }}
{{- dict "name" "background-worker-cassandra-galley" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}

