
{{/* Allow KubeVersion to be overridden. */}}
{{- define "kubeVersion" -}}
  {{- default .Capabilities.KubeVersion.Version .Values.kubeVersionOverride -}}
{{- end -}}

{{- define "includeSecurityContext" -}}
  {{- (semverCompare ">= 1.24-0" (include "kubeVersion" .)) -}}
{{- end -}}

{{- define "useCassandraCA" -}}
{{ or (hasKey .cassandra "tlsCa") (hasKey .cassandra "tlsCaSecretRef") }}
{{- end -}}

{{- define "tlsSecretRef" -}}
{{- if .cassandra.tlsCaSecretRef -}}
{{ .cassandra.tlsCaSecretRef | toYaml }}
{{- else }}
{{- dict "name" "brig-cassandra-cert" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}
