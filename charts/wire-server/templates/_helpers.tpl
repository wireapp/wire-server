
{{/* Allow KubeVersion to be overridden. */}}
{{- define "galley.kubeVersion" -}}
  {{- default .Capabilities.KubeVersion.Version .Values.kubeVersionOverride -}}
{{- end -}}

{{- define "galley.includeSecurityContext" -}}
  {{- (semverCompare ">= 1.24-0" (include "galley.kubeVersion" .)) -}}
{{- end -}}

{{- define "galley.useCassandraTLS" -}}
{{ or (hasKey .cassandra "tlsCa") (hasKey .cassandra "tlsCaSecretRef") }}
{{- end -}}

{{/* Return a Dict of TLS CA secret name and key
This is used to switch between provided secret (e.g. by cert-manager) and
created one (in case the CA is provided as PEM string.)
*/}}
{{- define "galley.tlsSecretRef" -}}
{{- if .cassandra.tlsCaSecretRef -}}
{{ .cassandra.tlsCaSecretRef | toYaml }}
{{- else }}
{{- dict "name" "galley-cassandra" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}
