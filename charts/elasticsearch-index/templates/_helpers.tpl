
{{/* Allow KubeVersion to be overridden. */}}
{{- define "kubeVersion" -}}
  {{- default .Capabilities.KubeVersion.Version .Values.kubeVersionOverride -}}
{{- end -}}

{{- define "includeSecurityContext" -}}
  {{- (semverCompare ">= 1.24-0" (include "kubeVersion" .)) -}}
{{- end -}}

{{/*
NOTE: The following helpers are DEPRECATED.
This chart now uses brig's ConfigMap and Secret for configuration.
TLS settings should be configured via:
  - elasticsearchTlsEnabled: true/false
  - cassandraTlsEnabled: true/false
  - secrets.elasticsearchCaSecretName: name of the secret (default: brig-elasticsearch-ca)
  - secrets.cassandraCaSecretName: name of the secret (default: brig-cassandra)
*/}}
