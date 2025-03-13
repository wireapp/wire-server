{{/* Allow KubeVersion to be overridden. */}}
{{- define "kubeVersion" -}}
  {{- default .Capabilities.KubeVersion.Version .Values.kubeVersionOverride -}}
{{- end -}}

{{- define "includeSecurityContext" -}}
  {{- (semverCompare ">= 1.24-0" (include "kubeVersion" .)) -}}
{{- end -}}

{{- define "useCassandraTLS" -}}
{{ or (hasKey .cassandra "tlsCa") (hasKey .cassandra "tlsCaSecretRef") }}
{{- end -}}

{{/* Return a Dict of TLS CA secret name and key
This is used to switch between provided secret (e.g. by cert-manager) and
created one (in case the CA is provided as PEM string.)
*/}}
{{- define "tlsSecretRef" -}}
{{- if .cassandra.tlsCaSecretRef -}}
{{ .cassandra.tlsCaSecretRef | toYaml }}
{{- else }}
{{- dict "name" "spar-cassandra" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}

{{/* Compute the SCIM base URI

The rules are:
- If `scimBaseUri` is defined, take that value
- Otherwise, if `spAppUri` is defined, take the value and drop a possible last /sso path element
- Otherwise, fail
*/}}
{{- define "computeScimBaseUri" -}}
{{- $scimBaseUri := .Values.config.scimBaseUri -}}
{{- $spAppUri := .Values.config.spAppUri -}}
{{- if $scimBaseUri -}}
  {{- $scimBaseUri -}}
{{- else if $spAppUri -}}
  {{- $parts := split "/" $spAppUri -}}
  {{- if eq (last $parts) "sso" -}}
    {{- $baseUri := (pluck 0 (sub 1 (len $parts)) $parts) | join "/" -}}
    {{- $baseUri -}}
  {{- else -}}
    {{- $spAppUri -}}
  {{- end -}}
{{- else -}}
  {{- fail "Either scimBaseUri or spAppUri must be defined" -}}
{{- end -}}
{{- end -}}
