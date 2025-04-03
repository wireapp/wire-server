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
- Otherwise, if `ssoUri` is defined, take the value and drop a possible last
  /sso path element
- Otherwise, fail

In multi-ingress setups you have to configure the `scimBaseUri`, because it
cannot be decided which `ssoUri` to take from the map.
*/}}
{{- define "computeScimBaseUri" -}}
{{- $scimBaseUri := .scimBaseUri -}}
{{- $ssoUri := .ssoUri -}}
{{- if $scimBaseUri -}}
  {{- $scimBaseUri -}}
{{- else if $ssoUri -}}
  {{- $parts := splitList "/" $ssoUri -}}
  {{- if eq (last $parts) "sso" -}}
    {{- $baseUri := $parts | reverse | rest | reverse | join "/" -}}
    {{- $baseUri -}}/scim/v2
  {{- else -}}
    {{- $ssoUri -}}/scim/v2
  {{- end -}}
{{- else -}}
  {{- fail "Either scimBaseUri or ssoUri must be defined" -}}
{{- end -}}
{{- end -}}
