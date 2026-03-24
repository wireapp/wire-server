
{{/*
Name of the Gateway resource for dynamic backends in envoy mode.
*/}}
{{- define "integration.getDynBackendsGatewayName" -}}
{{- if .Values.envoy.gateway.name -}}
{{ .Values.envoy.gateway.name }}
{{- else -}}
{{ .Release.Name }}-dynamic-backends
{{- end -}}
{{- end -}}

{{/*
Federation origin domain for a given namespace (used as originDomain in the config).
Returns the SRV hostname that other backends use to reach this namespace's federator.
Args: list $namespace $envoyEnabled
*/}}
{{- define "integration.federationOriginDomain" -}}
{{- $namespace := index . 0 -}}
{{- $envoyEnabled := index . 1 -}}
{{- if $envoyEnabled -}}
{{- printf "%s-fed.envoy-gateway-system.svc.cluster.local" $namespace -}}
{{- else -}}
{{- printf "federation-test-helper.%s.svc.cluster.local" $namespace -}}
{{- end -}}
{{- end -}}

{{/*
Domain for a dynamic backend. Returns the correct hostname depending on whether
envoy mode is enabled.
Args: list $dynamicBackend $namespace $envoyEnabled
*/}}
{{- define "integration.dynamicBackendDomain" -}}
{{- $dynamicBackend := index . 0 -}}
{{- $namespace := index . 1 -}}
{{- $envoyEnabled := index . 2 -}}
{{- if $envoyEnabled -}}
{{- printf "%s-%s.envoy-gateway-system.svc.cluster.local" $dynamicBackend.federatorExternalHostPrefix $namespace -}}
{{- else -}}
{{- printf "%s.%s.svc.cluster.local" $dynamicBackend.federatorExternalHostPrefix $namespace -}}
{{- end -}}
{{- end -}}

{{/* Allow KubeVersion to be overridden. */}}
{{- define "kubeVersion" -}}
  {{- default $.Capabilities.KubeVersion.Version $.Values.kubeVersionOverride -}}
{{- end -}}

{{- define "includeSecurityContext" -}}
  {{- (semverCompare ">= 1.24-0" (include "kubeVersion" .)) -}}
{{- end -}}

{{- define "integrationTestHelperNewLabels" -}}
  {{- (semverCompare ">= 1.23-0" (include "kubeVersion" .)) -}}
{{- end -}}

{{- define "useCassandraTLS" -}}
{{ or (hasKey .cassandra "tlsCa") (hasKey .cassandra "tlsCaSecretRef") }}
{{- end -}}

{{- define "cassandraTlsSecretName" -}}
{{- if .cassandra.tlsCaSecretRef -}}
{{ .cassandra.tlsCaSecretRef.name }}
{{- else }}
{{- print "integration-cassandra" -}}
{{- end -}}
{{- end -}}

{{- define "cassandraTlsSecretKey" -}}
{{- if .cassandra.tlsCaSecretRef -}}
{{ .cassandra.tlsCaSecretRef.key }}
{{- else }}
{{- print "ca.pem" -}}
{{- end -}}
{{- end -}}
