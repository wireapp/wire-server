
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
{{- dict "name" "brig-cassandra" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}


{{- define "configureElasticSearchCa" -}}
{{ or (hasKey .elasticsearch "tlsCa") (hasKey .elasticsearch "tlsCaSecretRef") }}
{{- end -}}

{{- define "elasticsearchTlsSecretName" -}}
{{- if .elasticsearch.tlsCaSecretRef -}}
{{ .elasticsearch.tlsCaSecretRef.name }}
{{- else }}
{{- print "brig-elasticsearch-ca" -}}
{{- end -}}
{{- end -}}

{{- define "elasticsearchTlsSecretKey" -}}
{{- if .elasticsearch.tlsCaSecretRef -}}
{{ .elasticsearch.tlsCaSecretRef.key }}
{{- else }}
{{- print "ca.pem" -}}
{{- end -}}
{{- end -}}

{{- define "configureAdditionalElasticSearchCa" -}}
{{ or (hasKey .elasticsearch "additionalTlsCa") (hasKey .elasticsearch "additionalTlsCaSecretRef") }}
{{- end -}}

{{- define "additionalElasticsearchTlsSecretName" -}}
{{- if .elasticsearch.additionalTlsCaSecretRef -}}
{{ .elasticsearch.additionalTlsCaSecretRef.name }}
{{- else }}
{{- print "brig-additional-elasticsearch-ca" -}}
{{- end -}}
{{- end -}}

{{- define "additionalElasticsearchTlsSecretKey" -}}
{{- if .elasticsearch.additionalTlsCaSecretRef -}}
{{ .elasticsearch.additionalTlsCaSecretRef.key }}
{{- else }}
{{- print "ca.pem" -}}
{{- end -}}
{{- end -}}
