
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

{{- define "cassandraTlsSecretName" -}}
{{- if .cassandra.tlsCaSecretRef -}}
{{ .cassandra.tlsCaSecretRef.name }}
{{- else }}
{{- print "elasticsearch-index-migrate-cassandra-client-ca" -}}
{{- end -}}
{{- end -}}

{{- define "cassandraTlsSecretKey" -}}
{{- if .cassandra.tlsCaSecretRef -}}
{{ .cassandra.tlsCaSecretRef.key }}
{{- else }}
{{- print "ca.pem" -}}
{{- end -}}
{{- end -}}

{{- define "configureElasticsearchCa" -}}
{{ or (hasKey .elasticsearch "tlsCa") (hasKey .elasticsearch "tlsCaSecretRef") }}
{{- end -}}

{{- define "elasticsearchTlsSecretName" -}}
{{- if .elasticsearch.tlsCaSecretRef -}}
{{ .elasticsearch.tlsCaSecretRef.name }}
{{- else }}
{{- printf "%s-ca" (include "fullname" .) -}}
{{- end -}}
{{- end -}}

{{- define "elasticsearchTlsSecretKey" -}}
{{- if .elasticsearch.tlsCaSecretRef -}}
{{ .elasticsearch.tlsCaSecretRef.key }}
{{- else }}
{{- print "ca.pem" -}}
{{- end -}}
{{- end -}}
