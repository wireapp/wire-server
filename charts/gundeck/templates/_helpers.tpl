
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
{{- dict "name" "gundeck-cassandra" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}

{{- define "configureRedisCa" -}}
{{ or (hasKey .redis "tlsCa") (hasKey .redis "tlsCaSecretRef") }}
{{- end -}}

{{- define "redisTlsSecretName" -}}
{{- if .redis.tlsCaSecretRef -}}
{{ .redis.tlsCaSecretRef.name }}
{{- else }}
{{- print "gundeck-redis-ca" -}}
{{- end -}}
{{- end -}}

{{- define "redisTlsSecretKey" -}}
{{- if .redis.tlsCaSecretRef -}}
{{ .redis.tlsCaSecretRef.key }}
{{- else }}
{{- print "ca.pem" -}}
{{- end -}}
{{- end -}}

{{- define "configureAdditionalRedisCa" -}}
{{ and (hasKey . "redisAdditionalWrite") (or (hasKey .redisAdditionalWrite "additionalTlsCa") (hasKey .redis "additionalTlsCaSecretRef")) }}
{{- end -}}

{{- define "additionalRedisTlsSecretName" -}}
{{- if .redis.additionalTlsCaSecretRef -}}
{{ .redis.additionalTlsCaSecretRef.name }}
{{- else }}
{{- print "gundeck-additional-redis-ca" -}}
{{- end -}}
{{- end -}}

{{- define "additionalRedisTlsSecretKey" -}}
{{- if .redis.additionalTlsCaSecretRef -}}
{{ .redis.additionalTlsCaSecretRef.key }}
{{- else }}
{{- print "ca.pem" -}}
{{- end -}}
{{- end -}}
