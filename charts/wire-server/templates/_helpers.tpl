
{{/* SHARED HELPERS */}}
{{/* Allow KubeVersion to be overridden. */}}
{{- define "kubeVersion" -}}
  {{- default .Capabilities.KubeVersion.Version .Values.kubeVersionOverride -}}
{{- end -}}
{{- define "includeSecurityContext" -}}
  {{- (semverCompare ">= 1.24-0" (include "kubeVersion" .)) -}}
{{- end -}}

{{- define "useCassandraTLS" -}}
{{ or (hasKey . "tlsCa") (hasKey . "tlsCaSecretRef") }}
{{- end -}}

{{/* GALLEY */}}
{{- define "galley.tlsSecretRef" -}}
{{- if .cassandra.tlsCaSecretRef -}}
{{ .cassandra.tlsCaSecretRef | toYaml }}
{{- else }}
{{- dict "name" "galley-cassandra" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}


{{/* BACKGROUND-WORKER */}}
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

{{- define "galleyTlsSecretRef" -}}
{{- if and .cassandraGalley .cassandraGalley.tlsCaSecretRef -}}
{{ .cassandraGalley.tlsCaSecretRef | toYaml }}
{{- else }}
{{- dict "name" "background-worker-cassandra-galley" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}

{{/* BRIG */}}
{{- define "brig.tlsSecretRef" -}}
{{- if .cassandra.tlsCaSecretRef -}}
{{ .cassandra.tlsCaSecretRef | toYaml }}
{{- else }}
{{- dict "name" "brig-cassandra" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}

{{- define "brig.configureElasticSearchCa" -}}
{{ or (hasKey .elasticsearch "tlsCa") (hasKey .elasticsearch "tlsCaSecretRef") }}
{{- end -}}

{{- define "brig.elasticsearchTlsSecretName" -}}
{{- if .elasticsearch.tlsCaSecretRef -}}
{{ .elasticsearch.tlsCaSecretRef.name }}
{{- else }}
{{- print "brig-elasticsearch-ca" -}}
{{- end -}}
{{- end -}}

{{- define "brig.elasticsearchTlsSecretKey" -}}
{{- if .elasticsearch.tlsCaSecretRef -}}
{{ .elasticsearch.tlsCaSecretRef.key }}
{{- else }}
{{- print "ca.pem" -}}
{{- end -}}
{{- end -}}

{{- define "brig.configureAdditionalElasticSearchCa" -}}
{{ or (hasKey .elasticsearch "additionalTlsCa") (hasKey .elasticsearch "additionalTlsCaSecretRef") }}
{{- end -}}

{{- define "brig.additionalElasticsearchTlsSecretName" -}}
{{- if .elasticsearch.additionalTlsCaSecretRef -}}
{{ .elasticsearch.additionalTlsCaSecretRef.name }}
{{- else }}
{{- print "brig-additional-elasticsearch-ca" -}}
{{- end -}}
{{- end -}}

{{- define "brig.additionalElasticsearchTlsSecretKey" -}}
{{- if .elasticsearch.additionalTlsCaSecretRef -}}
{{ .elasticsearch.additionalTlsCaSecretRef.key }}
{{- else }}
{{- print "ca.pem" -}}
{{- end -}}
{{- end -}}

{{/* CANNON */}}
{{- define "cannon.tlsSecretRef" -}}
{{- if .cassandra.tlsCaSecretRef -}}
{{ .cassandra.tlsCaSecretRef | toYaml }}
{{- else }}
{{- dict "name" "cannon-cassandra" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}

{{/* GUNDECK */}}
{{- define "gundeck.tlsSecretRef" -}}
{{- if .cassandra.tlsCaSecretRef -}}
{{ .cassandra.tlsCaSecretRef | toYaml }}
{{- else }}
{{- dict "name" "gundeck-cassandra" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}

{{- define "gundeck.configureRedisCa" -}}
{{ or (hasKey .redis "tlsCa") (hasKey .redis "tlsCaSecretRef") }}
{{- end -}}

{{- define "gundeck.redisTlsSecretName" -}}
{{- if .redis.tlsCaSecretRef -}}
{{ .redis.tlsCaSecretRef.name }}
{{- else }}
{{- print "gundeck-redis-ca" -}}
{{- end -}}
{{- end -}}

{{- define "gundeck.redisTlsSecretKey" -}}
{{- if .redis.tlsCaSecretRef -}}
{{ .redis.tlsCaSecretRef.key }}
{{- else }}
{{- print "ca.pem" -}}
{{- end -}}
{{- end -}}

{{- define "gundeck.configureAdditionalRedisCa" -}}
{{ and (hasKey . "redisAdditionalWrite") (or (hasKey .redis "additionalTlsCa") (hasKey .redis "additionalTlsCaSecretRef")) }}
{{- end -}}

{{- define "gundeck.additionalRedisTlsSecretName" -}}
{{- if .redis.additionalTlsCaSecretRef -}}
{{ .redis.additionalTlsCaSecretRef.name }}
{{- else }}
{{- print "gundeck-additional-redis-ca" -}}
{{- end -}}
{{- end -}}

{{- define "gundeck.additionalRedisTlsSecretKey" -}}
{{- if .redis.additionalTlsCaSecretRef -}}
{{ .redis.additionalTlsCaSecretRef.key }}
{{- else }}
{{- print "ca.pem" -}}
{{- end -}}
{{- end -}}

{{/* SPAR */}}
{{- define "spar.tlsSecretRef" -}}
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
{{- define "spar.computeScimBaseUri" -}}
{{- if .scimBaseUri -}}
  {{- .scimBaseUri -}}
{{- else if .ssoUri -}}
  {{- $parts := splitList "/" .ssoUri -}}
  {{- if eq (last $parts) "sso" -}}
    {{- $baseUri := $parts | reverse | rest | reverse | join "/" -}}
    {{- $baseUri -}}/scim/v2
  {{- else -}}
    {{- .ssoUri -}}/scim/v2
  {{- end -}}
{{- else -}}
  {{- fail "Either scimBaseUri or ssoUri must be defined" -}}
{{- end -}}
{{- end -}}
