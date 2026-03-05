
{{/* SHARED HELPERS */}}
{{/* Allow KubeVersion to be overridden. */}}
{{- define "kubeVersion" -}}
  {{- default .Capabilities.KubeVersion.Version .Values.kubeVersionOverride -}}
{{- end -}}
{{- define "includeSecurityContext" -}}
  {{- (semverCompare ">= 1.24-0" (include "kubeVersion" .)) -}}
{{- end -}}


{{/* GALLEY */}}
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


{{/* BACKGROUND-WORKER */}}
{{- define "useGundeckCassandraTLS" -}}
{{ or (hasKey .cassandra "tlsCa") (hasKey .cassandra "tlsCaSecretRef") }}
{{- end -}}

{{- define "useBrigCassandraTLS" -}}
{{ or (hasKey .cassandraBrig "tlsCa") (hasKey .cassandraBrig "tlsCaSecretRef") }}
{{- end -}}

{{- define "useGalleyCassandraTLS" -}}
{{ or (hasKey .cassandraGalley "tlsCa") (hasKey .cassandraGalley "tlsCaSecretRef") }}
{{- end -}}

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

{{/* Return a Dict of TLS CA secret name and key
This is used to switch between provided secret (e.g. by cert-manager) and
created one (in case the CA is provided as PEM string.)
*/}}
{{- define "galleyTlsSecretRef" -}}
{{- if and .cassandraGalley .cassandraGalley.tlsCaSecretRef -}}
{{ .cassandraGalley.tlsCaSecretRef | toYaml }}
{{- else }}
{{- dict "name" "background-worker-cassandra-galley" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}

{{/* BRIG */}}
{{- define "brig.useCassandraTLS" -}}
{{ or (hasKey .cassandra "tlsCa") (hasKey .cassandra "tlsCaSecretRef") }}
{{- end -}}

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
{{- define "cannon.useCassandraTLS" -}}
{{ or (hasKey .cassandra "tlsCa") (hasKey .cassandra "tlsCaSecretRef") }}
{{- end -}}

{{/* Return a Dict of TLS CA secret name and key
This is used to switch between provided secret (e.g. by cert-manager) and
created one (in case the CA is provided as PEM string.)
*/}}
{{- define "cannon.tlsSecretRef" -}}
{{- if .cassandra.tlsCaSecretRef -}}
{{ .cassandra.tlsCaSecretRef | toYaml }}
{{- else }}
{{- dict "name" "cannon-cassandra" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}
