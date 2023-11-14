
{{/* Allow KubeVersion to be overridden. */}}
{{- define "kubeVersion" -}}
  {{- default .Capabilities.KubeVersion.Version .Values.kubeVersionOverride -}}
{{- end -}}

{{- define "includeSecurityContext" -}}
  {{- (semverCompare ">= 1.24-0" (include "kubeVersion" .)) -}}
{{- end -}}

{{- define "useCassandraTLS" -}}
{{ and (hasKey .cassandra "tls") .cassandra.tls.enabled }}
{{- end -}}

{{- define "useCassandraCA" -}}
{{/* The evaluation of Helm is odd: This cannot call useCassandraTLS without changing the evaluation order. */}}
{{ and (hasKey .cassandra "tls") .cassandra.tls.enabled (hasKey .cassandra.tls "ca") }}
{{- end -}}
