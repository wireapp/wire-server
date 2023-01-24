{{/* Allow KubeVersion to be overridden. */}}
{{- define "kubeVersion" -}}
  {{- default .Capabilities.KubeVersion.Version .Values.kubeVersionOverride -}}
{{- end -}}

{{/* Get Batch API Version */}}
{{- define "batch.apiVersion" -}}
  {{- if and (.Capabilities.APIVersions.Has "batch/v1") (semverCompare ">= 1.21-0" (include "kubeVersion" .)) -}}
      {{- print "batch/v1" -}}
  {{- else -}}
    {{- print "batch/v1beta1" -}}
  {{- end -}}
{{- end -}}
