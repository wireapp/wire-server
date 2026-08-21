{{/* Allow KubeVersion to be overridden. */}}
{{- define "kubeVersion" -}}
  {{- default .Capabilities.KubeVersion.Version .Values.kubeVersionOverride -}}
{{- end -}}

{{- define "includeSecurityContext" -}}
  {{- (semverCompare ">= 1.24-0" (include "kubeVersion" .)) -}}
{{- end -}}

{{/* Fully qualified image reference, digest taking precedence over tag. */}}
{{- define "reaper.image" -}}
{{- $repository := .Values.image.repository -}}
{{- if .Values.image.registry -}}
{{- $repository = printf "%s/%s" .Values.image.registry .Values.image.repository -}}
{{- end -}}
{{- if .Values.image.digest -}}
{{- printf "%s@%s" $repository .Values.image.digest -}}
{{- else -}}
{{- printf "%s:%s" $repository (.Values.image.tag | toString) -}}
{{- end -}}
{{- end -}}

{{/* Release-scoped name for the ServiceAccount, Role and RoleBinding. */}}
{{- define "reaper.serviceAccountName" -}}
{{- printf "%s-reaper" .Release.Name | trunc 63 | trimSuffix "-" -}}
{{- end -}}
