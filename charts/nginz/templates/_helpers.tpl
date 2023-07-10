{{/* vim: set filetype=mustache: */}}
{{/*
Expand the name of the chart.
*/}}
{{- define "nginz.name" -}}
{{- default .Chart.Name .Values.nameOverride | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Create a default fully qualified app name.
We truncate at 63 chars because some Kubernetes name fields are limited to this (by the DNS naming spec).
*/}}
{{- define "nginz.fullname" -}}
{{- $name := default .Chart.Name .Values.nameOverride -}}
{{- printf "%s-%s" .Release.Name $name | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Takes no parameters and returns a merged map of upstreams ('upstreams' and 'extra_upstreams')
that should be configured.
*/}}
{{- define "valid_upstreams" -}}
    {{- range $e := $.Values.nginx_conf.ignored_upstreams }}
        {{- if not (hasKey $.Values.nginx_conf.upstreams $e) }}
            {{- fail (print "Upstream '" $e "' does not exist in 'upstreams'!") }}
        {{- end }}
    {{- end }}
    {{- range $e := $.Values.nginx_conf.enabled_extra_upstreams }}
        {{- if not (hasKey $.Values.nginx_conf.extra_upstreams $e) }}
            {{- fail (print "Upstream '" $e "' does not exist in 'extra_upstreams'!") }}
        {{- end }}
    {{- end }}

    {{- $validUpstreams := (deepCopy $.Values.nginx_conf.upstreams) }}
    {{- range $key := $.Values.nginx_conf.ignored_upstreams }}
        {{- $validUpstreams = unset $validUpstreams $key}}
    {{- end }}
    {{- range $key := $.Values.nginx_conf.enabled_extra_upstreams }}
        {{- $validUpstreams = set $validUpstreams $key (get $.Values.nginx_conf.extra_upstreams $key)}}
    {{- end }}

    {{- toJson $validUpstreams}}
{{- end -}}

{{/* Allow KubeVersion to be overridden. */}}
{{- define "kubeVersion" -}}
  {{- default .Capabilities.KubeVersion.Version .Values.kubeVersionOverride -}}
{{- end -}}

{{- define "includeSecurityContext" -}}
  {{- (semverCompare ">= 1.24-0" (include "kubeVersion" .)) -}}
{{- end -}}
