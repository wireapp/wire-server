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
    {{- toJson $.Values.nginx_conf.upstreams }}
{{- end -}}

{{- define "valid_upstreams_2" -}}
    {{- range $e := $.Values.nginx_conf.ignored_upstreams }}
        {{- if not (hasKey $.Values.nginx_conf.upstreams $e) }}
            {{- fail (print "Upstream '" $e "' does not exist in 'upstreams'!" (toYaml $.Values.nginx_conf.upstreams)) }}
        {{- end }}
    {{- end }}
    {{- range $e := $.Values.nginx_conf.enabled_extra_upstreams }}
        {{- if not (hasKey $.Values.nginx_conf.extra_upstreams $e) }}
            {{- fail (print "Upstream '" $e "' does not exist in 'extra_upstreams'!") }}
        {{- end }}
    {{- end }}

    {{- $validUpstreams := $.Values.nginx_conf.upstreams }}
    {{- range $key := $.Values.nginx_conf.ignored_upstreams }}
        {{- $validUpstreams = unset $validUpstreams $key}}
    {{- end }}
    {{- range $key := $.Values.nginx_conf.enabled_extra_upstreams }}
        {{- $validUpstreams = set $validUpstreams $key (get $.Values.nginx_conf.enabled_extra_upstreams $key)}}
    {{- end }}

    {{- toJson $validUpstreams}}
{{- end -}}
