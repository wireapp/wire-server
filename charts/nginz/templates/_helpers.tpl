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
Takes no parameters and returns the list of upstreams that should be configured.
*/}}
{{- define "valid_upstreams" -}}
    {{- range $e := $.Values.nginx_conf.ignored_upstreams }}
        {{- if (has $e $.Values.nginx_conf.extra_upstreams) }}
            {{- fail (print "Contradiction: Upstream is in 'extra_upstreams' and 'ignored_upstreams' : " $e) }}
        {{- end }}
    {{- end }}
    {{- $potentiallyIgnored := (concat (list "ibis" "galeb" "calling-test" "proxy") .Values.nginx_conf.ignored_upstreams) -}}
    {{- $ignored := list }}
    {{- range $e := $potentiallyIgnored }}
        {{- if not (has $e $.Values.nginx_conf.extra_upstreams) }}
            {{- $ignored = append $ignored $e }}
        {{- end }}
    {{- end }}
    {{- $validUpstreams := list }}
    {{- range $key, $value := .Values.nginx_conf.upstreams -}}
        {{- if not (has $key $ignored) -}}
            {{- $validUpstreams = append $validUpstreams $key }}
        {{- end -}}
    {{- end -}}
    {{- toJson $validUpstreams}}
{{- end -}}
