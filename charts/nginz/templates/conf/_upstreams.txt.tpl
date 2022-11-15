{{ define "nginz_upstreams.txt" }}
{{- $validUpstreams := include "valid_upstreams" . | fromJsonArray }}
{{ range $key, $value := .Values.nginx_conf.upstreams }}{{ if (has $key $validUpstreams) }} {{ $key }}{{ if hasKey $.Values.nginx_conf.upstream_namespace $key }}.{{ get $.Values.nginx_conf.upstream_namespace $key }}{{end}} {{ end }}{{ end -}}
{{ end }}
