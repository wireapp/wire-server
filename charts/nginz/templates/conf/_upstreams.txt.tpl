{{ define "nginz_upstreams.txt" }}
{{ range $key, $value := .Values.nginx_conf.upstreams }}{{ if not (has $key $.Values.nginx_conf.ignored_upstreams) }} {{ $key }}{{ if hasKey $.Values.nginx_conf.upstream_namespace $key }}.{{ get $.Values.nginx_conf.upstream_namespace $key }}{{end}} {{ end }}{{ end -}}
{{ end }}