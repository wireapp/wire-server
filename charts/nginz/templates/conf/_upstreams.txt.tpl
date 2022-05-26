{{ define "nginz_upstreams.txt" }}
{{ range $key, $value := .Values.nginx_conf.upstreams }}{{ if not (has $key $.Values.nginx_conf.ignored_upstreams) }} {{ $key }} {{ end }}{{ end -}}
{{ end }}
