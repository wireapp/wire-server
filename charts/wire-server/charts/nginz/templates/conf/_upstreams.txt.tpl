{{ define "nginz_upstreams.txt" }}
{{ range $key, $value := .Values.nginx_conf.upstreams }}{{ $key }} {{ end -}}
{{ end }}
