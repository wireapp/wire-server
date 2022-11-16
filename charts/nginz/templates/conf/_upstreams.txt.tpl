{{- define "nginz_upstreams.txt" }}
{{- $validUpstreams := include "valid_upstreams" . | fromJson }}
{{- range $key, $value := $validUpstreams }}{{ $key }}{{- if hasKey $.Values.nginx_conf.upstream_namespace $key }}.{{ get $.Values.nginx_conf.upstream_namespace $key }}{{end}} {{ end -}}
{{- end }}
