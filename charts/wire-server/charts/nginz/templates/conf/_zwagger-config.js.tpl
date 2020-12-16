{{ define "nginz_zwagger-config.js" }}
var environment = '{{ .Values.nginx_conf.env }}';
{{- end }}
