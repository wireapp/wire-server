{{ define "turn-servers.txt" }}
{{- if not .Values.turnDNS.enable }}
{{ range .Values.turnStatic.v1 }}{{ . }}
{{ end -}}
{{- end }}
{{ end }}