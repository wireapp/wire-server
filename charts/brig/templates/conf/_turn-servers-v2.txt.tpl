{{ define "turn-servers-v2.txt" }}
{{- if not .Values.turnDNS.enable }}
{{ range .Values.turnStatic.v2 }}{{ . }}
{{ end -}}
{{- end }}
{{ end }}