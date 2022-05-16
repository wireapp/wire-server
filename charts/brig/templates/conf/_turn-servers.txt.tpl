{{ define "turn-servers.txt" }}
{{- if eq $.Values.turn.serversSource "files" }}
{{ range .Values.turnStatic.v1 }}{{ . }}
{{ end -}}
{{- end }}
{{ end }}