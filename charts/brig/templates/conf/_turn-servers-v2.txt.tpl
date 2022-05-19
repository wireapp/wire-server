{{ define "turn-servers-v2.txt" }}
{{- if eq $.Values.turn.serversSource "files" }}
{{ range .Values.turnStatic.v2 }}{{ . }}
{{ end -}}
{{- end }}
{{ end }}