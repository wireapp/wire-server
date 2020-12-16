{{ define "turn-servers.txt" }}
{{ range .Values.turnStatic.v1 }}{{ . }}
{{ end -}}
{{ end }}
