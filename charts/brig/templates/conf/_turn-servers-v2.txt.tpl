{{ define "turn-servers-v2.txt" }}
{{ range .Values.turnStatic.v2 }}{{ . }}
{{ end -}}
{{ end }}
