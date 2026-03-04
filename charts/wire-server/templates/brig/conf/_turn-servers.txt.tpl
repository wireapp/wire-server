{{ define "brig.turn-servers.txt" }}
{{- if eq $.Values.brig.turn.serversSource "files" }}
{{ range .Values.brig.turnStatic.v1 }}{{ . }}
{{ end -}}
{{- end }}
{{ end }}