{{ define "brig.turn-servers-v2.txt" }}
{{- if eq $.Values.brig.turn.serversSource "files" }}
{{ range .Values.brig.turnStatic.v2 }}{{ . }}
{{ end -}}
{{- end }}
{{ end }}