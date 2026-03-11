{{ define "brig.turn-servers.txt" }}
{{- $turn := $.Values.brig.turn | default dict }}
{{- if eq ($turn.serversSource | default "files") "files" }}
{{- if not $.Values.brig.turnStatic }}
{{- fail "brig.turnStatic must be configured when turn.serversSource is 'files'" }}
{{- end }}
{{ range .Values.brig.turnStatic.v1 }}{{ . }}
{{ end -}}
{{- end }}
{{ end }}