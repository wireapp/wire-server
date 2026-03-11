{{ define "brig.turn-servers-v2.txt" }}
{{- $turn := $.Values.brig.turn | default dict }}
{{- if eq ($turn.serversSource | default "files") "files" }}
{{- if not $.Values.brig.turnStatic }}
{{- fail "brig.turnStatic must be configured when turn.serversSource is 'files'" }}
{{- end }}
{{ range .Values.brig.turnStatic.v2 }}{{ . }}
{{ end -}}
{{- end }}
{{ end }}