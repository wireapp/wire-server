{{ define "brig.turn-servers.txt" }}
{{- $turn := $.Values.brig.turn | default dict }}
{{- if eq ($turn.serversSource | default "files") "files" }}
{{- $turnStatic := $.Values.brig.turnStatic | default dict }}
{{ range ($turnStatic.v1 | default list) }}{{ . }}
{{ end -}}
{{- end }}
{{ end }}