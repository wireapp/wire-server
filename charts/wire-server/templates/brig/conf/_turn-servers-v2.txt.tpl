{{ define "brig.turn-servers-v2.txt" }}
{{- $turn := $.Values.brig.turn | default dict }}
{{- if eq ($turn.serversSource | default "files") "files" }}
{{- $turnStatic := $.Values.brig.turnStatic | default dict }}
{{ range ($turnStatic.v2 | default list) }}{{ . }}
{{ end -}}
{{- end }}
{{ end }}