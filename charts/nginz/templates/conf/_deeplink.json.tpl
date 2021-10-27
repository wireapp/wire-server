{{- define "nginz_deeplink.json" }}
{{- if hasKey .Values.nginx_conf "deeplink" }}
{{- with .Values.nginx_conf.deeplink }}
{
  "endpoints" : {
      {{- with .endpoints }}
      "backendURL" : {{ .backendURL | quote }},
      "backendWSURL": {{ .backendWSURL | quote }},
      "blackListURL": {{ .blackListURL | quote }},
      "teamsURL": {{ .teamsURL | quote }},
      "accountsURL": {{ .accountsURL | quote }},
      "websiteURL": {{ .websiteURL | quote }}
      {{- end }}
   },
   "title" : {{ .title | quote }}
}
{{- end }}
{{- else }}
{}
{{- end }}
{{- end }}