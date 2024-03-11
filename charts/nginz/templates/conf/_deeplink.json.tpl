{{- define "nginz_deeplink.json" }}
{{- if hasKey .Values.nginx_conf "deeplink" }}
{{- with .Values.nginx_conf.deeplink }}
{{/* See https://docs.wire.com/how-to/associate/deeplink.html
     (or search for "deeplink" on docs.wire.com)
     for details on use of the deeplink*/}}
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
   {{- if hasKey . "apiProxy" }}
   {{- with .apiProxy }}
   "apiProxy" : {
      "host" : {{ .host | quote }},
      "port" : {{ .port }},
      "needsAuthentication" : {{ .needsAuthentication }}
   },
   {{- end }}
   {{- end }}
   "title" : {{ .title | quote }}
}
{{- end }}
{{- else }}
{}
{{- end }}
{{- end }}
