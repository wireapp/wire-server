{{- define "nginz_deeplink.html" }}
<html>
  <head></head>
  <body>
    {{- if hasKey .Values.nginx_conf "deeplink" }}
    <a href="wire://access/?config=https://{{ .Values.nginx_conf.deeplink.endpoints.backendURL }}/deeplink/deeplink.json">Click here for access</a>
    {{- else }}
    No Deep Link.
    {{- end }}
  </body>
</html>
{{- end }}
