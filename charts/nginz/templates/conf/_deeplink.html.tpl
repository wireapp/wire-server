{{- define "nginz_deeplink.html" }}
{{/* See https://docs.wire.com/how-to/associate/deeplink.html
     (or search for "deeplink" on docs.wire.com)
     for details on use of the deeplink*/}}
<html>
  <head></head>
  <body>
    {{- if hasKey .Values.nginx_conf "deeplink" }}
    <a href="wire://access/?config={{ .Values.nginx_conf.deeplink.endpoints.backendURL }}/deeplink.json">Click here for access</a>
    {{- else }}
    No Deep Link.
    {{- end }}
  </body>
</html>
{{- end }}
