{{- define "nginz_deeplink.html" }}
{{/* See https://docs.wire.com/how-to/associate/deeplink.html
     (or search for "deeplink" on docs.wire.com)
     for details on use of the deeplink*/}}
<html>
  <head>
   <meta name="viewport" content="width=device-width, initial-scale=1.0" />
  </head>
  <body style="font-size: 1.5em; font-family: sans">
    <h4>Wire client setup</h4>
    {{- if hasKey .Values.nginx_conf "deeplink" }}
    <p>
    <label>backend: <code>{{ .Values.nginx_conf.deeplink.endpoints.backendURL }}/deeplink.json</code></label>
    </p>
    <a href="wire://access/?config={{ .Values.nginx_conf.deeplink.endpoints.backendURL }}/deeplink.json">
    <button >Configure client to use this backend</button>
    </a>
    {{- else }}
    No Deep Link.
    {{- end }}
  </body>
</html>
{{- end }}
