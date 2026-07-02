{{/* vim: set filetype=mustache: */}}

{{- define "wire-ingress.name" -}}
{{- default .Chart.Name .Values.nameOverride | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{- define "wire-ingress.fullname" -}}
{{- $name := default .Chart.Name .Values.nameOverride -}}
{{- printf "%s-%s" .Release.Name $name | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Determine DNS zone based on the HTTPS FQDN (e.g. "nginz-https.example.com" → "example.com")
*/}}
{{- define "wire-ingress.zone" -}}
{{- $zones := splitList "." .Values.config.dns.https -}}
{{- slice $zones 1 | join "." -}}
{{- end -}}

{{/*
Name of the TLS certificate secret. Differs based on whether cert-manager is used.
*/}}
{{- define "wire-ingress.certificateSecretName" -}}
{{- if .Values.tls.secret.nameOverride -}}
    {{- .Values.tls.secret.nameOverride -}}
{{- else -}}
    {{- $nameParts := list (include "wire-ingress.fullname" .) -}}
    {{- if .Values.tls.useCertManager -}}
        {{- $nameParts = append $nameParts "managed" -}}
    {{- else -}}
        {{- $nameParts = append $nameParts "wildcard" -}}
    {{- end -}}
    {{- $nameParts = append $nameParts "tls-certificate" -}}
    {{- join "-" $nameParts -}}
{{- end -}}
{{- end -}}

{{/*
Name of the custom ACME solver secret.
*/}}
{{- define "wire-ingress.customSolversSecretName" -}}
{{- $nameParts := list (include "wire-ingress.fullname" .) -}}
{{- $nameParts = append $nameParts "cert-manager-custom-solvers" -}}
{{- join "-" $nameParts -}}
{{- end -}}

{{/*
Returns the Letsencrypt ACME API server URL.
*/}}
{{- define "wire-ingress.certManagerAPIServerURL" -}}
{{- $hostnameParts := list "acme" -}}
{{- if .Values.certManager.inTestMode -}}
    {{- $hostnameParts = append $hostnameParts "staging" -}}
{{- end -}}
{{- $hostnameParts = append $hostnameParts "v02" -}}
{{- join "-" $hostnameParts | printf "https://%s.api.letsencrypt.org/directory" -}}
{{- end -}}

{{/*
Name of the cert-manager Issuer / ClusterIssuer.
*/}}
{{- define "wire-ingress.issuerName" -}}
{{ .Values.tls.issuer.name }}
{{- end -}}

{{/*
Name of the Gateway resource. Uses gateway.name if set, otherwise derives one from the release name.
*/}}
{{- define "wire-ingress.gatewayName" -}}
{{- if .Values.gateway.name -}}
{{ .Values.gateway.name }}
{{- else -}}
{{ include "wire-ingress.fullname" . }}-gateway
{{- end -}}
{{- end -}}

{{/*
Normalized list of ingress domains, returned as a JSON array so callers can
`fromJsonArray` and range over it.

Back-compat: when `config.domains` is NOT set, a single "primary" entry is
derived from the legacy scalar `config.dns` + `gateway.listeners.https.hostname`,
so existing single-domain deployments render exactly as before.

Multi-domain: `config.domains` is a list; the FIRST entry is the primary
(its resources keep the un-suffixed names, and its frontend apps set their own
CSP so no CSP is injected). Every additional entry gets a `-<name>` suffix, its
own Gateway listener (`https-<name>`), its own certificate/secret, and — being
an "additional ingress" — a per-domain CSP header injected on the app routes.

Each entry has: suffix, section, hostname, https, ssl, webapp, teamSettings,
accountPages, fakeS3, base, secretName, certName, issuerName, issuerKind,
primary (bool), csp (bool).
*/}}
{{- define "wire-ingress.domains" -}}
{{- $root := . -}}
{{- $fullname := include "wire-ingress.fullname" . -}}
{{- $out := list -}}
{{- if .Values.config.domains -}}
  {{- range $i, $domain := .Values.config.domains -}}
    {{- $primary := eq $i 0 -}}
    {{- $name := required "each config.domains entry requires a 'name'" $domain.name -}}
    {{- $base := required (printf "config.domains[%d] (%s) requires a 'base' domain" $i $name) $domain.base -}}
    {{- $dns := required (printf "config.domains[%d] (%s) requires a 'dns' map" $i $name) $domain.dns -}}
    {{- $tls := $domain.tls | default dict -}}
    {{- $issuer := $tls.issuer | default dict -}}
    {{- $suffix := ternary "" (printf "-%s" $name) $primary -}}
    {{- $section := ternary "https" (printf "https-%s" $name) $primary -}}
    {{- $secretName := "" -}}
    {{- if $tls.secretName -}}{{- $secretName = $tls.secretName -}}
    {{- else if $primary -}}{{- $secretName = include "wire-ingress.certificateSecretName" $root -}}
    {{- else -}}{{- $secretName = printf "%s-%s-tls-certificate" $fullname $name -}}{{- end -}}
    {{- $cspFlag := true -}}
    {{- if hasKey $domain "renderCSP" -}}{{- $cspFlag = $domain.renderCSP -}}{{- end -}}
    {{- $entry := dict
        "suffix" $suffix
        "section" $section
        "hostname" ($domain.hostname | default (printf "*.%s" $base))
        "https" (required (printf "config.domains[%d] (%s) requires dns.https" $i $name) $dns.https)
        "ssl" ($dns.ssl | default "")
        "webapp" ($dns.webapp | default "")
        "teamSettings" ($dns.teamSettings | default "")
        "accountPages" ($dns.accountPages | default "")
        "fakeS3" ($dns.fakeS3 | default "")
        "base" $base
        "secretName" $secretName
        "certName" (printf "%s-csr" ($base | replace "." "-"))
        "issuerName" ($issuer.name | default $root.Values.tls.issuer.name)
        "issuerKind" ($issuer.kind | default $root.Values.tls.issuer.kind)
        "primary" $primary
        "csp" (and (not $primary) $cspFlag) -}}
    {{- $out = append $out $entry -}}
  {{- end -}}
{{- else -}}
  {{- $dns := .Values.config.dns -}}
  {{- $base := include "wire-ingress.zone" . -}}
  {{- $entry := dict
      "suffix" ""
      "section" "https"
      "hostname" .Values.gateway.listeners.https.hostname
      "https" (required "config.dns.https is required" $dns.https)
      "ssl" ($dns.ssl | default "")
      "webapp" ($dns.webapp | default "")
      "teamSettings" ($dns.teamSettings | default "")
      "accountPages" ($dns.accountPages | default "")
      "fakeS3" ($dns.fakeS3 | default "")
      "base" $base
      "secretName" (include "wire-ingress.certificateSecretName" .)
      "certName" (printf "%s-csr" ($base | replace "." "-"))
      "issuerName" .Values.tls.issuer.name
      "issuerKind" .Values.tls.issuer.kind
      "primary" true
      "csp" false -}}
  {{- $out = append $out $entry -}}
{{- end -}}
{{- $out | toJson -}}
{{- end -}}

{{/*
Content-Security-Policy header value for an "additional ingress" domain.
This mirrors the approximation the legacy nginx-ingress-services chart injected
for multi-ingress domains (charts/nginx-ingress-services/templates/ingress.yaml),
where the primary domain's frontend apps set CSP themselves but additional
domains need the header set at the front door.

Call with a dict: {https, ssl, base, websockets (bool)}.
*/}}
{{- define "wire-ingress.cspHeader" -}}
{{- $csp := printf "connect-src 'self' blob: data: https://*.giphy.com https://%s" .https -}}
{{- if and .websockets .ssl -}}{{- $csp = printf "%s wss://%s" $csp .ssl -}}{{- end -}}
{{- $csp = printf "%s https://*.%s;" $csp .base -}}
{{- $csp = printf "%s default-src 'self';" $csp -}}
{{- $csp = printf "%s font-src 'self' data:;" $csp -}}
{{- $csp = printf "%s frame-src https://*.soundcloud.com https://*.spotify.com https://*.vimeo.com https://*.youtube-nocookie.com;" $csp -}}
{{- $csp = printf "%s img-src 'self' blob: data: https://*.giphy.com https://*.%s;" $csp .base -}}
{{- $csp = printf "%s manifest-src 'self';" $csp -}}
{{- $csp = printf "%s media-src 'self' blob: data:;" $csp -}}
{{- $csp = printf "%s object-src 'none';" $csp -}}
{{- $csp = printf "%s script-src 'self' 'unsafe-eval' https://*.%s;" $csp .base -}}
{{- $csp = printf "%s style-src 'self' 'unsafe-inline';" $csp -}}
{{- $csp = printf "%s worker-src 'self' blob:;" $csp -}}
{{- $csp = printf "%s base-uri 'self';" $csp -}}
{{- $csp = printf "%s form-action 'self';" $csp -}}
{{- $csp = printf "%s frame-ancestors 'self';" $csp -}}
{{- $csp = printf "%s script-src-attr 'none';" $csp -}}
{{- $csp = printf "%s upgrade-insecure-requests" $csp -}}
{{- $csp -}}
{{- end -}}
