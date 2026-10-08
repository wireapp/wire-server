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

{{- define "wire-ingress.httpsSectionName" -}}
{{- .Values.gateway.listeners.https.sectionName | default "https" -}}
{{- end -}}

{{- define "wire-ingress.federatorSectionName" -}}
{{- .Values.gateway.listeners.federator.sectionName | default "federator" -}}
{{- end -}}

{{/* Normalize config.dns/config.domains for listeners, certificates and routes.
     The primary domain keeps unsuffixed resource names; additional domains get
     their own certificates and team-settings CSP headers. */}}
{{- define "wire-ingress.domains" -}}
{{- $root := . -}}
{{- $fullname := include "wire-ingress.fullname" . -}}
{{- $out := list -}}
{{- if .Values.config.domains -}}
  {{- if .Values.federator.enabled -}}
    {{- fail "config.domains (multi-ingress) is mutually exclusive with federator.enabled (federation). Choose one: federation with a single backend domain via config.dns, OR multi-ingress via config.domains with federator.enabled=false." -}}
  {{- end -}}
  {{- range $i, $domain := .Values.config.domains -}}
    {{- $primary := eq $i 0 -}}
    {{- $name := required "each config.domains entry requires a 'name'" $domain.name -}}
    {{- $base := required (printf "config.domains[%d] (%s) requires a 'base' domain" $i $name) $domain.base -}}
    {{- $dns := required (printf "config.domains[%d] (%s) requires a 'dns' map" $i $name) $domain.dns -}}
    {{- $tls := $domain.tls | default dict -}}
    {{- $issuer := $tls.issuer | default dict -}}
    {{- $suffix := ternary "" (printf "-%s" $name) $primary -}}
    {{- $section := ternary (include "wire-ingress.httpsSectionName" $root) ($domain.sectionName | default (printf "https-%s" $name)) $primary -}}
    {{- $secretName := "" -}}
    {{- if $tls.secretName -}}{{- $secretName = $tls.secretName -}}
    {{- else if $primary -}}{{- $secretName = include "wire-ingress.certificateSecretName" $root -}}
    {{- else -}}{{- $secretName = printf "%s-%s-tls-certificate" $fullname $name -}}{{- end -}}
    {{- $cspFlag := not $primary -}}
    {{- if hasKey $domain "renderCSP" -}}{{- $cspFlag = $domain.renderCSP -}}{{- end -}}
    {{/* Without cert-manager, additional domains need an existing TLS secret. */}}
    {{- if and (not $primary) (not $root.Values.tls.useCertManager) (not $tls.secretName) -}}
      {{- fail (printf "config.domains[%d] (%s): additional domains need their own TLS secret, but tls.useCertManager is false and no config.domains[%d].tls.secretName is set. Either enable cert-manager (tls.useCertManager: true) or point tls.secretName at a pre-created kubernetes.io/tls Secret for this domain." $i $name $i) -}}
    {{- end -}}
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
        "csp" $cspFlag -}}
    {{- $out = append $out $entry -}}
  {{- end -}}
{{- else -}}
  {{- $dns := .Values.config.dns -}}
  {{- $base := include "wire-ingress.zone" . -}}
  {{- $entry := dict
      "suffix" ""
      "section" (include "wire-ingress.httpsSectionName" .)
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

Only the team-settings route uses this. The webapp and account-pages routes are
excluded, matching the `$skip_csp` hosts in the nginx chart's snippet, because
those apps emit correct per-domain headers on their own.

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

{{/* Shared TLS/ALPN settings for the ListenerSet and federation policies. */}}
{{- define "wire-ingress.downstreamTls" -}}
{{- $tls := .Values.gateway.tls -}}
{{- if .Values.gateway.alpn.enabled }}
alpnProtocols:
  {{- range .Values.gateway.alpn.protocols }}
  - {{ . }}
  {{- end }}
{{- end }}
{{- if $tls.enabled }}
{{- if .Values.FIPS_202205_tls_profile }}
# Safe baseline: only the compliance patch may enable TLS 1.3.
minVersion: "1.2"
maxVersion: "1.2"
ciphers:
  - ECDHE-ECDSA-AES128-GCM-SHA256
  - ECDHE-ECDSA-AES256-GCM-SHA384
  - ECDHE-RSA-AES128-GCM-SHA256
  - ECDHE-RSA-AES256-GCM-SHA384
ecdhCurves: [P-256, P-384]
signatureAlgorithms:
  - ecdsa_secp256r1_sha256
  - ecdsa_secp384r1_sha384
  - rsa_pss_rsae_sha256
  - rsa_pss_rsae_sha384
  - rsa_pss_rsae_sha512
{{- else }}
{{- $minVersion := $tls.minVersion | default "" | toString }}
{{- if $minVersion }}
minVersion: {{ $minVersion | quote }}
{{- end }}
{{- if $tls.maxVersion }}
maxVersion: {{ $tls.maxVersion | toString | quote }}
{{- end }}
{{- /* EG rejects ciphers alongside minVersion 1.3; suites only affect TLS <=1.2. */}}
{{- if and $tls.ciphers (ne $minVersion "1.3") }}
ciphers: {{ toJson $tls.ciphers }}
{{- end }}
{{- if $tls.ecdhCurves }}
ecdhCurves: {{ toJson $tls.ecdhCurves }}
{{- end }}
{{- if $tls.signatureAlgorithms }}
signatureAlgorithms: {{ toJson $tls.signatureAlgorithms }}
{{- end }}
{{- end }}
{{- end }}
{{- end }}
