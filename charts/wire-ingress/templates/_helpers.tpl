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
{{- define "wire-ingress.getCertificateSecretName" -}}
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
{{- define "wire-ingress.getCustomSolversSecretName" -}}
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
{{- define "wire-ingress.getIssuerName" -}}
{{ .Values.tls.issuer.name }}
{{- end -}}

{{/*
Name of the Gateway resource. Uses gateway.name if set, otherwise derives one from the release name.
*/}}
{{- define "wire-ingress.getGatewayName" -}}
{{- if .Values.gateway.name -}}
{{ .Values.gateway.name }}
{{- else -}}
{{ include "wire-ingress.fullname" . }}-gateway
{{- end -}}
{{- end -}}

{{/*
Whether to use new-style app.kubernetes.io labels for the federation-test-helper.
Kubernetes >= 1.23 uses app.kubernetes.io/*, older versions use app/component labels.
*/}}
{{- define "wire-ingress.integrationTestHelperNewLabels" -}}
  {{- (semverCompare ">= 1.23-0" .Capabilities.KubeVersion.Version) -}}
{{- end -}}
