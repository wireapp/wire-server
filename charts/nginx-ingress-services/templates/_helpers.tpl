{{/* vim: set filetype=mustache: */}}
{{/*
Expand the name of the chart.
*/}}
{{- define "nginx-ingress-services.name" -}}
{{- default .Chart.Name .Values.nameOverride | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Create a default fully qualified app name.
We truncate at 63 chars because some Kubernetes name fields are limited to this (by the DNS naming spec).
*/}}
{{- define "nginx-ingress-services.fullname" -}}
{{- $name := default .Chart.Name .Values.nameOverride -}}
{{- printf "%s-%s" .Release.Name $name | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Determine DNS zone based on one of the given FQDNs
*/}}
{{- define "nginx-ingress-services.zone" -}}
{{- $zones := splitList "." .Values.config.dns.https -}}
{{- slice $zones 1 | join "." -}}
{{- end -}}

{{/*
Generate the secrate name in a conistent way, since it's referred to in multiple places, while
at the same time being used for distinct scenarios
*/}}
{{- define "nginx-ingress-services.getCertificateSecretName" -}}
{{- $nameParts := list (include "nginx-ingress-services.fullname" .) -}}
{{- if .Values.tls.useCertManager -}}
    {{- $nameParts = append $nameParts "managed" -}}
{{- else -}}
    {{- $nameParts = append $nameParts "wildcard" -}}
{{- end -}}
{{- $nameParts = append $nameParts "tls-certificate" -}}
{{- join "-" $nameParts -}}
{{- end -}}

{{/*
Generate the secret name in a conistent way, since it's referred to in multiple places, while
at the same time being used for distinct scenarios
*/}}
{{- define "nginx-ingress-services.getCustomSolversSecretName" -}}
{{- $nameParts := list (include "nginx-ingress-services.fullname" .) -}}
{{- $nameParts = append $nameParts "cert-manager-custom-solvers" -}}
{{- join "-" $nameParts -}}
{{- end -}}

{{/*
Returns the Letsencrypt API server URL based on whether testMode is enabled or disabled
*/}}
{{- define "certificate-manager.apiServerURL" -}}
{{- $hostnameParts := list "acme" -}}
{{- if .Values.certManager.inTestMode -}}
    {{- $hostnameParts = append $hostnameParts "staging" -}}
{{- end -}}
{{- $hostnameParts = append $hostnameParts "v02" -}}
{{- join "-" $hostnameParts | printf "https://%s.api.letsencrypt.org/directory" -}}
{{- end -}}

{{/* Allow KubeVersion to be overridden. */}}
{{- define "kubeVersion" -}}
  {{- default .Capabilities.KubeVersion.Version .Values.kubeVersionOverride -}}
{{- end -}}

{{/* Get Ingress API Version */}}
{{- define "ingress.apiVersion" -}}
  {{- if and (.Capabilities.APIVersions.Has "networking.k8s.io/v1") (semverCompare ">= 1.19-0" (include "kubeVersion" .)) -}}
      {{- print "networking.k8s.io/v1" -}}
  {{- else if .Capabilities.APIVersions.Has "networking.k8s.io/v1beta1" -}}
    {{- print "networking.k8s.io/v1beta1" -}}
  {{- else -}}
    {{- print "extensions/v1beta1" -}}
  {{- end -}}
{{- end -}}

{{/* Check Ingress stability */}}
{{- define "ingress.isStable" -}}
  {{- eq (include "ingress.apiVersion" .) "networking.k8s.io/v1" -}}
{{- end -}}

{{/* Check Ingress supports pathType */}}
{{/* pathType was added to networking.k8s.io/v1beta1 in Kubernetes 1.18 */}}
{{- define "ingress.supportsPathType" -}}
  {{- or (eq (include "ingress.isStable" .) "true") (and (eq (include "ingress.apiVersion" .) "networking.k8s.io/v1beta1") (semverCompare ">= 1.18-0" (include "kubeVersion" .))) -}}
{{- end -}}
