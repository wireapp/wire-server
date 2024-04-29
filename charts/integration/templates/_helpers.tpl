
{{/* Allow KubeVersion to be overridden. */}}
{{- define "kubeVersion" -}}
  {{- default $.Capabilities.KubeVersion.Version $.Values.kubeVersionOverride -}}
{{- end -}}

{{- define "includeSecurityContext" -}}
  {{- (semverCompare ">= 1.24-0" (include "kubeVersion" .)) -}}
{{- end -}}

{{/* Get Ingress API Version */}}
{{- define "ingress.apiVersion" -}}
  {{- if and ($.Capabilities.APIVersions.Has "networking.k8s.io/v1") (semverCompare ">= 1.19-0" (include "kubeVersion" .)) -}}
      {{- print "networking.k8s.io/v1" -}}
  {{- else if $.Capabilities.APIVersions.Has "networking.k8s.io/v1beta1" -}}
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

{{- define "ingress.FieldNotAnnotation" -}}
  {{- (semverCompare ">= 1.27-0" (include "kubeVersion" .)) -}}
{{- end -}}

{{- define "integrationTestHelperNewLabels" -}}
  {{- (semverCompare ">= 1.23-0" (include "kubeVersion" .)) -}}
{{- end -}}

{{- define "useCassandraTLS" -}}
{{ or (hasKey .cassandra "tlsCa") (hasKey .cassandra "tlsCaSecretRef") }}
{{- end -}}

{{- define "cassandraTlsSecretName" -}}
{{- if .cassandra.tlsCaSecretRef -}}
{{ .cassandra.tlsCaSecretRef.name }}
{{- else }}
{{- print "integration-cassandra" -}}
{{- end -}}
{{- end -}}

{{- define "cassandraTlsSecretKey" -}}
{{- if .cassandra.tlsCaSecretRef -}}
{{ .cassandra.tlsCaSecretRef.key }}
{{- else }}
{{- print "ca.pem" -}}
{{- end -}}
{{- end -}}
