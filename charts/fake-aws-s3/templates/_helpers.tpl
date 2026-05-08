{{/* vim: set filetype=mustache: */}}
{{- define "fake-aws-s3.name" -}}
{{- default .Chart.Name .Values.nameOverride | trunc 53 | trimSuffix "-" -}}
{{- end -}}

{{- define "fake-aws-s3.fullname" -}}
{{- default .Chart.Name .Values.fullnameOverride | trunc 53 | trimSuffix "-" -}}
{{- end -}}
