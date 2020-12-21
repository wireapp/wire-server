{{/*
override default fullname template to remove the .Release.Name from the definition in
https://github.com/kubernetes/charts/blob/master/stable/redis-ha/templates/_helpers.tpl
*/}}
{{- define "cassandra.fullname" -}}
{{- $name := default .Chart.Name .Values.nameOverride -}}
{{- printf "%s" $name | trunc 63 | trimSuffix "-" -}}
{{- end -}}