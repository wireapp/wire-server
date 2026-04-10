
{{/* SHARED HELPERS */}}
{{/* Allow KubeVersion to be overridden. */}}
{{- define "kubeVersion" -}}
  {{- default .Capabilities.KubeVersion.Version .Values.kubeVersionOverride -}}
{{- end -}}
{{- define "includeSecurityContext" -}}
  {{- (semverCompare ">= 1.24-0" (include "kubeVersion" .)) -}}
{{- end -}}

{{- define "useCassandraTLS" -}}
{{ or (hasKey . "tlsCa") (hasKey . "tlsCaSecretRef") }}
{{- end -}}

{{/* GALLEY */}}
{{- define "galley.tlsSecretRef" -}}
{{- if .cassandra.tlsCaSecretRef -}}
{{ .cassandra.tlsCaSecretRef | toYaml }}
{{- else }}
{{- dict "name" "galley-cassandra" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}


{{/* BACKGROUND-WORKER */}}
{{- define "gundeckTlsSecretRef" -}}
{{- if .cassandra.tlsCaSecretRef -}}
{{ .cassandra.tlsCaSecretRef | toYaml }}
{{- else }}
{{- dict "name" "background-worker-cassandra-gundeck" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}

{{- define "brigTlsSecretRef" -}}
{{- if .cassandraBrig.tlsCaSecretRef -}}
{{ .cassandraBrig.tlsCaSecretRef | toYaml }}
{{- else }}
{{- dict "name" "background-worker-cassandra-brig" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}

{{- define "galleyTlsSecretRef" -}}
{{- if and .cassandraGalley .cassandraGalley.tlsCaSecretRef -}}
{{ .cassandraGalley.tlsCaSecretRef | toYaml }}
{{- else }}
{{- dict "name" "background-worker-cassandra-galley" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}

{{/* BRIG */}}
{{- define "brig.tlsSecretRef" -}}
{{- if .cassandra.tlsCaSecretRef -}}
{{ .cassandra.tlsCaSecretRef | toYaml }}
{{- else }}
{{- dict "name" "brig-cassandra" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}

{{- define "brig.configureElasticSearchCa" -}}
{{ or (hasKey .elasticsearch "tlsCa") (hasKey .elasticsearch "tlsCaSecretRef") }}
{{- end -}}

{{- define "brig.elasticsearchTlsSecretName" -}}
{{- if .elasticsearch.tlsCaSecretRef -}}
{{ .elasticsearch.tlsCaSecretRef.name }}
{{- else }}
{{- print "brig-elasticsearch-ca" -}}
{{- end -}}
{{- end -}}

{{- define "brig.elasticsearchTlsSecretKey" -}}
{{- if .elasticsearch.tlsCaSecretRef -}}
{{ .elasticsearch.tlsCaSecretRef.key }}
{{- else }}
{{- print "ca.pem" -}}
{{- end -}}
{{- end -}}

{{- define "brig.configureAdditionalElasticSearchCa" -}}
{{ or (hasKey .elasticsearch "additionalTlsCa") (hasKey .elasticsearch "additionalTlsCaSecretRef") }}
{{- end -}}

{{- define "brig.additionalElasticsearchTlsSecretName" -}}
{{- if .elasticsearch.additionalTlsCaSecretRef -}}
{{ .elasticsearch.additionalTlsCaSecretRef.name }}
{{- else }}
{{- print "brig-additional-elasticsearch-ca" -}}
{{- end -}}
{{- end -}}

{{- define "brig.additionalElasticsearchTlsSecretKey" -}}
{{- if .elasticsearch.additionalTlsCaSecretRef -}}
{{ .elasticsearch.additionalTlsCaSecretRef.key }}
{{- else }}
{{- print "ca.pem" -}}
{{- end -}}
{{- end -}}

{{/* CANNON */}}
{{- define "cannon.tlsSecretRef" -}}
{{- if .cassandra.tlsCaSecretRef -}}
{{ .cassandra.tlsCaSecretRef | toYaml }}
{{- else }}
{{- dict "name" "cannon-cassandra" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}

{{/* GUNDECK */}}
{{- define "gundeck.tlsSecretRef" -}}
{{- if .cassandra.tlsCaSecretRef -}}
{{ .cassandra.tlsCaSecretRef | toYaml }}
{{- else }}
{{- dict "name" "gundeck-cassandra" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}

{{- define "gundeck.configureRedisCa" -}}
{{ or (hasKey .redis "tlsCa") (hasKey .redis "tlsCaSecretRef") }}
{{- end -}}

{{- define "gundeck.redisTlsSecretName" -}}
{{- if .redis.tlsCaSecretRef -}}
{{ .redis.tlsCaSecretRef.name }}
{{- else }}
{{- print "gundeck-redis-ca" -}}
{{- end -}}
{{- end -}}

{{- define "gundeck.redisTlsSecretKey" -}}
{{- if .redis.tlsCaSecretRef -}}
{{ .redis.tlsCaSecretRef.key }}
{{- else }}
{{- print "ca.pem" -}}
{{- end -}}
{{- end -}}

{{- define "gundeck.configureAdditionalRedisCa" -}}
{{ and (hasKey . "redisAdditionalWrite") (or (hasKey .redis "additionalTlsCa") (hasKey .redis "additionalTlsCaSecretRef")) }}
{{- end -}}

{{- define "gundeck.additionalRedisTlsSecretName" -}}
{{- if .redis.additionalTlsCaSecretRef -}}
{{ .redis.additionalTlsCaSecretRef.name }}
{{- else }}
{{- print "gundeck-additional-redis-ca" -}}
{{- end -}}
{{- end -}}

{{- define "gundeck.additionalRedisTlsSecretKey" -}}
{{- if .redis.additionalTlsCaSecretRef -}}
{{ .redis.additionalTlsCaSecretRef.key }}
{{- else }}
{{- print "ca.pem" -}}
{{- end -}}
{{- end -}}

{{/* SPAR */}}
{{- define "spar.tlsSecretRef" -}}
{{- if .cassandra.tlsCaSecretRef -}}
{{ .cassandra.tlsCaSecretRef | toYaml }}
{{- else }}
{{- dict "name" "spar-cassandra" "key" "ca.pem" | toYaml -}}
{{- end -}}
{{- end -}}

{{/* GALLEY SETTINGS */}}
{{- define "wire-server.galley.settings" -}}
maxTeamSize: {{ .maxTeamSize }}
maxConvSize: {{ .maxConvSize }}
intraListing: {{ .intraListing }}
{{- if .maxFanoutSize }}
maxFanoutSize: {{ .maxFanoutSize }}
{{- end }}
{{- if .exposeInvitationURLsTeamAllowlist }}
exposeInvitationURLsTeamAllowlist: {{ toYaml .exposeInvitationURLsTeamAllowlist | nindent 8 }}
{{- end }}
{{- if .conversationCodeURI }}
conversationCodeURI: {{ .conversationCodeURI | quote }}
{{- else if .multiIngress }}
multiIngress: {{- toYaml .multiIngress | nindent 8 }}
{{- else }}
{{ fail "Either settings.conversationCodeURI or settings.multiIngress have to be set" }}
{{- end }}
{{- if (and .conversationCodeURI .multiIngress) }}
{{ fail "settings.conversationCodeURI and settings.multiIngress are mutually exclusive" }}
{{- end }}
{{- if hasKey . "httpPoolSize" }}
httpPoolSize: {{ .httpPoolSize }}
{{- end }}
{{- if hasKey . "federationDomain" }}
federationDomain: {{ .federationDomain }}
{{- end }}
{{- if .federationProtocols }}
federationProtocols: {{ .federationProtocols | toJson }}
{{- end }}
{{- if .concurrentDeletionEvents }}
concurrentDeletionEvents: {{ .concurrentDeletionEvents }}
{{- end }}
{{- if .deleteConvThrottleMillis }}
deleteConvThrottleMillis: {{ .deleteConvThrottleMillis }}
{{- end }}
{{- if hasKey . "disabledAPIVersions" }}
disabledAPIVersions: {{ toJson .disabledAPIVersions }}
{{- end }}
{{- if .guestLinkTTLSeconds }}
guestLinkTTLSeconds: {{ .guestLinkTTLSeconds }}
{{- end }}
passwordHashingOptions: {{ toYaml .passwordHashingOptions | nindent 8 }}
passwordHashingRateLimit: {{ toYaml .passwordHashingRateLimit | nindent 8 }}
{{- if .checkGroupInfo }}
checkGroupInfo: {{ .checkGroupInfo }}
{{- end }}
{{- if hasKey . "meetings" }}
meetings:
  {{- toYaml .meetings | nindent 8 }}
{{- end }}
{{- if .featureFlags }}
featureFlags:
  sso: {{ .featureFlags.sso }}
  legalhold: {{ .featureFlags.legalhold }}
  teamSearchVisibility: {{ .featureFlags.teamSearchVisibility }}
  classifiedDomains:
    {{- toYaml .featureFlags.classifiedDomains | nindent 10 }}
  {{- if .featureFlags.fileSharing }}
  fileSharing:
    {{- toYaml .featureFlags.fileSharing | nindent 10 }}
  {{- end }}
  {{- if .featureFlags.enforceFileDownloadLocation }}
  enforceFileDownloadLocation:
    {{- toYaml .featureFlags.enforceFileDownloadLocation | nindent 10 }}
  {{- end }}
  {{- if .featureFlags.sndFactorPasswordChallenge }}
  sndFactorPasswordChallenge:
    {{- toYaml .featureFlags.sndFactorPasswordChallenge | nindent 10 }}
  {{- end }}
  {{- if .featureFlags.searchVisibilityInbound }}
  searchVisibilityInbound:
    {{- toYaml .featureFlags.searchVisibilityInbound | nindent 10 }}
  {{- end }}
  {{- /* Accept the legacy typo in Helm values, but always render the canonical Galley key. */}}
  {{- $validateSAMLemails := .featureFlags.validateSAMLemails | default .featureFlags.validateSAMLEmails }}
  {{- if $validateSAMLemails }}
  validateSAMLemails:
    {{- toYaml $validateSAMLemails | nindent 10 }}
  {{- end }}
  {{- if .featureFlags.appLock }}
  appLock:
    {{- toYaml .featureFlags.appLock | nindent 10 }}
  {{- end }}
  {{- if .featureFlags.conferenceCalling }}
  conferenceCalling:
    {{- toYaml .featureFlags.conferenceCalling | nindent 10 }}
  {{- end }}
  {{- if .featureFlags.selfDeletingMessages }}
  selfDeletingMessages:
    {{- toYaml .featureFlags.selfDeletingMessages | nindent 10 }}
  {{- end }}
  {{- if .featureFlags.conversationGuestLinks }}
  conversationGuestLinks:
    {{- toYaml .featureFlags.conversationGuestLinks | nindent 10 }}
  {{- end }}
  {{- if .featureFlags.mls }}
  mls:
    {{- toYaml .featureFlags.mls | nindent 10 }}
  {{- end }}
  {{- if .featureFlags.outlookCalIntegration }}
  outlookCalIntegration:
    {{- toYaml .featureFlags.outlookCalIntegration | nindent 10 }}
  {{- end }}
  {{- if .featureFlags.mlsE2EId }}
  mlsE2EId:
    {{- toYaml .featureFlags.mlsE2EId | nindent 10 }}
  {{- end }}
  {{- if .featureFlags.mlsMigration }}
  mlsMigration:
    {{- toYaml .featureFlags.mlsMigration | nindent 10 }}
  {{- end }}
  {{- if .featureFlags.limitedEventFanout }}
  limitedEventFanout:
    {{- toYaml .featureFlags.limitedEventFanout | nindent 10 }}
  {{- end }}
  {{- if .featureFlags.domainRegistration }}
  domainRegistration:
    {{- toYaml .featureFlags.domainRegistration | nindent 10 }}
  {{- end }}
  {{- if .featureFlags.channels }}
  channels:
    {{- toYaml .featureFlags.channels | nindent 10 }}
  {{- end }}
  {{- if .featureFlags.cells }}
  cells:
    {{- toYaml .featureFlags.cells | nindent 10 }}
  {{- end }}
  {{- if .featureFlags.cellsInternal }}
  cellsInternal:
    {{- toYaml .featureFlags.cellsInternal | nindent 10 }}
  {{- end }}
  {{- if .featureFlags.allowedGlobalOperations }}
  allowedGlobalOperations:
    {{- toYaml .featureFlags.allowedGlobalOperations | nindent 10 }}
  {{- end }}
  {{- if .featureFlags.assetAuditLog }}
  assetAuditLog:
    {{- toYaml .featureFlags.assetAuditLog | nindent 10 }}
  {{- end }}
  {{- if .featureFlags.consumableNotifications }}
  consumableNotifications:
    {{- toYaml .featureFlags.consumableNotifications | nindent 10 }}
  {{- end }}
  {{- if .featureFlags.chatBubbles }}
  chatBubbles:
    {{- toYaml .featureFlags.chatBubbles | nindent 10 }}
  {{- end }}
  {{- if .featureFlags.apps }}
  apps:
    {{- toYaml .featureFlags.apps | nindent 10 }}
  {{- end }}
  {{- if .featureFlags.simplifiedUserConnectionRequestQRCode }}
  simplifiedUserConnectionRequestQRCode:
    {{- toYaml .featureFlags.simplifiedUserConnectionRequestQRCode | nindent 10 }}
  {{- end }}
  {{- if .featureFlags.stealthUsers }}
  stealthUsers:
    {{- toYaml .featureFlags.stealthUsers | nindent 10 }}
  {{- end }}
  {{- if .featureFlags.meetings }}
  meetings:
    {{- toYaml .featureFlags.meetings | nindent 10 }}
  {{- end }}
  {{- if .featureFlags.meetingsPremium }}
  meetingsPremium:
    {{- toYaml .featureFlags.meetingsPremium | nindent 10 }}
  {{- end }}
{{- end }}
{{- end -}}

{{/* Compute the SCIM base URI

The rules are:
- If `scimBaseUri` is defined, take that value
- Otherwise, if `ssoUri` is defined, take the value and drop a possible last
  /sso path element
- Otherwise, fail

In multi-ingress setups you have to configure the `scimBaseUri`, because it
cannot be decided which `ssoUri` to take from the map.
*/}}
{{- define "spar.computeScimBaseUri" -}}
{{- if .scimBaseUri -}}
  {{- .scimBaseUri -}}
{{- else if .ssoUri -}}
  {{- $parts := splitList "/" .ssoUri -}}
  {{- if eq (last $parts) "sso" -}}
    {{- $baseUri := $parts | reverse | rest | reverse | join "/" -}}
    {{- $baseUri -}}/scim/v2
  {{- else -}}
    {{- .ssoUri -}}/scim/v2
  {{- end -}}
{{- else -}}
  {{- fail "Either scimBaseUri or ssoUri must be defined" -}}
{{- end -}}
{{- end -}}
