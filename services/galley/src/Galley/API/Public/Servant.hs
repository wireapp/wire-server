-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Galley.API.Public.Servant (mkNamedAPI, servantSitemap) where

import Galley.API.Create
import Galley.API.CustomBackend
import Galley.API.LegalHold
import Galley.API.MLS
import Galley.API.Query
import Galley.API.Teams
import Galley.API.Teams.Features
import Galley.API.Update
import Galley.App
import Galley.Cassandra.TeamFeatures
import Imports
import Wire.API.Routes.API
import Wire.API.Routes.Public.Galley
import Wire.API.Team.Feature

servantSitemap :: API ServantAPI GalleyEffects
servantSitemap =
  conversationsV1
    <@> conversations
    <@> teamConversations
    <@> messaging
    <@> bot
    <@> team
    <@> features
    <@> mls
    <@> customBackend
    <@> legalHold
    <@> teamMember
  where
    conversationsV1 =
      mkNamedAPI @"get-unqualified-conversation" getUnqualifiedConversation
        <@> mkNamedAPI @"get-unqualified-conversation-legalhold-alias" getUnqualifiedConversation
        <@> mkNamedAPI @"create-group-conversation-v2" createGroupConversation
        <@> mkNamedAPI @"create-self-conversation-v2" createSelfConversation
        <@> mkNamedAPI @"create-one-to-one-conversation-v2" createOne2OneConversation
        <@> mkNamedAPI @"list-conversations-v1" listConversations
        <@> mkNamedAPI @"list-conversation-ids-unqualified" conversationIdsPageFromUnqualified
        <@> mkNamedAPI @"add-members-to-conversation-unqualified" addMembersUnqualified
        <@> mkNamedAPI @"add-members-to-conversation-unqualified2" addMembersUnqualifiedV2
        <@> mkNamedAPI @"update-conversation-name-deprecated" updateUnqualifiedConversationName
        <@> mkNamedAPI @"update-conversation-name-unqualified" updateUnqualifiedConversationName
        <@> mkNamedAPI @"update-other-member-unqualified" updateOtherMemberUnqualified
    conversations =
      mkNamedAPI @"get-conversation" getConversation
        <@> mkNamedAPI @"get-conversation-roles" getConversationRoles
        <@> mkNamedAPI @"list-conversation-ids" conversationIdsPageFrom
        <@> mkNamedAPI @"get-conversations" getConversations
        <@> mkNamedAPI @"list-conversations" listConversations
        <@> mkNamedAPI @"get-conversation-by-reusable-code" (getConversationByReusableCode @Cassandra)
        -- <@> mkNamedAPI @"create-group-conversation" createGroupConversation
        -- <@> mkNamedAPI @"create-self-conversation" createSelfConversation
        -- <@> mkNamedAPI @"create-one-to-one-conversation" createOne2OneConversation
        <@> mkNamedAPI @"add-members-to-conversation" addMembers
        <@> mkNamedAPI @"join-conversation-by-id-unqualified" (joinConversationById @Cassandra)
        <@> mkNamedAPI @"join-conversation-by-code-unqualified" (joinConversationByReusableCode @Cassandra)
        <@> mkNamedAPI @"code-check" (checkReusableCode @Cassandra)
        <@> mkNamedAPI @"create-conversation-code-unqualified" (addCodeUnqualified @Cassandra)
        <@> mkNamedAPI @"get-conversation-guest-links-status" (getConversationGuestLinksStatus @Cassandra)
        <@> mkNamedAPI @"remove-code-unqualified" rmCodeUnqualified
        <@> mkNamedAPI @"get-code" (getCode @Cassandra)
        <@> mkNamedAPI @"member-typing-unqualified" isTypingUnqualified
        <@> mkNamedAPI @"remove-member-unqualified" removeMemberUnqualified
        <@> mkNamedAPI @"remove-member" removeMemberQualified
        <@> mkNamedAPI @"update-other-member" updateOtherMember
        <@> mkNamedAPI @"update-conversation-name" updateConversationName
        <@> mkNamedAPI @"update-conversation-message-timer-unqualified" updateConversationMessageTimerUnqualified
        <@> mkNamedAPI @"update-conversation-message-timer" updateConversationMessageTimer
        <@> mkNamedAPI @"update-conversation-receipt-mode-unqualified" updateConversationReceiptModeUnqualified
        <@> mkNamedAPI @"update-conversation-receipt-mode" updateConversationReceiptMode
        <@> mkNamedAPI @"update-conversation-access-unqualified" updateConversationAccessUnqualified
        <@> mkNamedAPI @"update-conversation-access" updateConversationAccess
        <@> mkNamedAPI @"get-conversation-self-unqualified" getLocalSelf
        <@> mkNamedAPI @"update-conversation-self-unqualified" updateUnqualifiedSelfMember
        <@> mkNamedAPI @"update-conversation-self" updateSelfMember

    teamConversations :: API TeamConversationAPI GalleyEffects
    teamConversations =
      mkNamedAPI @"get-team-conversation-roles" getTeamConversationRoles
        <@> mkNamedAPI @"get-team-conversations" getTeamConversations
        <@> mkNamedAPI @"get-team-conversation" getTeamConversation
        <@> mkNamedAPI @"delete-team-conversation" deleteTeamConversation

    messaging :: API MessagingAPI GalleyEffects
    messaging =
      mkNamedAPI @"post-otr-message-unqualified" postOtrMessageUnqualified
        <@> mkNamedAPI @"post-otr-broadcast-unqualified" postOtrBroadcastUnqualified
        <@> mkNamedAPI @"post-proteus-message" postProteusMessage
        <@> mkNamedAPI @"post-proteus-broadcast" postProteusBroadcast

    bot :: API BotAPI GalleyEffects
    bot = mkNamedAPI @"post-bot-message-unqualified" postBotMessageUnqualified

    team =
      mkNamedAPI @"create-non-binding-team" createNonBindingTeamH
        <@> mkNamedAPI @"update-team" updateTeamH
        <@> mkNamedAPI @"get-teams" getManyTeams
        <@> mkNamedAPI @"get-team" getTeamH
        <@> mkNamedAPI @"delete-team" deleteTeam

    features :: API FeatureAPI GalleyEffects
    features =
      mkNamedAPI @'("get", SSOConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("get", LegalholdConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("put", LegalholdConfig) (setFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("get", SearchVisibilityAvailableConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("put", SearchVisibilityAvailableConfig) (setFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("get-deprecated", SearchVisibilityAvailableConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("put-deprecated", SearchVisibilityAvailableConfig) (setFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @"get-search-visibility" getSearchVisibility
        <@> mkNamedAPI @"set-search-visibility" (setSearchVisibility @Cassandra (featureEnabledForTeam @Cassandra @SearchVisibilityAvailableConfig))
        <@> mkNamedAPI @'("get", ValidateSAMLEmailsConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("get-deprecated", ValidateSAMLEmailsConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("get", DigitalSignaturesConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("get-deprecated", DigitalSignaturesConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("get", AppLockConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("put", AppLockConfig) (setFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("get", FileSharingConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("put", FileSharingConfig) (setFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("get", ClassifiedDomainsConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("get", ConferenceCallingConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("get", SelfDeletingMessagesConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("put", SelfDeletingMessagesConfig) (setFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("get", GuestLinksConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("put", GuestLinksConfig) (setFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("get", SndFactorPasswordChallengeConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("put", SndFactorPasswordChallengeConfig) (setFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("get", MLSConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("put", MLSConfig) (setFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("get", ExposeInvitationURLsToTeamAdminConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("put", ExposeInvitationURLsToTeamAdminConfig) (setFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("get", SearchVisibilityInboundConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @'("put", SearchVisibilityInboundConfig) (setFeatureStatus @Cassandra . DoAuth)
        <@> mkNamedAPI @"get-all-feature-configs-for-user" (getAllFeatureConfigsForUser @Cassandra)
        <@> mkNamedAPI @"get-all-feature-configs-for-team" (getAllFeatureConfigsForTeam @Cassandra)
        <@> mkNamedAPI @'("get-config", LegalholdConfig) (getFeatureStatusForUser @Cassandra)
        <@> mkNamedAPI @'("get-config", SSOConfig) (getFeatureStatusForUser @Cassandra)
        <@> mkNamedAPI @'("get-config", SearchVisibilityAvailableConfig) (getFeatureStatusForUser @Cassandra)
        <@> mkNamedAPI @'("get-config", ValidateSAMLEmailsConfig) (getFeatureStatusForUser @Cassandra)
        <@> mkNamedAPI @'("get-config", DigitalSignaturesConfig) (getFeatureStatusForUser @Cassandra)
        <@> mkNamedAPI @'("get-config", AppLockConfig) (getFeatureStatusForUser @Cassandra)
        <@> mkNamedAPI @'("get-config", FileSharingConfig) (getFeatureStatusForUser @Cassandra)
        <@> mkNamedAPI @'("get-config", ClassifiedDomainsConfig) (getFeatureStatusForUser @Cassandra)
        <@> mkNamedAPI @'("get-config", ConferenceCallingConfig) (getFeatureStatusForUser @Cassandra)
        <@> mkNamedAPI @'("get-config", SelfDeletingMessagesConfig) (getFeatureStatusForUser @Cassandra)
        <@> mkNamedAPI @'("get-config", GuestLinksConfig) (getFeatureStatusForUser @Cassandra)
        <@> mkNamedAPI @'("get-config", SndFactorPasswordChallengeConfig) (getFeatureStatusForUser @Cassandra)
        <@> mkNamedAPI @'("get-config", MLSConfig) (getFeatureStatusForUser @Cassandra)

    mls :: API MLSAPI GalleyEffects
    mls =
      mkNamedAPI @"mls-welcome-message" postMLSWelcomeFromLocalUser
        <@> mkNamedAPI @"mls-message-v1" postMLSMessageFromLocalUserV1
        <@> mkNamedAPI @"mls-message" postMLSMessageFromLocalUser
        <@> mkNamedAPI @"mls-commit-bundle" postMLSCommitBundleFromLocalUser
        <@> mkNamedAPI @"mls-public-keys" getMLSPublicKeys

    customBackend :: API CustomBackendAPI GalleyEffects
    customBackend = mkNamedAPI @"get-custom-backend-by-domain" getCustomBackendByDomain

    legalHold :: API LegalHoldAPI GalleyEffects
    legalHold =
      mkNamedAPI @"create-legal-hold-settings" (createSettings @Cassandra)
        <@> mkNamedAPI @"get-legal-hold-settings" (getSettings @Cassandra)
        <@> mkNamedAPI @"delete-legal-hold-settings" (removeSettingsInternalPaging @Cassandra)
        <@> mkNamedAPI @"get-legal-hold" getUserStatus
        <@> mkNamedAPI @"consent-to-legal-hold" grantConsent
        <@> mkNamedAPI @"request-legal-hold-device" (requestDevice @Cassandra)
        <@> mkNamedAPI @"disable-legal-hold-for-user" disableForUser
        <@> mkNamedAPI @"approve-legal-hold-device" (approveDevice @Cassandra)

    teamMember :: API TeamMemberAPI GalleyEffects
    teamMember =
      mkNamedAPI @"get-team-members" getTeamMembers
        <@> mkNamedAPI @"get-team-member" getTeamMember
        <@> mkNamedAPI @"get-team-members-by-ids" bulkGetTeamMembers
        <@> mkNamedAPI @"add-team-member" (addTeamMember @Cassandra)
        <@> mkNamedAPI @"delete-team-member" deleteTeamMember
        <@> mkNamedAPI @"delete-non-binding-team-member" deleteNonBindingTeamMember
        <@> mkNamedAPI @"update-team-member" updateTeamMember
        <@> mkNamedAPI @"get-team-members-csv" getTeamMembersCSV
