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

module Galley.API.Public.Servant (mkGalleyAPI, servantSitemap, GalleyHandler (..)) where

import Data.Domain
import Galley.API.Create
import Galley.API.CustomBackend
import Galley.API.LegalHold
import Galley.API.MLS
import Galley.API.MLS.GroupInfo
import Galley.API.Query
import Galley.API.Teams
import Galley.API.Teams.Features
import Galley.API.Update
import Galley.App
import Galley.Cassandra.TeamFeatures
import Imports
import Polysemy
import Polysemy.Internal
import Servant
import Wire.API.Error
import Wire.API.Routes.API
import Wire.API.Routes.Named
import Wire.API.Routes.Public.Galley
import Wire.API.Team.Feature

newtype GalleyHandler a = GalleyHandler {unGalleyHandler :: ReaderT Env Handler a}

type GalleyAPI api = APIH api GalleyHandler

instance r ~ GalleyEffects => HandlerEffects GalleyHandler r where
  interpretHandlerEffects = error "TODO: later"

mkGalleyAPI ::
  forall name api.
  ( HasServer api '[Domain],
    ServerEffects (DeclaredErrorEffects api) GalleyEffects
  ) =>
  ServerT api (Sem (Append (DeclaredErrorEffects api) GalleyEffects)) ->
  GalleyAPI (Named name api)
mkGalleyAPI = mkNamedAPI' @name @GalleyEffects @api

servantSitemap :: GalleyAPI ServantAPI
servantSitemap =
  conversations
    <@+> teamConversations
    <@+> messaging
    <@+> bot
    <@+> team
    <@+> features
    <@+> mls
    <@+> customBackend
    <@+> legalHold
    <@+> teamMember
  where
    conversations =
      mkGalleyAPI @"get-unqualified-conversation" getUnqualifiedConversation
        <@+> mkGalleyAPI @"get-unqualified-conversation-legalhold-alias" getUnqualifiedConversation
        <@+> mkGalleyAPI @"get-conversation" getConversation
        <@+> mkGalleyAPI @"get-conversation-roles" getConversationRoles
        <@+> mkGalleyAPI @"get-group-info" getGroupInfo
        <@+> mkGalleyAPI @"list-conversation-ids-unqualified" conversationIdsPageFromUnqualified
        <@+> mkGalleyAPI @"list-conversation-ids" conversationIdsPageFrom
        <@+> mkGalleyAPI @"get-conversations" getConversations
        <@+> mkGalleyAPI @"list-conversations-v1" listConversations
        <@+> mkGalleyAPI @"list-conversations" listConversations
        <@+> mkGalleyAPI @"get-conversation-by-reusable-code" (getConversationByReusableCode @Cassandra)
        <@+> mkGalleyAPI @"create-group-conversation" createGroupConversation
        <@+> mkGalleyAPI @"create-self-conversation" createSelfConversation
        <@+> mkGalleyAPI @"create-one-to-one-conversation" createOne2OneConversation
        <@+> mkGalleyAPI @"add-members-to-conversation-unqualified" addMembersUnqualified
        <@+> mkGalleyAPI @"add-members-to-conversation-unqualified2" addMembersUnqualifiedV2
        <@+> mkGalleyAPI @"add-members-to-conversation" addMembers
        <@+> mkGalleyAPI @"join-conversation-by-id-unqualified" (joinConversationById @Cassandra)
        <@+> mkGalleyAPI @"join-conversation-by-code-unqualified" (joinConversationByReusableCode @Cassandra)
        <@+> mkGalleyAPI @"code-check" (checkReusableCode @Cassandra)
        <@+> mkGalleyAPI @"create-conversation-code-unqualified" (addCodeUnqualified @Cassandra)
        <@+> mkGalleyAPI @"get-conversation-guest-links-status" (getConversationGuestLinksStatus @Cassandra)
        <@+> mkGalleyAPI @"remove-code-unqualified" rmCodeUnqualified
        <@+> mkGalleyAPI @"get-code" (getCode @Cassandra)
        <@+> mkGalleyAPI @"member-typing-unqualified" isTypingUnqualified
        <@+> mkGalleyAPI @"remove-member-unqualified" removeMemberUnqualified
        <@+> mkGalleyAPI @"remove-member" removeMemberQualified
        <@+> mkGalleyAPI @"update-other-member-unqualified" updateOtherMemberUnqualified
        <@+> mkGalleyAPI @"update-other-member" updateOtherMember
        <@+> mkGalleyAPI @"update-conversation-name-deprecated" updateUnqualifiedConversationName
        <@+> mkGalleyAPI @"update-conversation-name-unqualified" updateUnqualifiedConversationName
        <@+> mkGalleyAPI @"update-conversation-name" updateConversationName
        <@+> mkGalleyAPI @"update-conversation-message-timer-unqualified" updateConversationMessageTimerUnqualified
        <@+> mkGalleyAPI @"update-conversation-message-timer" updateConversationMessageTimer
        <@+> mkGalleyAPI @"update-conversation-receipt-mode-unqualified" updateConversationReceiptModeUnqualified
        <@+> mkGalleyAPI @"update-conversation-receipt-mode" updateConversationReceiptMode
        <@+> mkGalleyAPI @"update-conversation-access-unqualified" updateConversationAccessUnqualified
        <@+> mkGalleyAPI @"update-conversation-access" updateConversationAccess
        <@+> mkGalleyAPI @"get-conversation-self-unqualified" getLocalSelf
        <@+> mkGalleyAPI @"update-conversation-self-unqualified" updateUnqualifiedSelfMember
        <@+> mkGalleyAPI @"update-conversation-self" updateSelfMember

    teamConversations :: GalleyAPI TeamConversationAPI
    teamConversations =
      mkGalleyAPI @"get-team-conversation-roles" getTeamConversationRoles
        <@+> mkGalleyAPI @"get-team-conversations" getTeamConversations
        <@+> mkGalleyAPI @"get-team-conversation" getTeamConversation
        <@+> mkGalleyAPI @"delete-team-conversation" deleteTeamConversation

    messaging :: GalleyAPI MessagingAPI
    messaging =
      mkGalleyAPI @"post-otr-message-unqualified" postOtrMessageUnqualified
        <@+> mkGalleyAPI @"post-otr-broadcast-unqualified" postOtrBroadcastUnqualified
        <@+> mkGalleyAPI @"post-proteus-message" postProteusMessage
        <@+> mkGalleyAPI @"post-proteus-broadcast" postProteusBroadcast

    bot :: GalleyAPI BotAPI
    bot = mkGalleyAPI @"post-bot-message-unqualified" postBotMessageUnqualified

    team =
      mkGalleyAPI @"create-non-binding-team" createNonBindingTeamH
        <@+> mkGalleyAPI @"update-team" updateTeamH
        <@+> mkGalleyAPI @"get-teams" getManyTeams
        <@+> mkGalleyAPI @"get-team" getTeamH
        <@+> mkGalleyAPI @"delete-team" deleteTeam

    features :: GalleyAPI FeatureAPI
    features =
      mkGalleyAPI @'("get", SSOConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("get", LegalholdConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("put", LegalholdConfig) (setFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("get", SearchVisibilityAvailableConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("put", SearchVisibilityAvailableConfig) (setFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("get-deprecated", SearchVisibilityAvailableConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("put-deprecated", SearchVisibilityAvailableConfig) (setFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @"get-search-visibility" getSearchVisibility
        <@+> mkGalleyAPI @"set-search-visibility" (setSearchVisibility @Cassandra (featureEnabledForTeam @Cassandra @SearchVisibilityAvailableConfig))
        <@+> mkGalleyAPI @'("get", ValidateSAMLEmailsConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("get-deprecated", ValidateSAMLEmailsConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("get", DigitalSignaturesConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("get-deprecated", DigitalSignaturesConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("get", AppLockConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("put", AppLockConfig) (setFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("get", FileSharingConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("put", FileSharingConfig) (setFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("get", ClassifiedDomainsConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("get", ConferenceCallingConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("get", SelfDeletingMessagesConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("put", SelfDeletingMessagesConfig) (setFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("get", GuestLinksConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("put", GuestLinksConfig) (setFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("get", SndFactorPasswordChallengeConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("put", SndFactorPasswordChallengeConfig) (setFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("get", MLSConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("put", MLSConfig) (setFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("get", ExposeInvitationURLsToTeamAdminConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("put", ExposeInvitationURLsToTeamAdminConfig) (setFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("get", SearchVisibilityInboundConfig) (getFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @'("put", SearchVisibilityInboundConfig) (setFeatureStatus @Cassandra . DoAuth)
        <@+> mkGalleyAPI @"get-all-feature-configs-for-user" (getAllFeatureConfigsForUser @Cassandra)
        <@+> mkGalleyAPI @"get-all-feature-configs-for-team" (getAllFeatureConfigsForTeam @Cassandra)
        <@+> mkGalleyAPI @'("get-config", LegalholdConfig) (getFeatureStatusForUser @Cassandra)
        <@+> mkGalleyAPI @'("get-config", SSOConfig) (getFeatureStatusForUser @Cassandra)
        <@+> mkGalleyAPI @'("get-config", SearchVisibilityAvailableConfig) (getFeatureStatusForUser @Cassandra)
        <@+> mkGalleyAPI @'("get-config", ValidateSAMLEmailsConfig) (getFeatureStatusForUser @Cassandra)
        <@+> mkGalleyAPI @'("get-config", DigitalSignaturesConfig) (getFeatureStatusForUser @Cassandra)
        <@+> mkGalleyAPI @'("get-config", AppLockConfig) (getFeatureStatusForUser @Cassandra)
        <@+> mkGalleyAPI @'("get-config", FileSharingConfig) (getFeatureStatusForUser @Cassandra)
        <@+> mkGalleyAPI @'("get-config", ClassifiedDomainsConfig) (getFeatureStatusForUser @Cassandra)
        <@+> mkGalleyAPI @'("get-config", ConferenceCallingConfig) (getFeatureStatusForUser @Cassandra)
        <@+> mkGalleyAPI @'("get-config", SelfDeletingMessagesConfig) (getFeatureStatusForUser @Cassandra)
        <@+> mkGalleyAPI @'("get-config", GuestLinksConfig) (getFeatureStatusForUser @Cassandra)
        <@+> mkGalleyAPI @'("get-config", SndFactorPasswordChallengeConfig) (getFeatureStatusForUser @Cassandra)
        <@+> mkGalleyAPI @'("get-config", MLSConfig) (getFeatureStatusForUser @Cassandra)

    mls :: GalleyAPI MLSAPI
    mls =
      mkGalleyAPI @"mls-welcome-message" postMLSWelcomeFromLocalUser
        <@+> mkGalleyAPI @"mls-message-v1" postMLSMessageFromLocalUserV1
        <@+> mkGalleyAPI @"mls-message" postMLSMessageFromLocalUser
        <@+> mkGalleyAPI @"mls-commit-bundle" postMLSCommitBundleFromLocalUser
        <@+> mkGalleyAPI @"mls-public-keys" getMLSPublicKeys

    customBackend :: GalleyAPI CustomBackendAPI
    customBackend = mkGalleyAPI @"get-custom-backend-by-domain" getCustomBackendByDomain

    legalHold :: GalleyAPI LegalHoldAPI
    legalHold =
      mkGalleyAPI @"create-legal-hold-settings" (createSettings @Cassandra)
        <@+> mkGalleyAPI @"get-legal-hold-settings" (getSettings @Cassandra)
        <@+> mkGalleyAPI @"delete-legal-hold-settings" (removeSettingsInternalPaging @Cassandra)
        <@+> mkGalleyAPI @"get-legal-hold" getUserStatus
        <@+> mkGalleyAPI @"consent-to-legal-hold" grantConsent
        <@+> mkGalleyAPI @"request-legal-hold-device" (requestDevice @Cassandra)
        <@+> mkGalleyAPI @"disable-legal-hold-for-user" disableForUser
        <@+> mkGalleyAPI @"approve-legal-hold-device" (approveDevice @Cassandra)

    teamMember :: GalleyAPI TeamMemberAPI
    teamMember =
      mkGalleyAPI @"get-team-members" getTeamMembers
        <@+> mkGalleyAPI @"get-team-member" getTeamMember
        <@+> mkGalleyAPI @"get-team-members-by-ids" bulkGetTeamMembers
        <@+> mkGalleyAPI @"add-team-member" (addTeamMember @Cassandra)
        <@+> mkGalleyAPI @"delete-team-member" deleteTeamMember
        <@+> mkGalleyAPI @"delete-non-binding-team-member" deleteNonBindingTeamMember
        <@+> mkGalleyAPI @"update-team-member" updateTeamMember
        <@+> mkGalleyAPI @"get-team-members-csv" getTeamMembersCSV
