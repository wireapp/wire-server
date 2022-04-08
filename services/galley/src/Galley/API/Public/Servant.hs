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
import Galley.API.MLS
import Galley.API.Query
import Galley.API.Teams
import Galley.API.Teams.Features
import Galley.API.Update
import Galley.App
import Galley.Cassandra.Paging
import Imports
import Wire.API.Routes.API
import Wire.API.Routes.Public.Galley
import Wire.API.Team.Feature

servantSitemap :: API ServantAPI GalleyEffects
servantSitemap =
  conversations
    <@> teamConversations
    <@> messaging
    <@> bot
    <@> team
    <@> features
    <@> mls
  where
    conversations =
      mkNamedAPI @"get-unqualified-conversation" getUnqualifiedConversation
        <@> mkNamedAPI @"get-conversation" getConversation
        <@> mkNamedAPI @"get-conversation-roles" getConversationRoles
        <@> mkNamedAPI @"list-conversation-ids-unqualified" conversationIdsPageFromUnqualified
        <@> mkNamedAPI @"list-conversation-ids" conversationIdsPageFrom
        <@> mkNamedAPI @"get-conversations" getConversations
        <@> mkNamedAPI @"list-conversations" listConversations
        <@> mkNamedAPI @"get-conversation-by-reusable-code" getConversationByReusableCode
        <@> mkNamedAPI @"create-group-conversation" createGroupConversation
        <@> mkNamedAPI @"create-self-conversation" createSelfConversation
        <@> mkNamedAPI @"create-one-to-one-conversation" createOne2OneConversation
        <@> mkNamedAPI @"add-members-to-conversation-unqualified" addMembersUnqualified
        <@> mkNamedAPI @"add-members-to-conversation" addMembers
        <@> mkNamedAPI @"join-conversation-by-id-unqualified" joinConversationById
        <@> mkNamedAPI @"join-conversation-by-code-unqualified" joinConversationByReusableCode
        <@> mkNamedAPI @"code-check" checkReusableCode
        <@> mkNamedAPI @"create-conversation-code-unqualified" addCodeUnqualified
        <@> mkNamedAPI @"get-conversation-guest-links-status" getConversationGuestLinksStatus
        <@> mkNamedAPI @"remove-code-unqualified" rmCodeUnqualified
        <@> mkNamedAPI @"get-code" getCode
        <@> mkNamedAPI @"member-typing-unqualified" isTypingUnqualified
        <@> mkNamedAPI @"remove-member-unqualified" removeMemberUnqualified
        <@> mkNamedAPI @"remove-member" removeMemberQualified
        <@> mkNamedAPI @"update-other-member-unqualified" updateOtherMemberUnqualified
        <@> mkNamedAPI @"update-other-member" updateOtherMember
        <@> mkNamedAPI @"update-conversation-name-deprecated" updateUnqualifiedConversationName
        <@> mkNamedAPI @"update-conversation-name-unqualified" updateUnqualifiedConversationName
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

    features =
      mkNamedAPI @'("get", 'TeamFeatureSSO)
        ( getFeatureStatus @'WithoutLockStatus @'TeamFeatureSSO
            getSSOStatusInternal
            . DoAuth
        )
        <@> mkNamedAPI @'("get", 'TeamFeatureLegalHold)
          ( getFeatureStatus @'WithoutLockStatus @'TeamFeatureLegalHold
              getLegalholdStatusInternal
              . DoAuth
          )
        <@> mkNamedAPI @'("put", 'TeamFeatureLegalHold)
          ( setFeatureStatus @'TeamFeatureLegalHold
              (setLegalholdStatusInternal @InternalPaging)
              . DoAuth
          )
        <@> mkNamedAPI @'("get", 'TeamFeatureSearchVisibility)
          ( getFeatureStatus @'WithoutLockStatus @'TeamFeatureSearchVisibility
              getTeamSearchVisibilityAvailableInternal
              . DoAuth
          )
        <@> mkNamedAPI @'("put", 'TeamFeatureSearchVisibility)
          ( setFeatureStatus @'TeamFeatureSearchVisibility
              setTeamSearchVisibilityAvailableInternal
              . DoAuth
          )
        <@> mkNamedAPI @'("get-deprecated", 'TeamFeatureSearchVisibility)
          ( getFeatureStatus @'WithoutLockStatus @'TeamFeatureSearchVisibility
              getTeamSearchVisibilityAvailableInternal
              . DoAuth
          )
        <@> mkNamedAPI @'("put-deprecated", 'TeamFeatureSearchVisibility)
          ( setFeatureStatus @'TeamFeatureSearchVisibility
              setTeamSearchVisibilityAvailableInternal
              . DoAuth
          )
        <@> mkNamedAPI @'("get", 'TeamFeatureValidateSAMLEmails)
          ( getFeatureStatus @'WithoutLockStatus @'TeamFeatureValidateSAMLEmails
              getValidateSAMLEmailsInternal
              . DoAuth
          )
        <@> mkNamedAPI @'("get-deprecated", 'TeamFeatureValidateSAMLEmails)
          ( getFeatureStatus @'WithoutLockStatus @'TeamFeatureValidateSAMLEmails
              getValidateSAMLEmailsInternal
              . DoAuth
          )
        <@> mkNamedAPI @'("get", 'TeamFeatureDigitalSignatures)
          ( getFeatureStatus @'WithoutLockStatus @'TeamFeatureDigitalSignatures
              getDigitalSignaturesInternal
              . DoAuth
          )
        <@> mkNamedAPI @'("get-deprecated", 'TeamFeatureDigitalSignatures)
          ( getFeatureStatus @'WithoutLockStatus @'TeamFeatureDigitalSignatures
              getDigitalSignaturesInternal
              . DoAuth
          )
        <@> mkNamedAPI @'("get", 'TeamFeatureAppLock)
          ( getFeatureStatus @'WithoutLockStatus @'TeamFeatureAppLock
              getAppLockInternal
              . DoAuth
          )
        <@> mkNamedAPI @'("put", 'TeamFeatureAppLock)
          ( setFeatureStatus @'TeamFeatureAppLock
              setAppLockInternal
              . DoAuth
          )
        <@> mkNamedAPI @'("get", 'TeamFeatureFileSharing)
          ( getFeatureStatus @'WithLockStatus @'TeamFeatureFileSharing
              getFileSharingInternal
              . DoAuth
          )
        <@> mkNamedAPI @'("put", 'TeamFeatureFileSharing)
          ( setFeatureStatus @'TeamFeatureFileSharing
              setFileSharingInternal
              . DoAuth
          )
        <@> mkNamedAPI @'("get", 'TeamFeatureClassifiedDomains)
          ( getFeatureStatus @'WithoutLockStatus @'TeamFeatureClassifiedDomains
              getClassifiedDomainsInternal
              . DoAuth
          )
        <@> mkNamedAPI @'("get", 'TeamFeatureConferenceCalling)
          ( getFeatureStatus @'WithoutLockStatus @'TeamFeatureConferenceCalling
              getConferenceCallingInternal
              . DoAuth
          )
        <@> mkNamedAPI @'("get", 'TeamFeatureSelfDeletingMessages)
          ( getFeatureStatus @'WithLockStatus @'TeamFeatureSelfDeletingMessages
              getSelfDeletingMessagesInternal
              . DoAuth
          )
        <@> mkNamedAPI @'("put", 'TeamFeatureSelfDeletingMessages)
          ( setFeatureStatus @'TeamFeatureSelfDeletingMessages
              setSelfDeletingMessagesInternal
              . DoAuth
          )
        <@> mkNamedAPI @'("get", 'TeamFeatureGuestLinks)
          ( getFeatureStatus @'WithLockStatus @'TeamFeatureGuestLinks
              getGuestLinkInternal
              . DoAuth
          )
        <@> mkNamedAPI @'("put", 'TeamFeatureGuestLinks)
          ( setFeatureStatus @'TeamFeatureGuestLinks
              setGuestLinkInternal
              . DoAuth
          )
        <@> mkNamedAPI @'("get", 'TeamFeatureSndFactorPasswordChallenge)
          ( getFeatureStatus @'WithLockStatus @'TeamFeatureSndFactorPasswordChallenge
              getSndFactorPasswordChallengeInternal
              . DoAuth
          )
        <@> mkNamedAPI @'("put", 'TeamFeatureSndFactorPasswordChallenge)
          ( setFeatureStatus @'TeamFeatureSndFactorPasswordChallenge
              setSndFactorPasswordChallengeInternal
              . DoAuth
          )
        <@> mkNamedAPI @"get-all-feature-configs" getAllFeatureConfigs
        <@> mkNamedAPI @'("get-config", 'TeamFeatureLegalHold)
          ( getFeatureConfig @'WithoutLockStatus @'TeamFeatureLegalHold
              getLegalholdStatusInternal
          )
        <@> mkNamedAPI @'("get-config", 'TeamFeatureSSO)
          ( getFeatureConfig @'WithoutLockStatus @'TeamFeatureSSO
              getSSOStatusInternal
          )
        <@> mkNamedAPI @'("get-config", 'TeamFeatureSearchVisibility)
          ( getFeatureConfig @'WithoutLockStatus @'TeamFeatureSearchVisibility
              getTeamSearchVisibilityAvailableInternal
          )
        <@> mkNamedAPI @'("get-config", 'TeamFeatureValidateSAMLEmails)
          ( getFeatureConfig @'WithoutLockStatus @'TeamFeatureValidateSAMLEmails
              getValidateSAMLEmailsInternal
          )
        <@> mkNamedAPI @'("get-config", 'TeamFeatureDigitalSignatures)
          ( getFeatureConfig @'WithoutLockStatus @'TeamFeatureDigitalSignatures
              getDigitalSignaturesInternal
          )
        <@> mkNamedAPI @'("get-config", 'TeamFeatureAppLock)
          ( getFeatureConfig @'WithoutLockStatus @'TeamFeatureAppLock
              getAppLockInternal
          )
        <@> mkNamedAPI @'("get-config", 'TeamFeatureFileSharing)
          ( getFeatureConfig @'WithLockStatus @'TeamFeatureFileSharing
              getFileSharingInternal
          )
        <@> mkNamedAPI @'("get-config", 'TeamFeatureClassifiedDomains)
          ( getFeatureConfig @'WithoutLockStatus @'TeamFeatureClassifiedDomains
              getClassifiedDomainsInternal
          )
        <@> mkNamedAPI @'("get-config", 'TeamFeatureConferenceCalling)
          ( getFeatureConfig @'WithoutLockStatus @'TeamFeatureConferenceCalling
              getConferenceCallingInternal
          )
        <@> mkNamedAPI @'("get-config", 'TeamFeatureSelfDeletingMessages)
          ( getFeatureConfig @'WithLockStatus @'TeamFeatureSelfDeletingMessages
              getSelfDeletingMessagesInternal
          )
        <@> mkNamedAPI @'("get-config", 'TeamFeatureGuestLinks)
          ( getFeatureConfig @'WithLockStatus @'TeamFeatureGuestLinks
              getGuestLinkInternal
          )
        <@> mkNamedAPI @'("get-config", 'TeamFeatureSndFactorPasswordChallenge)
          ( getFeatureConfig @'WithLockStatus @'TeamFeatureSndFactorPasswordChallenge
              getSndFactorPasswordChallengeInternal
          )

    mls :: API MLSAPI GalleyEffects
    mls =
      mkNamedAPI @"mls-welcome-message" postMLSWelcome
        <@> mkNamedAPI @"mls-message" postMLSMessage
