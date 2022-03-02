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

module Galley.API.Public.Servant (servantSitemap) where

import Galley.API.Create
import Galley.API.MLS
import Galley.API.Query
import Galley.API.Teams
import Galley.API.Teams.Features
import Galley.API.Update
import Galley.App
import Galley.Cassandra.Paging
import Imports
import Polysemy
import Servant.API
import Servant.Server
import Wire.API.Routes.Named
import Wire.API.Routes.Public.Galley
import Wire.API.Team.Feature

servantSitemap :: ServerT ServantAPI (Sem GalleyEffects)
servantSitemap = conversations :<|> teamConversations :<|> messaging :<|> bot :<|> team :<|> features :<|> mls
  where
    conversations =
      Named @"get-unqualified-conversation" getUnqualifiedConversation
        :<|> Named @"get-conversation" getConversation
        :<|> Named @"get-conversation-roles" getConversationRoles
        :<|> Named @"list-conversation-ids-unqualified" conversationIdsPageFromUnqualified
        :<|> Named @"list-conversation-ids" conversationIdsPageFrom
        :<|> Named @"get-conversations" getConversations
        :<|> Named @"list-conversations" listConversations
        :<|> Named @"get-conversation-by-reusable-code" getConversationByReusableCode
        :<|> Named @"create-group-conversation" createGroupConversation
        :<|> Named @"create-self-conversation" createSelfConversation
        :<|> Named @"create-one-to-one-conversation" createOne2OneConversation
        :<|> Named @"add-members-to-conversation-unqualified" addMembersUnqualified
        :<|> Named @"add-members-to-conversation" addMembers
        :<|> Named @"join-conversation-by-id-unqualified" joinConversationById
        :<|> Named @"join-conversation-by-code-unqualified" joinConversationByReusableCode
        :<|> Named @"code-check" checkReusableCode
        :<|> Named @"create-conversation-code-unqualified" addCodeUnqualified
        :<|> Named @"remove-code-unqualified" rmCodeUnqualified
        :<|> Named @"get-code" getCode
        :<|> Named @"member-typing-unqualified" isTypingUnqualified
        :<|> Named @"remove-member-unqualified" removeMemberUnqualified
        :<|> Named @"remove-member" removeMemberQualified
        :<|> Named @"update-other-member-unqualified" updateOtherMemberUnqualified
        :<|> Named @"update-other-member" updateOtherMember
        :<|> Named @"update-conversation-name-deprecated" updateUnqualifiedConversationName
        :<|> Named @"update-conversation-name-unqualified" updateUnqualifiedConversationName
        :<|> Named @"update-conversation-name" updateConversationName
        :<|> Named @"update-conversation-message-timer-unqualified" updateConversationMessageTimerUnqualified
        :<|> Named @"update-conversation-message-timer" updateConversationMessageTimer
        :<|> Named @"update-conversation-receipt-mode-unqualified" updateConversationReceiptModeUnqualified
        :<|> Named @"update-conversation-receipt-mode" updateConversationReceiptMode
        :<|> Named @"update-conversation-access-unqualified" updateConversationAccessUnqualified
        :<|> Named @"update-conversation-access" updateConversationAccess
        :<|> Named @"get-conversation-self-unqualified" getLocalSelf
        :<|> Named @"update-conversation-self-unqualified" updateUnqualifiedSelfMember
        :<|> Named @"update-conversation-self" updateSelfMember

    teamConversations =
      Named @"get-team-conversation-roles" getTeamConversationRoles
        :<|> Named @"get-team-conversations" getTeamConversations
        :<|> Named @"get-team-conversation" getTeamConversation
        :<|> Named @"delete-team-conversation" deleteTeamConversation

    messaging =
      Named @"post-otr-message-unqualified" postOtrMessageUnqualified
        :<|> Named @"post-otr-broadcast-unqualified" postOtrBroadcastUnqualified
        :<|> Named @"post-proteus-message" postProteusMessage
        :<|> Named @"post-proteus-broadcast" postProteusBroadcast

    bot =
      Named @"post-bot-message-unqualified" postBotMessageUnqualified

    team =
      Named @"create-non-binding-team" createNonBindingTeamH
        :<|> Named @"update-team" updateTeamH
        :<|> Named @"get-teams" getManyTeams
        :<|> Named @"get-team" getTeamH
        :<|> Named @"delete-team" deleteTeam

    mls = Named @"mls-welcome-message" postMLSWelcome

    features =
      Named @'("get", 'TeamFeatureSSO)
        ( getFeatureStatus @'WithoutLockStatus @'TeamFeatureSSO
            getSSOStatusInternal
            . DoAuth
        )
        :<|> Named @'("get", 'TeamFeatureLegalHold)
          ( getFeatureStatus @'WithoutLockStatus @'TeamFeatureLegalHold
              getLegalholdStatusInternal
              . DoAuth
          )
        :<|> Named @'("put", 'TeamFeatureLegalHold)
          ( setFeatureStatus @'TeamFeatureLegalHold
              (setLegalholdStatusInternal @InternalPaging)
              . DoAuth
          )
        :<|> Named @'("get", 'TeamFeatureSearchVisibility)
          ( getFeatureStatus @'WithoutLockStatus @'TeamFeatureSearchVisibility
              getTeamSearchVisibilityAvailableInternal
              . DoAuth
          )
        :<|> Named @'("put", 'TeamFeatureSearchVisibility)
          ( setFeatureStatus @'TeamFeatureSearchVisibility
              setTeamSearchVisibilityAvailableInternal
              . DoAuth
          )
        :<|> Named @'("get-deprecated", 'TeamFeatureSearchVisibility)
          ( getFeatureStatus @'WithoutLockStatus @'TeamFeatureSearchVisibility
              getTeamSearchVisibilityAvailableInternal
              . DoAuth
          )
        :<|> Named @'("put-deprecated", 'TeamFeatureSearchVisibility)
          ( setFeatureStatus @'TeamFeatureSearchVisibility
              setTeamSearchVisibilityAvailableInternal
              . DoAuth
          )
        :<|> Named @'("get", 'TeamFeatureValidateSAMLEmails)
          ( getFeatureStatus @'WithoutLockStatus @'TeamFeatureValidateSAMLEmails
              getValidateSAMLEmailsInternal
              . DoAuth
          )
        :<|> Named @'("get-deprecated", 'TeamFeatureValidateSAMLEmails)
          ( getFeatureStatus @'WithoutLockStatus @'TeamFeatureValidateSAMLEmails
              getValidateSAMLEmailsInternal
              . DoAuth
          )
        :<|> Named @'("get", 'TeamFeatureDigitalSignatures)
          ( getFeatureStatus @'WithoutLockStatus @'TeamFeatureDigitalSignatures
              getDigitalSignaturesInternal
              . DoAuth
          )
        :<|> Named @'("get-deprecated", 'TeamFeatureDigitalSignatures)
          ( getFeatureStatus @'WithoutLockStatus @'TeamFeatureDigitalSignatures
              getDigitalSignaturesInternal
              . DoAuth
          )
        :<|> Named @'("get", 'TeamFeatureAppLock)
          ( getFeatureStatus @'WithoutLockStatus @'TeamFeatureAppLock
              getAppLockInternal
              . DoAuth
          )
        :<|> Named @'("put", 'TeamFeatureAppLock)
          ( setFeatureStatus @'TeamFeatureAppLock
              setAppLockInternal
              . DoAuth
          )
        :<|> Named @'("get", 'TeamFeatureFileSharing)
          ( getFeatureStatus @'WithLockStatus @'TeamFeatureFileSharing
              getFileSharingInternal
              . DoAuth
          )
        :<|> Named @'("put", 'TeamFeatureFileSharing)
          ( setFeatureStatus @'TeamFeatureFileSharing
              setFileSharingInternal
              . DoAuth
          )
        :<|> Named @'("get", 'TeamFeatureClassifiedDomains)
          ( getFeatureStatus @'WithoutLockStatus @'TeamFeatureClassifiedDomains
              getClassifiedDomainsInternal
              . DoAuth
          )
        :<|> Named @'("get", 'TeamFeatureConferenceCalling)
          ( getFeatureStatus @'WithoutLockStatus @'TeamFeatureConferenceCalling
              getConferenceCallingInternal
              . DoAuth
          )
        :<|> Named @'("get", 'TeamFeatureSelfDeletingMessages)
          ( getFeatureStatus @'WithLockStatus @'TeamFeatureSelfDeletingMessages
              getSelfDeletingMessagesInternal
              . DoAuth
          )
        :<|> Named @'("put", 'TeamFeatureSelfDeletingMessages)
          ( setFeatureStatus @'TeamFeatureSelfDeletingMessages
              setSelfDeletingMessagesInternal
              . DoAuth
          )
        :<|> Named @'("get", 'TeamFeatureGuestLinks)
          ( getFeatureStatus @'WithLockStatus @'TeamFeatureGuestLinks
              getGuestLinkInternal
              . DoAuth
          )
        :<|> Named @'("put", 'TeamFeatureGuestLinks)
          ( setFeatureStatus @'TeamFeatureGuestLinks
              setGuestLinkInternal
              . DoAuth
          )
        :<|> Named @'("get", 'TeamFeatureSndFactorPasswordChallenge)
          ( getFeatureStatus @'WithLockStatus @'TeamFeatureSndFactorPasswordChallenge
              getSndFactorPasswordChallengeInternal
              . DoAuth
          )
        :<|> Named @'("put", 'TeamFeatureSndFactorPasswordChallenge)
          ( setFeatureStatus @'TeamFeatureSndFactorPasswordChallenge
              setSndFactorPasswordChallengeInternal
              . DoAuth
          )
        :<|> Named @"get-all-feature-configs" getAllFeatureConfigs
        :<|> Named @'("get-config", 'TeamFeatureLegalHold)
          ( getFeatureConfig @'WithoutLockStatus @'TeamFeatureLegalHold
              getLegalholdStatusInternal
          )
        :<|> Named @'("get-config", 'TeamFeatureSSO)
          ( getFeatureConfig @'WithoutLockStatus @'TeamFeatureSSO
              getSSOStatusInternal
          )
        :<|> Named @'("get-config", 'TeamFeatureSearchVisibility)
          ( getFeatureConfig @'WithoutLockStatus @'TeamFeatureSearchVisibility
              getTeamSearchVisibilityAvailableInternal
          )
        :<|> Named @'("get-config", 'TeamFeatureValidateSAMLEmails)
          ( getFeatureConfig @'WithoutLockStatus @'TeamFeatureValidateSAMLEmails
              getValidateSAMLEmailsInternal
          )
        :<|> Named @'("get-config", 'TeamFeatureDigitalSignatures)
          ( getFeatureConfig @'WithoutLockStatus @'TeamFeatureDigitalSignatures
              getDigitalSignaturesInternal
          )
        :<|> Named @'("get-config", 'TeamFeatureAppLock)
          ( getFeatureConfig @'WithoutLockStatus @'TeamFeatureAppLock
              getAppLockInternal
          )
        :<|> Named @'("get-config", 'TeamFeatureFileSharing)
          ( getFeatureConfig @'WithLockStatus @'TeamFeatureFileSharing
              getFileSharingInternal
          )
        :<|> Named @'("get-config", 'TeamFeatureClassifiedDomains)
          ( getFeatureConfig @'WithoutLockStatus @'TeamFeatureClassifiedDomains
              getClassifiedDomainsInternal
          )
        :<|> Named @'("get-config", 'TeamFeatureConferenceCalling)
          ( getFeatureConfig @'WithoutLockStatus @'TeamFeatureConferenceCalling
              getConferenceCallingInternal
          )
        :<|> Named @'("get-config", 'TeamFeatureSelfDeletingMessages)
          ( getFeatureConfig @'WithLockStatus @'TeamFeatureSelfDeletingMessages
              getSelfDeletingMessagesInternal
          )
        :<|> Named @'("get-config", 'TeamFeatureGuestLinks)
          ( getFeatureConfig @'WithLockStatus @'TeamFeatureGuestLinks
              getGuestLinkInternal
          )
        :<|> Named @'("get-config", 'TeamFeatureSndFactorPasswordChallenge)
          ( getFeatureConfig @'WithLockStatus @'TeamFeatureSndFactorPasswordChallenge
              getSndFactorPasswordChallengeInternal
          )
