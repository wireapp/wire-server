-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Routes.Internal.Galley where

import Control.Lens ((.~))
import Data.Domain
import Data.Id as Id
import Data.OpenApi (OpenApi, info, title)
import Data.Range
import GHC.TypeLits (AppendSymbol)
import Imports hiding (head)
import Servant
import Servant.OpenApi
import Wire.API.Bot
import Wire.API.Bot.Service
import Wire.API.Conversation
import Wire.API.Conversation.Role
import Wire.API.CustomBackend
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.FederationStatus
import Wire.API.Provider.Service (ServiceRef)
import Wire.API.Routes.Features
import Wire.API.Routes.Internal.Brig.EJPD
import Wire.API.Routes.Internal.Galley.ConversationsIntra
import Wire.API.Routes.Internal.Galley.TeamsIntra
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Routes.Public.Galley.Conversation
import Wire.API.Routes.Public.Galley.Feature
import Wire.API.Routes.Public.Util
import Wire.API.Routes.QualifiedCapture
import Wire.API.Routes.Version
import Wire.API.Team
import Wire.API.Team.Feature
import Wire.API.Team.Member
import Wire.API.Team.SearchVisibility
import Wire.API.User.Client

type family IFeatureAPI1 cfg where
  -- special case for classified domains, since it cannot be set
  IFeatureAPI1 ClassifiedDomainsConfig = IFeatureStatusGet ClassifiedDomainsConfig
  IFeatureAPI1 cfg = IFeatureAPI1Full cfg

type IFeatureAPI1Full cfg =
  IFeatureStatusGet cfg
    :<|> IFeatureStatusPut cfg
    :<|> IFeatureStatusPatch cfg

type family IAllFeaturesAPI cfgs where
  IAllFeaturesAPI '[cfg] = IFeatureAPI1 cfg
  IAllFeaturesAPI (cfg : cfgs) = IFeatureAPI1 cfg :<|> IAllFeaturesAPI cfgs

type IFeatureAPI =
  IAllFeaturesAPI Features
    -- legacy lock status put endpoints
    :<|> IFeatureStatusLockStatusPut FileSharingConfig
    :<|> IFeatureStatusLockStatusPut ConferenceCallingConfig
    :<|> IFeatureStatusLockStatusPut SelfDeletingMessagesConfig
    :<|> IFeatureStatusLockStatusPut GuestLinksConfig
    :<|> IFeatureStatusLockStatusPut SndFactorPasswordChallengeConfig
    :<|> IFeatureStatusLockStatusPut MLSConfig
    :<|> IFeatureStatusLockStatusPut OutlookCalIntegrationConfig
    :<|> IFeatureStatusLockStatusPut MlsE2EIdConfig
    :<|> IFeatureStatusLockStatusPut MlsMigrationConfig
    :<|> IFeatureStatusLockStatusPut EnforceFileDownloadLocationConfig
    -- all feature configs
    :<|> Named
           "feature-configs-internal"
           ( Summary "Get all feature configs (for user/team; if n/a fall back to site config)."
               :> "feature-configs"
               :> CanThrow OperationDenied
               :> CanThrow 'NotATeamMember
               :> CanThrow 'TeamNotFound
               :> QueryParam'
                    [ Optional,
                      Strict,
                      Description "Optional user id"
                    ]
                    "user_id"
                    UserId
               :> Get '[JSON] AllTeamFeatures
           )
    :<|> IFeatureStatusLockStatusPut DomainRegistrationConfig
    :<|> IFeatureStatusLockStatusPut ChannelsConfig

type InternalAPI = "i" :> InternalAPIBase

type InternalAPIBase =
  Named
    "status"
    ( "status" :> MultiVerb 'GET '[JSON] '[RespondEmpty 200 "OK"] ()
    )
    -- This endpoint can lead to the following events being sent:
    -- - MemberLeave event to members for all conversations the user was in
    :<|> Named
           "delete-user"
           ( Summary
               "Remove a user from their teams and conversations and erase their clients"
               :> ZLocalUser
               :> ZOptConn
               :> "user"
               :> MultiVerb 'DELETE '[JSON] '[RespondEmpty 200 "Remove a user from Galley"] ()
           )
    -- This endpoint can lead to the following events being sent:
    -- - ConvCreate event to self, if conversation did not exist before
    -- - ConvConnect event to self, if other didn't join the connect conversation before
    :<|> Named
           "connect"
           ( Summary "Create a connect conversation (deprecated)"
               :> CanThrow 'ConvNotFound
               :> CanThrow 'InvalidOperation
               :> CanThrow 'NotConnected
               :> CanThrow UnreachableBackends
               :> ZLocalUser
               :> ZOptConn
               :> "conversations"
               :> "connect"
               :> ReqBody '[JSON] Connect
               :> ConversationVerb 'V6 Conversation
           )
    -- This endpoint is meant for testing membership of a conversation
    :<|> Named
           "get-conversation-clients"
           ( Summary "Get mls conversation client list"
               :> CanThrow 'ConvNotFound
               :> "group"
               :> Capture "gid" GroupId
               :> MultiVerb1
                    'GET
                    '[JSON]
                    (Respond 200 "Clients" ClientList)
           )
    :<|> Named
           "guard-legalhold-policy-conflicts"
           ( "guard-legalhold-policy-conflicts"
               :> CanThrow 'MissingLegalholdConsent
               :> CanThrow 'MissingLegalholdConsentOldClients
               :> ReqBody '[JSON] GuardLegalholdPolicyConflicts
               :> MultiVerb1 'PUT '[JSON] (RespondEmpty 200 "Guard Legalhold Policy")
           )
    :<|> ILegalholdWhitelistedTeamsAPI
    :<|> ITeamsAPI
    :<|> IMiscAPI
    :<|> Named
           "upsert-one2one"
           ( Summary "Create or Update a connect or one2one conversation."
               :> "conversations"
               :> "one2one"
               :> "upsert"
               :> ReqBody '[JSON] UpsertOne2OneConversationRequest
               :> MultiVerb1 'POST '[JSON] (RespondEmpty 200 "Upsert One2One Policy")
           )
    :<|> IFeatureAPI
    :<|> IFederationAPI
    :<|> IConversationAPI
    :<|> IEJPDAPI

type ILegalholdWhitelistedTeamsAPI =
  "legalhold"
    :> "whitelisted-teams"
    :> Capture "tid" TeamId
    :> ILegalholdWhitelistedTeamsAPIBase

type ILegalholdWhitelistedTeamsAPIBase =
  Named
    "set-team-legalhold-whitelisted"
    (MultiVerb1 'PUT '[JSON] (RespondEmpty 200 "Team Legalhold Whitelisted"))
    :<|> Named
           "unset-team-legalhold-whitelisted"
           (MultiVerb1 'DELETE '[JSON] (RespondEmpty 204 "Team Legalhold un-Whitelisted"))
    :<|> Named
           "get-team-legalhold-whitelisted"
           ( MultiVerb
               'GET
               '[JSON]
               '[ RespondEmpty 404 "Team not Legalhold Whitelisted",
                  RespondEmpty 200 "Team Legalhold Whitelisted"
                ]
               Bool
           )

type ITeamsAPI = "teams" :> Capture "tid" TeamId :> ITeamsAPIBase

type ITeamsAPIBase =
  Named "get-team-internal" (CanThrow 'TeamNotFound :> Get '[JSON] TeamData)
    :<|> Named
           "create-binding-team"
           ( ZUser
               :> ReqBody '[JSON] NewTeam
               :> MultiVerb1
                    'PUT
                    '[JSON]
                    ( WithHeaders
                        '[Header "Location" TeamId]
                        TeamId
                        (RespondEmpty 201 "OK")
                    )
           )
    :<|> Named
           "delete-binding-team"
           ( CanThrow 'NoBindingTeam
               :> CanThrow 'NotAOneMemberTeam
               :> CanThrow 'DeleteQueueFull
               :> CanThrow 'TeamNotFound
               :> QueryFlag "force"
               :> MultiVerb1 'DELETE '[JSON] (RespondEmpty 202 "OK")
           )
    :<|> Named "get-team-name" ("name" :> CanThrow 'TeamNotFound :> Get '[JSON] TeamName)
    :<|> Named
           "update-team-status"
           ( "status"
               :> CanThrow 'TeamNotFound
               :> CanThrow 'InvalidTeamStatusUpdate
               :> ReqBody '[JSON] TeamStatusUpdate
               :> MultiVerb1 'PUT '[JSON] (RespondEmpty 200 "OK")
           )
    :<|> "members"
      :> ( Named
             "unchecked-add-team-member"
             ( CanThrow 'TooManyTeamMembers
                 :> CanThrow 'TooManyTeamMembersOnTeamWithLegalhold
                 :> CanThrow 'TooManyTeamAdmins
                 :> ReqBody '[JSON] NewTeamMember
                 :> MultiVerb1 'POST '[JSON] (RespondEmpty 200 "OK")
             )
             :<|> Named
                    "unchecked-get-team-members"
                    ( QueryParam' '[Strict] "maxResults" (Range 1 HardTruncationLimit Int32)
                        :> Get '[JSON] TeamMemberList
                    )
             :<|> Named
                    "unchecked-get-team-member"
                    ( Capture "uid" UserId
                        :> CanThrow 'TeamMemberNotFound
                        :> Get '[JSON] TeamMember
                    )
             :<|> Named
                    "can-user-join-team"
                    ( "check"
                        :> CanThrow 'TooManyTeamMembersOnTeamWithLegalhold
                        :> MultiVerb1 'GET '[JSON] (RespondEmpty 200 "User can join")
                    )
             :<|> Named
                    "unchecked-update-team-member"
                    ( CanThrow 'AccessDenied
                        :> CanThrow 'InvalidPermissions
                        :> CanThrow 'TeamNotFound
                        :> CanThrow 'TeamMemberNotFound
                        :> CanThrow 'TooManyTeamAdmins
                        :> CanThrow 'NotATeamMember
                        :> CanThrow OperationDenied
                        :> ReqBody '[JSON] NewTeamMember
                        :> MultiVerb1 'PUT '[JSON] (RespondEmpty 200 "")
                    )
         )
    :<|> Named
           "user-is-team-owner"
           ( "is-team-owner"
               :> Capture "uid" UserId
               :> CanThrow 'AccessDenied
               :> CanThrow 'TeamMemberNotFound
               :> CanThrow 'NotATeamMember
               :> MultiVerb1 'GET '[JSON] (RespondEmpty 200 "User is team owner")
           )
    :<|> "search-visibility"
      :> ( Named "get-search-visibility-internal" (Get '[JSON] TeamSearchVisibilityView)
             :<|> Named
                    "set-search-visibility-internal"
                    ( CanThrow 'TeamSearchVisibilityNotEnabled
                        :> CanThrow OperationDenied
                        :> CanThrow 'NotATeamMember
                        :> CanThrow 'TeamNotFound
                        :> ReqBody '[JSON] TeamSearchVisibilityView
                        :> MultiVerb1 'PUT '[JSON] (RespondEmpty 204 "OK")
                    )
         )

type IFeatureStatusGet cfg =
  Named
    '("iget", cfg)
    ( Description (FeatureAPIDesc cfg)
        :> FeatureStatusBaseGet cfg
    )

type IFeatureStatusPut cfg =
  Named
    '("iput", cfg)
    ( Description (FeatureAPIDesc cfg)
        :> FeatureStatusBasePutInternal cfg
    )

type IFeatureStatusPatch cfg =
  Named
    '("ipatch", cfg)
    ( Description (FeatureAPIDesc cfg)
        :> FeatureStatusBasePatchInternal cfg
    )

type FeatureStatusBasePutInternal cfg =
  FeatureStatusBaseInternal
    (AppendSymbol "Put config for " (FeatureSymbol cfg))
    cfg
    ( ReqBody '[JSON] (Feature cfg)
        :> Put '[JSON] (LockableFeature cfg)
    )

type FeatureStatusBasePatchInternal cfg =
  FeatureStatusBaseInternal
    (AppendSymbol "Patch config for " (FeatureSymbol cfg))
    cfg
    ( ReqBody '[JSON] (LockableFeaturePatch cfg)
        :> Patch '[JSON] (LockableFeature cfg)
    )

type FeatureStatusBaseInternal desc cfg a =
  Summary desc
    :> CanThrow OperationDenied
    :> CanThrow 'NotATeamMember
    :> CanThrow 'TeamNotFound
    :> CanThrow TeamFeatureError
    :> CanThrowMany (FeatureErrors cfg)
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> FeatureSymbol cfg
    :> a

type IFeatureStatusLockStatusPut cfg =
  Named
    '("ilock", cfg)
    ( Summary (AppendSymbol "(Un-)lock " (FeatureSymbol cfg))
        :> Description (FeatureAPIDesc cfg)
        :> CanThrow 'NotATeamMember
        :> CanThrow 'TeamNotFound
        :> "teams"
        :> Capture "tid" TeamId
        :> "features"
        :> FeatureSymbol cfg
        :> Capture "lockStatus" LockStatus
        :> Put '[JSON] LockStatusResponse
    )

type IFederationAPI =
  Named
    "get-federation-status"
    ( Summary "Get the federation status (only needed for integration/QA tests at the time of writing it)"
        :> CanThrow UnreachableBackends
        :> ZLocalUser
        :> "federation-status"
        :> ReqBody '[JSON] RemoteDomains
        :> Get '[JSON] FederationStatus
    )

type IConversationAPI =
  Named
    "conversation-get-member"
    ( "conversations"
        :> QualifiedCapture "cnv" ConvId
        :> "members"
        :> Capture "usr" UserId
        :> Get '[JSON] (Maybe Member)
    )
    -- This endpoint can lead to the following events being sent:
    -- - MemberJoin event to you, if the conversation existed and had < 2 members before
    -- - MemberJoin event to other, if the conversation existed and only the other was member
    --   before
    :<|> Named
           "conversation-accept-v2"
           ( CanThrow 'InvalidOperation
               :> CanThrow 'ConvNotFound
               :> ZLocalUser
               :> ZOptConn
               :> "conversations"
               :> Capture "cnv" ConvId
               :> "accept"
               :> "v2"
               :> Put '[JSON] Conversation
           )
    :<|> Named
           "conversation-block"
           ( CanThrow 'InvalidOperation
               :> CanThrow 'ConvNotFound
               :> ZLocalUser
               :> "conversations"
               :> QualifiedCapture "cnv" ConvId
               :> "block"
               :> Put '[JSON] ()
           )
    -- This endpoint can lead to the following events being sent:
    -- - MemberJoin event to you, if the conversation existed and had < 2 members before
    -- - MemberJoin event to other, if the conversation existed and only the other was member
    --   before
    :<|> Named
           "conversation-unblock"
           ( CanThrow 'InvalidOperation
               :> CanThrow 'ConvNotFound
               :> ZLocalUser
               :> ZOptConn
               :> "conversations"
               :> QualifiedCapture "cnv" ConvId
               :> "unblock"
               :> Put '[JSON] ()
           )
    :<|> Named
           "conversation-meta"
           ( CanThrow 'ConvNotFound
               :> "conversations"
               :> Capture "cnv" ConvId
               :> "meta"
               :> Get '[JSON] ConversationMetadata
           )
    :<|> Named
           "conversation-mls-one-to-one"
           ( CanThrow 'NotConnected
               :> CanThrow 'MLSNotEnabled
               :> "mls-one2one-conversations"
               :> ZLocalUser
               :> QualifiedCapture "user" UserId
               :> Get '[JSON] Conversation
           )
    :<|> Named
           "conversation-mls-one-to-one-established"
           ( CanThrow 'NotConnected
               :> CanThrow 'MLSNotEnabled
               :> ZLocalUser
               :> "mls-one2one-conversations"
               :> QualifiedCapture "user" UserId
               :> "established"
               :> Get '[JSON] Bool
           )

type IMiscAPI =
  Named
    "get-team-members"
    ( CanThrow 'NonBindingTeam
        :> CanThrow 'TeamNotFound
        :> "users"
        :> Capture "uid" UserId
        :> "team"
        :> "members"
        :> Get '[JSON] TeamMemberList
    )
    :<|> Named
           "get-team-id"
           ( CanThrow 'NonBindingTeam
               :> CanThrow 'TeamNotFound
               :> "users"
               :> Capture "uid" UserId
               :> "team"
               :> Get '[JSON] TeamId
           )
    :<|> Named
           "test-get-clients"
           ( -- eg. https://github.com/wireapp/wire-server/blob/3bdca5fc8154e324773802a0deb46d884bd09143/services/brig/test/integration/API/User/Client.hs#L319
             "test"
               :> "clients"
               :> ZUser
               :> Get '[JSON] [ClientId]
           )
    :<|> Named
           "test-add-client"
           ( "clients"
               :> ZUser
               :> Capture "cid" ClientId
               :> MultiVerb1
                    'POST
                    '[JSON]
                    (RespondEmpty 200 "OK")
           )
    :<|> Named
           "test-delete-client"
           ( "clients"
               :> ZUser
               :> Capture "cid" ClientId
               :> MultiVerb1
                    'DELETE
                    '[JSON]
                    (RespondEmpty 200 "OK")
           )
    :<|> Named
           "add-service"
           ( "services"
               :> ReqBody '[JSON] Service
               :> MultiVerb1
                    'POST
                    '[JSON]
                    (RespondEmpty 200 "OK")
           )
    :<|> Named
           "delete-service"
           ( "services"
               :> ReqBody '[JSON] ServiceRef
               :> MultiVerb1
                    'DELETE
                    '[JSON]
                    (RespondEmpty 200 "OK")
           )
    :<|> Named
           "i-add-bot"
           ( -- This endpoint can lead to the following events being sent:
             -- - MemberJoin event to members
             CanThrow ('ActionDenied 'AddConversationMember)
               :> CanThrow 'ConvNotFound
               :> CanThrow 'InvalidOperation
               :> CanThrow 'TooManyMembers
               :> "bots"
               :> ZLocalUser
               :> ZConn
               :> ReqBody '[JSON] AddBot
               :> Post '[JSON] Event
           )
    :<|> Named
           "delete-bot"
           ( -- This endpoint can lead to the following events being sent:
             -- - MemberLeave event to members
             CanThrow 'ConvNotFound
               :> CanThrow ('ActionDenied 'RemoveConversationMember)
               :> "bots"
               :> ZLocalUser
               :> ZOptConn
               :> ReqBody '[JSON] RemoveBot
               :> MultiVerb
                    'DELETE
                    '[JSON]
                    (UpdateResponses "Bot not found" "Bot deleted" Event)
                    (UpdateResult Event)
           )
    :<|> Named
           "put-custom-backend"
           ( "custom-backend"
               :> "by-domain"
               :> Capture "domain" Domain
               :> ReqBody '[JSON] CustomBackend
               :> MultiVerb1 'PUT '[JSON] (RespondEmpty 201 "OK")
           )
    :<|> Named
           "delete-custom-backend"
           ( "custom-backend"
               :> "by-domain"
               :> Capture "domain" Domain
               :> MultiVerb1 'DELETE '[JSON] (RespondEmpty 200 "OK")
           )

type IEJPDAPI =
  Named
    "get-conversations-by-user"
    ( CanThrow 'NotConnected
        :> "user"
        :> Capture "user" UserId
        :> "all-conversations"
        :> Get '[Servant.JSON] [EJPDConvInfo]
    )

swaggerDoc :: OpenApi
swaggerDoc =
  toOpenApi (Proxy @InternalAPI)
    & info . title .~ "Wire-Server internal galley API"
