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
import Data.Id as Id
import Data.Range
import Data.Swagger (Swagger, info, title)
import GHC.TypeLits (AppendSymbol)
import Imports hiding (head)
import Servant hiding (JSON, WithStatus)
import qualified Servant hiding (WithStatus)
import Servant.Swagger
import Wire.API.ApplyMods
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.MakesFederatedCall
import Wire.API.Routes.Internal.Galley.ConversationsIntra
import Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti
import Wire.API.Routes.Internal.Galley.TeamsIntra
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Routes.Public.Galley.Conversation
import Wire.API.Routes.Public.Galley.Feature
import Wire.API.Team
import Wire.API.Team.Feature
import Wire.API.Team.Member
import Wire.API.Team.SearchVisibility

type LegalHoldFeatureStatusChangeErrors =
  '( 'ActionDenied 'RemoveConversationMember,
     '( AuthenticationError,
        '( 'CannotEnableLegalHoldServiceLargeTeam,
           '( 'LegalHoldNotEnabled,
              '( 'LegalHoldDisableUnimplemented,
                 '( 'LegalHoldServiceNotRegistered,
                    '( 'UserLegalHoldIllegalOperation,
                       '( 'LegalHoldCouldNotBlockConnections, '())
                     )
                  )
               )
            )
         )
      )
   )

type LegalHoldFeaturesStatusChangeFederatedCalls =
  '[ MakesFederatedCall 'Galley "on-conversation-updated",
     MakesFederatedCall 'Galley "on-mls-message-sent",
     MakesFederatedCall 'Galley "on-new-remote-conversation"
   ]

type IFeatureAPI =
  -- SSOConfig
  IFeatureStatusGet SSOConfig
    :<|> IFeatureStatusPut '[] '() SSOConfig
    :<|> IFeatureStatusPatch '[] '() SSOConfig
    -- LegalholdConfig
    :<|> IFeatureStatusGet LegalholdConfig
    :<|> IFeatureStatusPut
           LegalHoldFeaturesStatusChangeFederatedCalls
           LegalHoldFeatureStatusChangeErrors
           LegalholdConfig
    :<|> IFeatureStatusPatch
           LegalHoldFeaturesStatusChangeFederatedCalls
           LegalHoldFeatureStatusChangeErrors
           LegalholdConfig
    -- SearchVisibilityAvailableConfig
    :<|> IFeatureStatusGet SearchVisibilityAvailableConfig
    :<|> IFeatureStatusPut '[] '() SearchVisibilityAvailableConfig
    :<|> IFeatureStatusPatch '[] '() SearchVisibilityAvailableConfig
    -- ValidateSAMLEmailsConfig
    :<|> IFeatureStatusGet ValidateSAMLEmailsConfig
    :<|> IFeatureStatusPut '[] '() ValidateSAMLEmailsConfig
    :<|> IFeatureStatusPatch '[] '() ValidateSAMLEmailsConfig
    -- DigitalSignaturesConfig
    :<|> IFeatureStatusGet DigitalSignaturesConfig
    :<|> IFeatureStatusPut '[] '() DigitalSignaturesConfig
    :<|> IFeatureStatusPatch '[] '() DigitalSignaturesConfig
    -- AppLockConfig
    :<|> IFeatureStatusGet AppLockConfig
    :<|> IFeatureStatusPut '[] '() AppLockConfig
    :<|> IFeatureStatusPatch '[] '() AppLockConfig
    -- FileSharingConfig
    :<|> IFeatureStatusGet FileSharingConfig
    :<|> IFeatureStatusPut '[] '() FileSharingConfig
    :<|> IFeatureStatusLockStatusPut FileSharingConfig
    :<|> IFeatureStatusPatch '[] '() FileSharingConfig
    -- ConferenceCallingConfig
    :<|> IFeatureStatusGet ConferenceCallingConfig
    :<|> IFeatureStatusPut '[] '() ConferenceCallingConfig
    :<|> IFeatureStatusPatch '[] '() ConferenceCallingConfig
    -- SelfDeletingMessagesConfig
    :<|> IFeatureStatusGet SelfDeletingMessagesConfig
    :<|> IFeatureStatusPut '[] '() SelfDeletingMessagesConfig
    :<|> IFeatureStatusLockStatusPut SelfDeletingMessagesConfig
    :<|> IFeatureStatusPatch '[] '() SelfDeletingMessagesConfig
    -- GuestLinksConfig
    :<|> IFeatureStatusGet GuestLinksConfig
    :<|> IFeatureStatusPut '[] '() GuestLinksConfig
    :<|> IFeatureStatusLockStatusPut GuestLinksConfig
    :<|> IFeatureStatusPatch '[] '() GuestLinksConfig
    --  SndFactorPasswordChallengeConfig
    :<|> IFeatureStatusGet SndFactorPasswordChallengeConfig
    :<|> IFeatureStatusPut '[] '() SndFactorPasswordChallengeConfig
    :<|> IFeatureStatusLockStatusPut SndFactorPasswordChallengeConfig
    :<|> IFeatureStatusPatch '[] '() SndFactorPasswordChallengeConfig
    -- SearchVisibilityInboundConfig
    :<|> IFeatureStatusGet SearchVisibilityInboundConfig
    :<|> IFeatureStatusPut '[] '() SearchVisibilityInboundConfig
    :<|> IFeatureStatusPatch '[] '() SearchVisibilityInboundConfig
    :<|> IFeatureNoConfigMultiGet SearchVisibilityInboundConfig
    -- ClassifiedDomainsConfig
    :<|> IFeatureStatusGet ClassifiedDomainsConfig
    -- MLSConfig
    :<|> IFeatureStatusGet MLSConfig
    :<|> IFeatureStatusPut '[] '() MLSConfig
    :<|> IFeatureStatusPatch '[] '() MLSConfig
    -- ExposeInvitationURLsToTeamAdminConfig
    :<|> IFeatureStatusGet ExposeInvitationURLsToTeamAdminConfig
    :<|> IFeatureStatusPut '[] '() ExposeInvitationURLsToTeamAdminConfig
    :<|> IFeatureStatusPatch '[] '() ExposeInvitationURLsToTeamAdminConfig
    -- SearchVisibilityInboundConfig
    :<|> IFeatureStatusGet SearchVisibilityInboundConfig
    :<|> IFeatureStatusPut '[] '() SearchVisibilityInboundConfig
    :<|> IFeatureStatusPatch '[] '() SearchVisibilityInboundConfig
    -- OutlookCalIntegrationConfig
    :<|> IFeatureStatusGet OutlookCalIntegrationConfig
    :<|> IFeatureStatusPut '[] '() OutlookCalIntegrationConfig
    :<|> IFeatureStatusPatch '[] '() OutlookCalIntegrationConfig
    :<|> IFeatureStatusLockStatusPut OutlookCalIntegrationConfig
    -- MlsE2EIdConfig
    :<|> IFeatureStatusGet MlsE2EIdConfig
    :<|> IFeatureStatusPut '[] '() MlsE2EIdConfig
    :<|> IFeatureStatusPatch '[] '() MlsE2EIdConfig
    :<|> IFeatureStatusLockStatusPut MlsE2EIdConfig
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
               :> Get '[Servant.JSON] AllFeatureConfigs
           )

type InternalAPI = "i" :> InternalAPIBase

type InternalAPIBase =
  Named
    "status"
    ( "status" :> MultiVerb 'GET '[Servant.JSON] '[RespondEmpty 200 "OK"] ()
    )
    -- This endpoint can lead to the following events being sent:
    -- - MemberLeave event to members for all conversations the user was in
    :<|> Named
           "delete-user"
           ( Summary
               "Remove a user from their teams and conversations and erase their clients"
               :> MakesFederatedCall 'Galley "on-conversation-updated"
               :> MakesFederatedCall 'Galley "on-user-deleted-conversations"
               :> MakesFederatedCall 'Galley "on-mls-message-sent"
               :> ZLocalUser
               :> ZOptConn
               :> "user"
               :> MultiVerb 'DELETE '[Servant.JSON] '[RespondEmpty 200 "Remove a user from Galley"] ()
           )
    -- This endpoint can lead to the following events being sent:
    -- - ConvCreate event to self, if conversation did not exist before
    -- - ConvConnect event to self, if other didn't join the connect conversation before
    :<|> Named
           "connect"
           ( Summary "Create a connect conversation (deprecated)"
               :> MakesFederatedCall 'Galley "on-conversation-created"
               :> CanThrow 'ConvNotFound
               :> CanThrow 'InvalidOperation
               :> CanThrow 'NotConnected
               :> ZLocalUser
               :> ZOptConn
               :> "conversations"
               :> "connect"
               :> ReqBody '[Servant.JSON] Connect
               :> ConversationVerb
           )
    :<|> Named
           "guard-legalhold-policy-conflicts"
           ( "guard-legalhold-policy-conflicts"
               :> CanThrow 'MissingLegalholdConsent
               :> ReqBody '[Servant.JSON] GuardLegalholdPolicyConflicts
               :> MultiVerb1 'PUT '[Servant.JSON] (RespondEmpty 200 "Guard Legalhold Policy")
           )
    :<|> ILegalholdWhitelistedTeamsAPI
    :<|> ITeamsAPI
    :<|> Named
           "upsert-one2one"
           ( Summary "Create or Update a connect or one2one conversation."
               :> "conversations"
               :> "one2one"
               :> "upsert"
               :> ReqBody '[Servant.JSON] UpsertOne2OneConversationRequest
               :> Post '[Servant.JSON] UpsertOne2OneConversationResponse
           )
    :<|> IFeatureAPI

type ILegalholdWhitelistedTeamsAPI =
  "legalhold"
    :> "whitelisted-teams"
    :> Capture "tid" TeamId
    :> ILegalholdWhitelistedTeamsAPIBase

type ILegalholdWhitelistedTeamsAPIBase =
  Named
    "set-team-legalhold-whitelisted"
    (MultiVerb1 'PUT '[Servant.JSON] (RespondEmpty 200 "Team Legalhold Whitelisted"))
    :<|> Named
           "unset-team-legalhold-whitelisted"
           (MultiVerb1 'DELETE '[Servant.JSON] (RespondEmpty 204 "Team Legalhold un-Whitelisted"))
    :<|> Named
           "get-team-legalhold-whitelisted"
           ( MultiVerb
               'GET
               '[Servant.JSON]
               '[ RespondEmpty 404 "Team not Legalhold Whitelisted",
                  RespondEmpty 200 "Team Legalhold Whitelisted"
                ]
               Bool
           )

type ITeamsAPI = "teams" :> Capture "tid" TeamId :> ITeamsAPIBase

type ITeamsAPIBase =
  Named "get-team-internal" (CanThrow 'TeamNotFound :> Get '[Servant.JSON] TeamData)
    :<|> Named
           "create-binding-team"
           ( ZUser
               :> ReqBody '[Servant.JSON] BindingNewTeam
               :> MultiVerb1
                    'PUT
                    '[Servant.JSON]
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
               :> MultiVerb1 'DELETE '[Servant.JSON] (RespondEmpty 202 "OK")
           )
    :<|> Named "get-team-name" ("name" :> CanThrow 'TeamNotFound :> Get '[Servant.JSON] TeamName)
    :<|> Named
           "update-team-status"
           ( "status"
               :> CanThrow 'TeamNotFound
               :> CanThrow 'InvalidTeamStatusUpdate
               :> ReqBody '[Servant.JSON] TeamStatusUpdate
               :> MultiVerb1 'PUT '[Servant.JSON] (RespondEmpty 200 "OK")
           )
    :<|> "members"
      :> ( Named
             "unchecked-add-team-member"
             ( CanThrow 'TooManyTeamMembers
                 :> CanThrow 'TooManyTeamMembersOnTeamWithLegalhold
                 :> ReqBody '[Servant.JSON] NewTeamMember
                 :> MultiVerb1 'POST '[Servant.JSON] (RespondEmpty 200 "OK")
             )
             :<|> Named
                    "unchecked-get-team-members"
                    ( QueryParam' '[Strict] "maxResults" (Range 1 HardTruncationLimit Int32)
                        :> Get '[Servant.JSON] TeamMemberList
                    )
             :<|> Named
                    "unchecked-get-team-member"
                    ( Capture "uid" UserId
                        :> CanThrow 'TeamMemberNotFound
                        :> Get '[Servant.JSON] TeamMember
                    )
             :<|> Named
                    "can-user-join-team"
                    ( "check"
                        :> CanThrow 'TooManyTeamMembersOnTeamWithLegalhold
                        :> MultiVerb1 'GET '[Servant.JSON] (RespondEmpty 200 "User can join")
                    )
             :<|> Named
                    "unchecked-update-team-member"
                    ( CanThrow 'AccessDenied
                        :> CanThrow 'InvalidPermissions
                        :> CanThrow 'TeamNotFound
                        :> CanThrow 'TeamMemberNotFound
                        :> CanThrow 'NotATeamMember
                        :> CanThrow OperationDenied
                        :> ReqBody '[Servant.JSON] NewTeamMember
                        :> MultiVerb1 'PUT '[Servant.JSON] (RespondEmpty 200 "")
                    )
         )
    :<|> Named
           "user-is-team-owner"
           ( "is-team-owner"
               :> Capture "uid" UserId
               :> CanThrow 'AccessDenied
               :> CanThrow 'TeamMemberNotFound
               :> CanThrow 'NotATeamMember
               :> MultiVerb1 'GET '[Servant.JSON] (RespondEmpty 200 "User is team owner")
           )
    :<|> "search-visibility"
      :> ( Named "get-search-visibility-internal" (Get '[Servant.JSON] TeamSearchVisibilityView)
             :<|> Named
                    "set-search-visibility-internal"
                    ( CanThrow 'TeamSearchVisibilityNotEnabled
                        :> CanThrow OperationDenied
                        :> CanThrow 'NotATeamMember
                        :> CanThrow 'TeamNotFound
                        :> ReqBody '[Servant.JSON] TeamSearchVisibilityView
                        :> MultiVerb1 'PUT '[Servant.JSON] (RespondEmpty 204 "OK")
                    )
         )

type IFeatureStatusGet f = Named '("iget", f) (FeatureStatusBaseGet f)

type IFeatureStatusPut calls errs f = Named '("iput", f) (ApplyMods calls (FeatureStatusBasePutInternal errs f))

type IFeatureStatusPatch calls errs f = Named '("ipatch", f) (ApplyMods calls (FeatureStatusBasePatchInternal errs f))

type FeatureStatusBasePutInternal errs featureConfig =
  FeatureStatusBaseInternal
    (AppendSymbol "Put config for " (FeatureSymbol featureConfig))
    errs
    featureConfig
    ( ReqBody '[Servant.JSON] (WithStatusNoLock featureConfig)
        :> Put '[Servant.JSON] (WithStatus featureConfig)
    )

type FeatureStatusBasePatchInternal errs featureConfig =
  FeatureStatusBaseInternal
    (AppendSymbol "Patch config for " (FeatureSymbol featureConfig))
    errs
    featureConfig
    ( ReqBody '[Servant.JSON] (WithStatusPatch featureConfig)
        :> Patch '[Servant.JSON] (WithStatus featureConfig)
    )

type FeatureStatusBaseInternal desc errs featureConfig a =
  Summary desc
    :> CanThrow OperationDenied
    :> CanThrow 'NotATeamMember
    :> CanThrow 'TeamNotFound
    :> CanThrow TeamFeatureError
    :> CanThrowMany errs
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> FeatureSymbol featureConfig
    :> a

type IFeatureStatusLockStatusPut featureName =
  Named
    '("ilock", featureName)
    ( Summary (AppendSymbol "(Un-)lock " (FeatureSymbol featureName))
        :> CanThrow 'NotATeamMember
        :> CanThrow 'TeamNotFound
        :> "teams"
        :> Capture "tid" TeamId
        :> "features"
        :> FeatureSymbol featureName
        :> Capture "lockStatus" LockStatus
        :> Put '[Servant.JSON] LockStatusResponse
    )

type FeatureNoConfigMultiGetBase featureName =
  Summary
    (AppendSymbol "Get team feature status in bulk for feature " (FeatureSymbol featureName))
    :> "features-multi-teams"
    :> FeatureSymbol featureName
    :> ReqBody '[Servant.JSON] TeamFeatureNoConfigMultiRequest
    :> Post '[Servant.JSON] (TeamFeatureNoConfigMultiResponse featureName)

type IFeatureNoConfigMultiGet f =
  Named
    '("igetmulti", f)
    (FeatureNoConfigMultiGetBase f)

swaggerDoc :: Swagger
swaggerDoc =
  toSwagger (Proxy @InternalAPI)
    & info . title .~ "Wire-Server internal galley API"
