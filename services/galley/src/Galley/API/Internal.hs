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

module Galley.API.Internal
  ( internalSitemap,
    internalAPI,
    InternalAPI,
    deleteLoop,
    safeForever,
  )
where

import Control.Exception.Safe (catchAny)
import Control.Lens hiding (Getter, Setter, (.=))
import Data.Id as Id
import Data.List1 (maybeList1)
import Data.Qualified
import Data.Range
import Data.Singletons
import Data.String.Conversions (cs)
import Data.Time
import GHC.TypeLits (AppendSymbol)
import qualified Galley.API.Clients as Clients
import qualified Galley.API.Create as Create
import qualified Galley.API.CustomBackend as CustomBackend
import Galley.API.LegalHold (unsetTeamLegalholdWhitelistedH)
import Galley.API.LegalHold.Conflicts
import Galley.API.One2One
import Galley.API.Public
import Galley.API.Public.Servant
import qualified Galley.API.Query as Query
import Galley.API.Teams (uncheckedDeleteTeamMember)
import qualified Galley.API.Teams as Teams
import Galley.API.Teams.Features
import qualified Galley.API.Update as Update
import Galley.API.Util
import Galley.App
import Galley.Cassandra.Paging
import Galley.Cassandra.TeamFeatures
import qualified Galley.Data.Conversation as Data
import Galley.Effects
import Galley.Effects.ClientStore
import Galley.Effects.ConversationStore
import Galley.Effects.FederatorAccess
import Galley.Effects.GundeckAccess
import Galley.Effects.LegalHoldStore as LegalHoldStore
import Galley.Effects.MemberStore
import Galley.Effects.Paging
import Galley.Effects.TeamStore
import qualified Galley.Intra.Push as Intra
import Galley.Monad
import Galley.Options
import qualified Galley.Queue as Q
import Galley.Types
import Galley.Types.Bot (AddBot, RemoveBot)
import Galley.Types.Bot.Service
import Galley.Types.Conversations.Intra (UpsertOne2OneConversationRequest (..), UpsertOne2OneConversationResponse (..))
import Galley.Types.Teams hiding (MemberLeave)
import Galley.Types.Teams.Intra
import Galley.Types.Teams.SearchVisibility
import Galley.Types.UserList
import Imports hiding (head)
import Network.Wai.Predicate hiding (Error, err)
import qualified Network.Wai.Predicate as Predicate
import Network.Wai.Routing hiding (App, route, toList)
import Network.Wai.Utilities hiding (Error)
import Network.Wai.Utilities.ZAuth
import Polysemy
import Polysemy.Error
import Polysemy.Input
import qualified Polysemy.TinyLog as P
import Servant hiding (JSON, WithStatus)
import qualified Servant hiding (WithStatus)
import System.Logger.Class hiding (Path, name)
import qualified System.Logger.Class as Log
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Action
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.Routes.API
import Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti
import Wire.API.Routes.MultiTablePaging (mtpHasMore, mtpPagingState, mtpResults)
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Routes.Public.Galley
import Wire.API.Team.Feature

type IFeatureAPI =
  -- SSOConfig
  IFeatureStatusGet SSOConfig
    :<|> IFeatureStatusPut '() SSOConfig
    -- LegalholdConfig
    :<|> IFeatureStatusGet LegalholdConfig
    :<|> IFeatureStatusPut
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
           LegalholdConfig
    -- SearchVisibilityAvailableConfig
    :<|> IFeatureStatusGet SearchVisibilityAvailableConfig
    :<|> IFeatureStatusPut '() SearchVisibilityAvailableConfig
    :<|> IFeatureStatusDeprecatedGet SearchVisibilityAvailableConfig
    :<|> IFeatureStatusDeprecatedPut SearchVisibilityAvailableConfig
    -- ValidateSAMLEmailsConfig
    :<|> IFeatureStatusGet ValidateSAMLEmailsConfig
    :<|> IFeatureStatusPut '() ValidateSAMLEmailsConfig
    :<|> IFeatureStatusDeprecatedGet ValidateSAMLEmailsConfig
    :<|> IFeatureStatusDeprecatedPut ValidateSAMLEmailsConfig
    -- DigitalSignaturesConfig
    :<|> IFeatureStatusGet DigitalSignaturesConfig
    :<|> IFeatureStatusPut '() DigitalSignaturesConfig
    :<|> IFeatureStatusDeprecatedGet DigitalSignaturesConfig
    :<|> IFeatureStatusDeprecatedPut DigitalSignaturesConfig
    -- AppLockConfig
    :<|> IFeatureStatusGet AppLockConfig
    :<|> IFeatureStatusPut '() AppLockConfig
    -- FileSharingConfig
    :<|> IFeatureStatusGet FileSharingConfig
    :<|> IFeatureStatusPut '() FileSharingConfig
    :<|> IFeatureStatusLockStatusPut FileSharingConfig
    -- ConferenceCallingConfig
    :<|> IFeatureStatusGet ConferenceCallingConfig
    :<|> IFeatureStatusPut '() ConferenceCallingConfig
    -- SelfDeletingMessagesConfig
    :<|> IFeatureStatusGet SelfDeletingMessagesConfig
    :<|> IFeatureStatusPut '() SelfDeletingMessagesConfig
    :<|> IFeatureStatusLockStatusPut SelfDeletingMessagesConfig
    -- GuestLinksConfig
    :<|> IFeatureStatusGet GuestLinksConfig
    :<|> IFeatureStatusPut '() GuestLinksConfig
    :<|> IFeatureStatusLockStatusPut GuestLinksConfig
    --  SndFactorPasswordChallengeConfig
    :<|> IFeatureStatusGet SndFactorPasswordChallengeConfig
    :<|> IFeatureStatusPut '() SndFactorPasswordChallengeConfig
    :<|> IFeatureStatusLockStatusPut SndFactorPasswordChallengeConfig
    -- SearchVisibilityInboundConfig
    :<|> IFeatureStatusGet SearchVisibilityInboundConfig
    :<|> IFeatureStatusPut '() SearchVisibilityInboundConfig
    :<|> IFeatureNoConfigMultiGet SearchVisibilityInboundConfig
    -- ClassifiedDomainsConfig
    :<|> IFeatureStatusGet ClassifiedDomainsConfig

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
    :<|> Named
           "feature-config-snd-factor-password-challenge"
           -- FUTUREWORK: Introduce `/i/feature-configs` and drop this one again.  The internal end-poins has the
           -- same handler as the public one, plus optional user id in the query.  Maybe require `DoAuth` to disable
           -- access control only on the internal end-point, not on the public one.  (This may also be a good oppportunity
           -- to make `AllFeatureConfigs` more type-safe.)
           ( Summary "Get feature config for the 2nd factor password challenge feature (for user/team; if n/a fall back to site config)."
               :> "feature-configs"
               :> CanThrow OperationDenied
               :> CanThrow 'NotATeamMember
               :> CanThrow 'TeamNotFound
               :> FeatureSymbol SndFactorPasswordChallengeConfig
               :> QueryParam'
                    [ Optional,
                      Strict,
                      Description "Optional user id"
                    ]
                    "user_id"
                    UserId
               :> Get '[Servant.JSON] (WithStatus SndFactorPasswordChallengeConfig)
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
           ( ZUser :> ReqBody '[Servant.JSON] BindingNewTeam
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
           ( "status" :> CanThrow 'TeamNotFound :> CanThrow 'InvalidTeamStatusUpdate
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
                    ( Capture "uid" UserId :> CanThrow 'TeamMemberNotFound
                        :> Get '[Servant.JSON] TeamMember
                    )
             :<|> Named
                    "can-user-join-team"
                    ( "check"
                        :> CanThrow 'TooManyTeamMembersOnTeamWithLegalhold
                        :> MultiVerb1 'GET '[Servant.JSON] (RespondEmpty 200 "User can join")
                    )
         )
    :<|> Named
           "user-is-team-owner"
           ( "is-team-owner" :> Capture "uid" UserId
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
                        :> ReqBody '[Servant.JSON] TeamSearchVisibilityView
                        :> MultiVerb1 'PUT '[Servant.JSON] (RespondEmpty 204 "OK")
                    )
         )

type IFeatureStatusGet f = Named '("iget", f) (FeatureStatusBaseGet f)

type IFeatureStatusPut errs f = Named '("iput", f) (FeatureStatusBasePutInternal errs f)

type IFeatureStatusDeprecatedGet f = Named '("iget-deprecated", f) (FeatureStatusBaseDeprecatedGet f)

type IFeatureStatusDeprecatedPut f = Named '("iput-deprecated", f) (FeatureStatusBaseDeprecatedPut f)

type IFeatureStatusLockStatusPut featureName =
  Named
    '("lock", featureName)
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

internalAPI :: API InternalAPI GalleyEffects
internalAPI =
  hoistAPI @InternalAPIBase id $
    mkNamedAPI @"status" (pure ())
      <@> mkNamedAPI @"delete-user" rmUser
      <@> mkNamedAPI @"connect" Create.createConnectConversation
      <@> mkNamedAPI @"guard-legalhold-policy-conflicts" guardLegalholdPolicyConflictsH
      <@> legalholdWhitelistedTeamsAPI
      <@> iTeamsAPI
      <@> mkNamedAPI @"upsert-one2one" iUpsertOne2OneConversation
      <@> mkNamedAPI @"feature-config-snd-factor-password-challenge" (getFeatureStatusNoPermissionCheck @Cassandra)
      <@> featureAPI

legalholdWhitelistedTeamsAPI :: API ILegalholdWhitelistedTeamsAPI GalleyEffects
legalholdWhitelistedTeamsAPI = mkAPI $ \tid -> hoistAPIHandler id (base tid)
  where
    base :: TeamId -> API ILegalholdWhitelistedTeamsAPIBase GalleyEffects
    base tid =
      mkNamedAPI @"set-team-legalhold-whitelisted" (LegalHoldStore.setTeamLegalholdWhitelisted tid)
        <@> mkNamedAPI @"unset-team-legalhold-whitelisted" (unsetTeamLegalholdWhitelistedH tid)
        <@> mkNamedAPI @"get-team-legalhold-whitelisted" (LegalHoldStore.isTeamLegalholdWhitelisted tid)

iTeamsAPI :: API ITeamsAPI GalleyEffects
iTeamsAPI = mkAPI $ \tid -> hoistAPIHandler id (base tid)
  where
    hoistAPISegment ::
      (ServerT (seg :> inner) (Sem r) ~ ServerT inner (Sem r)) =>
      API inner r ->
      API (seg :> inner) r
    hoistAPISegment = hoistAPI id

    base :: TeamId -> API ITeamsAPIBase GalleyEffects
    base tid =
      mkNamedAPI @"get-team-internal" (Teams.getTeamInternalH tid)
        <@> mkNamedAPI @"create-binding-team" (Teams.createBindingTeam tid)
        <@> mkNamedAPI @"delete-binding-team" (Teams.internalDeleteBindingTeam tid)
        <@> mkNamedAPI @"get-team-name" (Teams.getTeamNameInternalH tid)
        <@> mkNamedAPI @"update-team-status" (Teams.updateTeamStatus tid)
        <@> hoistAPISegment
          ( mkNamedAPI @"unchecked-add-team-member" (Teams.uncheckedAddTeamMember @Cassandra tid)
              <@> mkNamedAPI @"unchecked-get-team-members" (Teams.uncheckedGetTeamMembersH tid)
              <@> mkNamedAPI @"unchecked-get-team-member" (Teams.uncheckedGetTeamMember tid)
              <@> mkNamedAPI @"can-user-join-team" (Teams.canUserJoinTeam @Cassandra tid)
          )
        <@> mkNamedAPI @"user-is-team-owner" (Teams.userIsTeamOwner tid)
        <@> hoistAPISegment
          ( mkNamedAPI @"get-search-visibility-internal" (Teams.getSearchVisibilityInternal tid)
              <@> mkNamedAPI @"set-search-visibility-internal" (Teams.setSearchVisibilityInternal @Cassandra tid)
          )

featureAPI :: API IFeatureAPI GalleyEffects
featureAPI =
  mkNamedAPI (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI (\tid ws ttl -> setFeatureStatus @Cassandra ttl DontDoAuth tid ws)
    <@> mkNamedAPI (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI (\tid ws ttl -> setFeatureStatus @Cassandra ttl DontDoAuth tid ws)
    <@> mkNamedAPI (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI (\tid ws ttl -> setFeatureStatus @Cassandra ttl DontDoAuth tid ws)
    <@> mkNamedAPI (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI (setFeatureStatus @Cassandra Nothing DontDoAuth)
    <@> mkNamedAPI (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI (\tid ws ttl -> setFeatureStatus @Cassandra ttl DontDoAuth tid ws)
    <@> mkNamedAPI (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI (setFeatureStatus @Cassandra Nothing DontDoAuth)
    <@> mkNamedAPI (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI (\tid ws ttl -> setFeatureStatus @Cassandra ttl DontDoAuth tid ws)
    <@> mkNamedAPI (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI (setFeatureStatus @Cassandra Nothing DontDoAuth)
    <@> mkNamedAPI (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI (\tid ws ttl -> setFeatureStatus @Cassandra ttl DontDoAuth tid ws)
    <@> mkNamedAPI (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI (\tid ws ttl -> setFeatureStatus @Cassandra ttl DontDoAuth tid ws)
    <@> mkNamedAPI (setLockStatus @Cassandra @FileSharingConfig)
    <@> mkNamedAPI (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI (\tid ws ttl -> setFeatureStatus @Cassandra ttl DontDoAuth tid ws)
    <@> mkNamedAPI (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI (\tid ws ttl -> setFeatureStatus @Cassandra ttl DontDoAuth tid ws)
    <@> mkNamedAPI (setLockStatus @Cassandra @SelfDeletingMessagesConfig)
    <@> mkNamedAPI (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI (\tid ws ttl -> setFeatureStatus @Cassandra ttl DontDoAuth tid ws)
    <@> mkNamedAPI (setLockStatus @Cassandra @GuestLinksConfig)
    <@> mkNamedAPI (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI (\tid ws ttl -> setFeatureStatus @Cassandra ttl DontDoAuth tid ws)
    <@> mkNamedAPI (setLockStatus @Cassandra @SndFactorPasswordChallengeConfig)
    <@> mkNamedAPI (getFeatureStatus @Cassandra DontDoAuth)
    <@> mkNamedAPI (\tid ws ttl -> setFeatureStatus @Cassandra ttl DontDoAuth tid ws)
    <@> mkNamedAPI (getFeatureStatusMulti @Cassandra @SearchVisibilityInboundConfig)
    <@> mkNamedAPI (getFeatureStatus @Cassandra DontDoAuth)

internalSitemap :: Routes a (Sem GalleyEffects) ()
internalSitemap = do
  -- Conversation API (internal) ----------------------------------------
  put "/i/conversations/:cnv/channel" (continue $ const (pure empty)) $
    zauthUserId
      .&. (capture "cnv" :: HasCaptures r => Predicate r Predicate.Error ConvId)
      .&. request

  get "/i/conversations/:cnv/members/:usr" (continue Query.internalGetMemberH) $
    capture "cnv"
      .&. capture "usr"

  -- This endpoint can lead to the following events being sent:
  -- - MemberJoin event to you, if the conversation existed and had < 2 members before
  -- - MemberJoin event to other, if the conversation existed and only the other was member
  --   before
  put "/i/conversations/:cnv/accept/v2" (continueE Update.acceptConvH) $
    zauthUserId
      .&. opt zauthConnId
      .&. capture "cnv"

  put "/i/conversations/:cnv/block" (continueE Update.blockConvH) $
    zauthUserId
      .&. capture "cnv"

  -- This endpoint can lead to the following events being sent:
  -- - MemberJoin event to you, if the conversation existed and had < 2 members before
  -- - MemberJoin event to other, if the conversation existed and only the other was member
  --   before
  put "/i/conversations/:cnv/unblock" (continueE Update.unblockConvH) $
    zauthUserId
      .&. opt zauthConnId
      .&. capture "cnv"

  get "/i/conversations/:cnv/meta" (continue Query.getConversationMetaH) $
    capture "cnv"

  -- Misc API (internal) ------------------------------------------------

  get "/i/users/:uid/team/members" (continueE Teams.getBindingTeamMembersH) $
    capture "uid"

  get "/i/users/:uid/team" (continueE Teams.getBindingTeamIdH) $
    capture "uid"

  get "/i/test/clients" (continueE Clients.getClientsH) $
    zauthUserId
  -- eg. https://github.com/wireapp/wire-server/blob/3bdca5fc8154e324773802a0deb46d884bd09143/services/brig/test/integration/API/User/Client.hs#L319

  post "/i/clients/:client" (continue Clients.addClientH) $
    zauthUserId
      .&. capture "client"

  delete "/i/clients/:client" (continue Clients.rmClientH) $
    zauthUserId
      .&. capture "client"

  post "/i/services" (continue Update.addServiceH) $
    jsonRequest @Service

  delete "/i/services" (continue Update.rmServiceH) $
    jsonRequest @ServiceRef

  -- This endpoint can lead to the following events being sent:
  -- - MemberJoin event to members
  post "/i/bots" (continueE Update.addBotH) $
    zauthUserId
      .&. zauthConnId
      .&. jsonRequest @AddBot

  -- This endpoint can lead to the following events being sent:
  -- - MemberLeave event to members
  delete "/i/bots" (continueE Update.rmBotH) $
    zauthUserId
      .&. opt zauthConnId
      .&. jsonRequest @RemoveBot

  put "/i/custom-backend/by-domain/:domain" (continue CustomBackend.internalPutCustomBackendByDomainH) $
    capture "domain"
      .&. jsonRequest @CustomBackend

  delete "/i/custom-backend/by-domain/:domain" (continue CustomBackend.internalDeleteCustomBackendByDomainH) $
    capture "domain"
      .&. accept "application" "json"

rmUser ::
  forall p1 p2 r.
  ( p1 ~ CassandraPaging,
    p2 ~ InternalPaging,
    Members
      '[ BrigAccess,
         ClientStore,
         ConversationStore,
         ExternalAccess,
         FederatorAccess,
         GundeckAccess,
         Input UTCTime,
         ListItems p1 ConvId,
         ListItems p1 (Remote ConvId),
         ListItems p2 TeamId,
         MemberStore,
         TeamStore,
         P.TinyLog
       ]
      r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  Sem r ()
rmUser lusr conn = do
  let nRange1000 = toRange (Proxy @1000) :: Range 1 1000 Int32
  tids <- listTeams (tUnqualified lusr) Nothing maxBound
  leaveTeams tids
  allConvIds <- Query.conversationIdsPageFrom lusr (GetPaginatedConversationIds Nothing nRange1000)
  goConvPages nRange1000 allConvIds

  deleteClients (tUnqualified lusr)
  where
    goConvPages :: Range 1 1000 Int32 -> ConvIdsPage -> Sem r ()
    goConvPages range page = do
      let (localConvs, remoteConvs) = partitionQualified lusr (mtpResults page)
      leaveLocalConversations localConvs
      traverse_ leaveRemoteConversations (rangedChunks remoteConvs)
      when (mtpHasMore page) $ do
        let nextState = mtpPagingState page
            nextQuery = GetPaginatedConversationIds (Just nextState) range
        newCids <- Query.conversationIdsPageFrom lusr nextQuery
        goConvPages range newCids

    leaveTeams page = for_ (pageItems page) $ \tid -> do
      mems <- getTeamMembersForFanout tid
      uncheckedDeleteTeamMember lusr conn tid (tUnqualified lusr) mems
      page' <- listTeams @p2 (tUnqualified lusr) (Just (pageState page)) maxBound
      leaveTeams page'

    leaveLocalConversations :: [ConvId] -> Sem r ()
    leaveLocalConversations ids = do
      let qUser = qUntagged lusr
      cc <- getConversations ids
      now <- input
      pp <- for cc $ \c -> case Data.convType c of
        SelfConv -> pure Nothing
        One2OneConv -> deleteMembers (Data.convId c) (UserList [tUnqualified lusr] []) $> Nothing
        ConnectConv -> deleteMembers (Data.convId c) (UserList [tUnqualified lusr] []) $> Nothing
        RegularConv
          | tUnqualified lusr `isMember` Data.convLocalMembers c -> do
            deleteMembers (Data.convId c) (UserList [tUnqualified lusr] [])
            let e =
                  Event
                    (qUntagged (qualifyAs lusr (Data.convId c)))
                    (qUntagged lusr)
                    now
                    (EdMembersLeave (QualifiedUserIdList [qUser]))
            for_ (bucketRemote (fmap rmId (Data.convRemoteMembers c))) $ notifyRemoteMembers now qUser (Data.convId c)
            pure $
              Intra.newPushLocal ListComplete (tUnqualified lusr) (Intra.ConvEvent e) (Intra.recipient <$> Data.convLocalMembers c)
                <&> set Intra.pushConn conn
                  . set Intra.pushRoute Intra.RouteDirect
          | otherwise -> pure Nothing

      for_
        (maybeList1 (catMaybes pp))
        push

    -- FUTUREWORK: This could be optimized to reduce the number of RPCs
    -- made. When a team is deleted the burst of RPCs created here could
    -- lead to performance issues. We should cover this in a performance
    -- test.
    notifyRemoteMembers :: UTCTime -> Qualified UserId -> ConvId -> Remote [UserId] -> Sem r ()
    notifyRemoteMembers now qUser cid remotes = do
      let convUpdate =
            ConversationUpdate
              { cuTime = now,
                cuOrigUserId = qUser,
                cuConvId = cid,
                cuAlreadyPresentUsers = tUnqualified remotes,
                cuAction = SomeConversationAction (sing @'ConversationLeaveTag) (pure qUser)
              }
      let rpc = fedClient @'Galley @"on-conversation-updated" convUpdate
      runFederatedEither remotes rpc
        >>= logAndIgnoreError "Error in onConversationUpdated call" (qUnqualified qUser)

    leaveRemoteConversations :: Range 1 UserDeletedNotificationMaxConvs [Remote ConvId] -> Sem r ()
    leaveRemoteConversations cids = do
      for_ (bucketRemote (fromRange cids)) $ \remoteConvs -> do
        let userDelete = UserDeletedConversationsNotification (tUnqualified lusr) (unsafeRange (tUnqualified remoteConvs))
        let rpc = fedClient @'Galley @"on-user-deleted-conversations" userDelete
        runFederatedEither remoteConvs rpc
          >>= logAndIgnoreError "Error in onUserDeleted call" (tUnqualified lusr)

    -- FUTUREWORK: Add a retry mechanism if there are federation errrors.
    -- See https://wearezeta.atlassian.net/browse/SQCORE-1091
    logAndIgnoreError :: Text -> UserId -> Either FederationError a -> Sem r ()
    logAndIgnoreError message usr res = do
      case res of
        Left federationError ->
          P.err
            ( Log.msg
                ( "Federation error while notifying remote backends of a user deletion (Galley). "
                    <> message
                    <> " "
                    <> (cs . show $ federationError)
                )
                . Log.field "user" (show usr)
            )
        Right _ -> pure ()

deleteLoop :: App ()
deleteLoop = do
  q <- view deleteQueue
  safeForever "deleteLoop" $ do
    i@(TeamItem tid usr con) <- Q.pop q
    env <- ask
    liftIO (evalGalley env (doDelete usr con tid))
      `catchAny` someError q i
  where
    someError q i x = do
      err $ "error" .= show x ~~ msg (val "failed to delete")
      ok <- Q.tryPush q i
      unless ok $
        err (msg (val "delete queue is full, dropping item") ~~ "item" .= show i)
      liftIO $ threadDelay 1000000

    doDelete usr con tid = do
      lusr <- qualifyLocal usr
      Teams.uncheckedDeleteTeam lusr con tid

safeForever :: String -> App () -> App ()
safeForever funName action =
  forever $
    action `catchAny` \exc -> do
      err $ "error" .= show exc ~~ msg (val $ cs funName <> " failed")
      threadDelay 60000000 -- pause to keep worst-case noise in logs manageable

guardLegalholdPolicyConflictsH ::
  Members
    '[ BrigAccess,
       Input Opts,
       TeamStore,
       P.TinyLog,
       WaiRoutes,
       ErrorS 'MissingLegalholdConsent
     ]
    r =>
  GuardLegalholdPolicyConflicts ->
  Sem r ()
guardLegalholdPolicyConflictsH glh = do
  mapError @LegalholdConflicts (const $ Tagged @'MissingLegalholdConsent ()) $
    guardLegalholdPolicyConflicts (glhProtectee glh) (glhUserClients glh)
