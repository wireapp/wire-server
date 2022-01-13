-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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
  ( sitemap,
    servantSitemap,
    InternalApi,
    ServantAPI,
    deleteLoop,
    safeForever,
  )
where

import Control.Exception.Safe (catchAny)
import Control.Lens hiding ((.=))
import Data.Data (Proxy (Proxy))
import Data.Id as Id
import Data.List1 (maybeList1)
import Data.Qualified
import Data.Range
import Data.String.Conversions (cs)
import Data.Time
import GHC.TypeLits (AppendSymbol)
import qualified Galley.API.Clients as Clients
import qualified Galley.API.Create as Create
import qualified Galley.API.CustomBackend as CustomBackend
import Galley.API.Error
import Galley.API.LegalHold (getTeamLegalholdWhitelistedH, setTeamLegalholdWhitelistedH, unsetTeamLegalholdWhitelistedH)
import Galley.API.LegalHold.Conflicts
import qualified Galley.API.One2One as One2One
import qualified Galley.API.Query as Query
import Galley.API.Teams (uncheckedDeleteTeamMember)
import qualified Galley.API.Teams as Teams
import Galley.API.Teams.Features (DoAuth (..))
import qualified Galley.API.Teams.Features as Features
import qualified Galley.API.Update as Update
import Galley.API.Util
import Galley.App
import Galley.Cassandra.Paging
import qualified Galley.Data.Conversation as Data
import Galley.Data.TeamFeatures (MaybeHasLockStatusCol)
import Galley.Effects
import Galley.Effects.ClientStore
import Galley.Effects.ConversationStore
import Galley.Effects.FederatorAccess
import Galley.Effects.GundeckAccess
import Galley.Effects.MemberStore
import Galley.Effects.Paging
import Galley.Effects.TeamStore
import Galley.Effects.WaiRoutes
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
import Network.HTTP.Types (status200)
import Network.Wai
import Network.Wai.Predicate hiding (Error, err)
import qualified Network.Wai.Predicate as Predicate
import Network.Wai.Routing hiding (App, route, toList)
import Network.Wai.Utilities hiding (Error)
import Network.Wai.Utilities.ZAuth
import Polysemy
import Polysemy.Error
import Polysemy.Input
import qualified Polysemy.TinyLog as P
import Servant.API hiding (JSON)
import qualified Servant.API as Servant
import Servant.API.Generic
import Servant.Server
import Servant.Server.Generic (genericServerT)
import System.Logger.Class hiding (Path, name)
import qualified System.Logger.Class as Log
import Wire.API.Conversation (ConvIdsPage, pattern GetPaginatedConversationIds)
import Wire.API.Conversation.Action (ConversationAction (ConversationActionRemoveMembers))
import Wire.API.ErrorDescription
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.Routes.MultiTablePaging (mtpHasMore, mtpPagingState, mtpResults)
import Wire.API.Routes.MultiVerb (MultiVerb, RespondEmpty)
import Wire.API.Routes.Public (ZLocalUser, ZOptConn)
import Wire.API.Routes.Public.Galley (ConversationVerb)
import qualified Wire.API.Team.Feature as Public

data InternalApi routes = InternalApi
  { iStatusGet ::
      routes
        :- "i"
        :> "status"
        :> Get '[Servant.JSON] NoContent,
    iStatusHead ::
      routes
        :- "i"
        :> "status"
        :> Verb 'HEAD 200 '[Servant.JSON] NoContent,
    -- Team Feature Flag API (internal) -----------------------------------
    --
    -- Configuring some features should only be possible internally.
    -- Viewing the config for features should be allowed for any admin.
    iTeamFeatureStatusSSOGet ::
      routes
        :- IFeatureStatusGet 'Public.WithoutLockStatus 'Public.TeamFeatureSSO,
    iTeamFeatureStatusSSOPut ::
      routes
        :- IFeatureStatusPut 'Public.TeamFeatureSSO,
    iTeamFeatureStatusLegalHoldGet ::
      routes
        :- IFeatureStatusGet 'Public.WithoutLockStatus 'Public.TeamFeatureLegalHold,
    iTeamFeatureStatusLegalHoldPut ::
      routes
        :- IFeatureStatusPut 'Public.TeamFeatureLegalHold,
    iTeamFeatureStatusSearchVisibilityGet ::
      routes
        :- IFeatureStatusGet 'Public.WithoutLockStatus 'Public.TeamFeatureSearchVisibility,
    iTeamFeatureStatusSearchVisibilityPut ::
      routes
        :- IFeatureStatusPut 'Public.TeamFeatureSearchVisibility,
    iTeamFeatureStatusSearchVisibilityDeprecatedGet ::
      routes
        :- IFeatureStatusDeprecatedGet 'Public.WithoutLockStatus 'Public.TeamFeatureSearchVisibility,
    iTeamFeatureStatusSearchVisibilityDeprecatedPut ::
      routes
        :- IFeatureStatusDeprecatedPut 'Public.TeamFeatureSearchVisibility,
    iTeamFeatureStatusValidateSAMLEmailsGet ::
      routes
        :- IFeatureStatusGet 'Public.WithoutLockStatus 'Public.TeamFeatureValidateSAMLEmails,
    iTeamFeatureStatusValidateSAMLEmailsPut ::
      routes
        :- IFeatureStatusPut 'Public.TeamFeatureValidateSAMLEmails,
    iTeamFeatureStatusValidateSAMLEmailsDeprecatedGet ::
      routes
        :- IFeatureStatusDeprecatedGet 'Public.WithoutLockStatus 'Public.TeamFeatureValidateSAMLEmails,
    iTeamFeatureStatusValidateSAMLEmailsDeprecatedPut ::
      routes
        :- IFeatureStatusDeprecatedPut 'Public.TeamFeatureValidateSAMLEmails,
    iTeamFeatureStatusDigitalSignaturesGet ::
      routes
        :- IFeatureStatusGet 'Public.WithoutLockStatus 'Public.TeamFeatureDigitalSignatures,
    iTeamFeatureStatusDigitalSignaturesPut ::
      routes
        :- IFeatureStatusPut 'Public.TeamFeatureDigitalSignatures,
    iTeamFeatureStatusDigitalSignaturesDeprecatedGet ::
      routes
        :- IFeatureStatusDeprecatedGet 'Public.WithoutLockStatus 'Public.TeamFeatureDigitalSignatures,
    iTeamFeatureStatusDigitalSignaturesDeprecatedPut ::
      routes
        :- IFeatureStatusDeprecatedPut 'Public.TeamFeatureDigitalSignatures,
    iTeamFeatureStatusAppLockGet ::
      routes
        :- IFeatureStatusGet 'Public.WithoutLockStatus 'Public.TeamFeatureAppLock,
    iTeamFeatureStatusAppLockPut ::
      routes
        :- IFeatureStatusPut 'Public.TeamFeatureAppLock,
    iTeamFeatureStatusFileSharingGet ::
      routes
        :- IFeatureStatusGet 'Public.WithoutLockStatus 'Public.TeamFeatureFileSharing,
    iTeamFeatureStatusFileSharingPut ::
      routes
        :- IFeatureStatusPut 'Public.TeamFeatureFileSharing,
    iTeamFeatureStatusClassifiedDomainsGet ::
      routes
        :- IFeatureStatusGet 'Public.WithoutLockStatus 'Public.TeamFeatureClassifiedDomains,
    iTeamFeatureStatusConferenceCallingPut ::
      routes
        :- IFeatureStatusPut 'Public.TeamFeatureConferenceCalling,
    iTeamFeatureStatusConferenceCallingGet ::
      routes
        :- IFeatureStatusGet 'Public.WithoutLockStatus 'Public.TeamFeatureConferenceCalling,
    iTeamFeatureStatusSelfDeletingMessagesPut ::
      routes
        :- IFeatureStatusPut 'Public.TeamFeatureSelfDeletingMessages,
    iTeamFeatureStatusSelfDeletingMessagesGet ::
      routes
        :- IFeatureStatusGet 'Public.WithLockStatus 'Public.TeamFeatureSelfDeletingMessages,
    iTeamFeatureLockStatusSelfDeletingMessagesPut ::
      routes
        :- IFeatureStatusLockStatusPut 'Public.TeamFeatureSelfDeletingMessages,
    iTeamFeatureStatusGuestLinksGet ::
      routes
        :- IFeatureStatusGet 'Public.WithLockStatus 'Public.TeamFeatureGuestLinks,
    iTeamFeatureStatusGuestLinksPut ::
      routes
        :- IFeatureStatusPut 'Public.TeamFeatureGuestLinks,
    iTeamFeatureLockStatusGuestLinksPut ::
      routes
        :- IFeatureStatusLockStatusPut 'Public.TeamFeatureGuestLinks,
    -- This endpoint can lead to the following events being sent:
    -- - MemberLeave event to members for all conversations the user was in
    iDeleteUser ::
      routes
        :- Summary
             "Remove a user from their teams and conversations and erase their clients"
        :> ZLocalUser
        :> ZOptConn
        :> "i"
        :> "user"
        :> MultiVerb 'DELETE '[Servant.JSON] '[RespondEmpty 200 "Remove a user from Galley"] (),
    -- This endpoint can lead to the following events being sent:
    -- - ConvCreate event to self, if conversation did not exist before
    -- - ConvConnect event to self, if other didn't join the connect conversation before
    iConnect ::
      routes
        :- Summary "Create a connect conversation (deprecated)"
        :> ZLocalUser
        :> ZOptConn
        :> "i"
        :> "conversations"
        :> "connect"
        :> ReqBody '[Servant.JSON] Connect
        :> ConversationVerb,
    iUpsertOne2OneConversation ::
      routes
        :- Summary "Create or Update a connect or one2one conversation."
        :> "i"
        :> "conversations"
        :> "one2one"
        :> "upsert"
        :> ReqBody '[Servant.JSON] UpsertOne2OneConversationRequest
        :> Post '[Servant.JSON] UpsertOne2OneConversationResponse
  }
  deriving (Generic)

type ServantAPI = ToServantApi InternalApi

type IFeatureStatusGet lockStatus featureName =
  Summary (AppendSymbol "Get config for " (Public.KnownTeamFeatureNameSymbol featureName))
    :> "i"
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> Public.KnownTeamFeatureNameSymbol featureName
    :> Get '[Servant.JSON] (Public.TeamFeatureStatus lockStatus featureName)

type IFeatureStatusPut featureName =
  Summary (AppendSymbol "Put config for " (Public.KnownTeamFeatureNameSymbol featureName))
    :> "i"
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> Public.KnownTeamFeatureNameSymbol featureName
    :> ReqBody '[Servant.JSON] (Public.TeamFeatureStatus 'Public.WithoutLockStatus featureName)
    :> Put '[Servant.JSON] (Public.TeamFeatureStatus 'Public.WithoutLockStatus featureName)

type IFeatureStatusLockStatusPut featureName =
  Summary (AppendSymbol "(Un-)lock " (Public.KnownTeamFeatureNameSymbol featureName))
    :> "i"
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> Public.KnownTeamFeatureNameSymbol featureName
    :> Capture "lockStatus" Public.LockStatusValue
    :> Put '[Servant.JSON] Public.LockStatus

-- | A type for a GET endpoint for a feature with a deprecated path
type IFeatureStatusDeprecatedGet lockStatus featureName =
  Summary (AppendSymbol "[deprecated] Get config for " (Public.KnownTeamFeatureNameSymbol featureName))
    :> "i"
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> Public.DeprecatedFeatureName featureName
    :> Get '[Servant.JSON] (Public.TeamFeatureStatus lockStatus featureName)

-- | A type for a PUT endpoint for a feature with a deprecated path
type IFeatureStatusDeprecatedPut featureName =
  Summary (AppendSymbol "[deprecated] Put config for " (Public.KnownTeamFeatureNameSymbol featureName))
    :> "i"
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> Public.DeprecatedFeatureName featureName
    :> ReqBody '[Servant.JSON] (Public.TeamFeatureStatus 'Public.WithoutLockStatus featureName)
    :> Put '[Servant.JSON] (Public.TeamFeatureStatus 'Public.WithoutLockStatus featureName)

servantSitemap :: ServerT ServantAPI (Sem GalleyEffects)
servantSitemap =
  genericServerT $
    InternalApi
      { iStatusGet = pure NoContent,
        iStatusHead = pure NoContent,
        iTeamFeatureStatusSSOGet = iGetTeamFeature @'Public.WithoutLockStatus @'Public.TeamFeatureSSO Features.getSSOStatusInternal,
        iTeamFeatureStatusSSOPut = iPutTeamFeature @'Public.TeamFeatureSSO Features.setSSOStatusInternal,
        iTeamFeatureStatusLegalHoldGet = iGetTeamFeature @'Public.WithoutLockStatus @'Public.TeamFeatureLegalHold Features.getLegalholdStatusInternal,
        iTeamFeatureStatusLegalHoldPut = iPutTeamFeature @'Public.TeamFeatureLegalHold (Features.setLegalholdStatusInternal @InternalPaging),
        iTeamFeatureStatusSearchVisibilityGet = iGetTeamFeature @'Public.WithoutLockStatus @'Public.TeamFeatureSearchVisibility Features.getTeamSearchVisibilityAvailableInternal,
        iTeamFeatureStatusSearchVisibilityPut = iPutTeamFeature @'Public.TeamFeatureLegalHold Features.setTeamSearchVisibilityAvailableInternal,
        iTeamFeatureStatusSearchVisibilityDeprecatedGet = iGetTeamFeature @'Public.WithoutLockStatus @'Public.TeamFeatureSearchVisibility Features.getTeamSearchVisibilityAvailableInternal,
        iTeamFeatureStatusSearchVisibilityDeprecatedPut = iPutTeamFeature @'Public.TeamFeatureLegalHold Features.setTeamSearchVisibilityAvailableInternal,
        iTeamFeatureStatusValidateSAMLEmailsGet = iGetTeamFeature @'Public.WithoutLockStatus @'Public.TeamFeatureValidateSAMLEmails Features.getValidateSAMLEmailsInternal,
        iTeamFeatureStatusValidateSAMLEmailsPut = iPutTeamFeature @'Public.TeamFeatureValidateSAMLEmails Features.setValidateSAMLEmailsInternal,
        iTeamFeatureStatusValidateSAMLEmailsDeprecatedGet = iGetTeamFeature @'Public.WithoutLockStatus @'Public.TeamFeatureValidateSAMLEmails Features.getValidateSAMLEmailsInternal,
        iTeamFeatureStatusValidateSAMLEmailsDeprecatedPut = iPutTeamFeature @'Public.TeamFeatureValidateSAMLEmails Features.setValidateSAMLEmailsInternal,
        iTeamFeatureStatusDigitalSignaturesGet = iGetTeamFeature @'Public.WithoutLockStatus @'Public.TeamFeatureDigitalSignatures Features.getDigitalSignaturesInternal,
        iTeamFeatureStatusDigitalSignaturesPut = iPutTeamFeature @'Public.TeamFeatureDigitalSignatures Features.setDigitalSignaturesInternal,
        iTeamFeatureStatusDigitalSignaturesDeprecatedGet = iGetTeamFeature @'Public.WithoutLockStatus @'Public.TeamFeatureDigitalSignatures Features.getDigitalSignaturesInternal,
        iTeamFeatureStatusDigitalSignaturesDeprecatedPut = iPutTeamFeature @'Public.TeamFeatureDigitalSignatures Features.setDigitalSignaturesInternal,
        iTeamFeatureStatusAppLockGet = iGetTeamFeature @'Public.WithoutLockStatus @'Public.TeamFeatureAppLock Features.getAppLockInternal,
        iTeamFeatureStatusAppLockPut = iPutTeamFeature @'Public.TeamFeatureAppLock Features.setAppLockInternal,
        iTeamFeatureStatusFileSharingGet = iGetTeamFeature @'Public.WithoutLockStatus @'Public.TeamFeatureFileSharing Features.getFileSharingInternal,
        iTeamFeatureStatusFileSharingPut = iPutTeamFeature @'Public.TeamFeatureFileSharing Features.setFileSharingInternal,
        iTeamFeatureStatusClassifiedDomainsGet = iGetTeamFeature @'Public.WithoutLockStatus @'Public.TeamFeatureClassifiedDomains Features.getClassifiedDomainsInternal,
        iTeamFeatureStatusConferenceCallingPut = iPutTeamFeature @'Public.TeamFeatureConferenceCalling Features.setConferenceCallingInternal,
        iTeamFeatureStatusConferenceCallingGet = iGetTeamFeature @'Public.WithoutLockStatus @'Public.TeamFeatureConferenceCalling Features.getConferenceCallingInternal,
        iTeamFeatureStatusSelfDeletingMessagesPut = iPutTeamFeature @'Public.TeamFeatureSelfDeletingMessages Features.setSelfDeletingMessagesInternal,
        iTeamFeatureStatusSelfDeletingMessagesGet = iGetTeamFeature @'Public.WithLockStatus @'Public.TeamFeatureSelfDeletingMessages Features.getSelfDeletingMessagesInternal,
        iTeamFeatureLockStatusSelfDeletingMessagesPut = Features.setLockStatus @'Public.TeamFeatureSelfDeletingMessages,
        iTeamFeatureStatusGuestLinksGet = iGetTeamFeature @'Public.WithLockStatus @'Public.TeamFeatureGuestLinks Features.getGuestLinkInternal,
        iTeamFeatureStatusGuestLinksPut = iPutTeamFeature @'Public.TeamFeatureGuestLinks Features.setGuestLinkInternal,
        iTeamFeatureLockStatusGuestLinksPut = Features.setLockStatus @'Public.TeamFeatureGuestLinks,
        iDeleteUser = rmUser,
        iConnect = Create.createConnectConversation,
        iUpsertOne2OneConversation = One2One.iUpsertOne2OneConversation
      }

iGetTeamFeature ::
  forall ps a r.
  ( Public.KnownTeamFeatureName a,
    Members
      '[ Error ActionError,
         Error NotATeamMember,
         Error TeamError,
         TeamStore
       ]
      r
  ) =>
  (Features.GetFeatureInternalParam -> Sem r (Public.TeamFeatureStatus ps a)) ->
  TeamId ->
  Sem r (Public.TeamFeatureStatus ps a)
iGetTeamFeature getter = Features.getFeatureStatus @ps @a getter DontDoAuth

iPutTeamFeature ::
  forall a r.
  ( Public.KnownTeamFeatureName a,
    MaybeHasLockStatusCol a,
    Members
      '[ Error ActionError,
         Error NotATeamMember,
         Error TeamError,
         Error TeamFeatureError,
         TeamStore,
         TeamFeatureStore
       ]
      r
  ) =>
  (TeamId -> Public.TeamFeatureStatus 'Public.WithoutLockStatus a -> Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus a)) ->
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutLockStatus a ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus a)
iPutTeamFeature setter = Features.setFeatureStatus @a setter DontDoAuth

sitemap :: Routes a (Sem GalleyEffects) ()
sitemap = do
  -- Conversation API (internal) ----------------------------------------

  put "/i/conversations/:cnv/channel" (continue $ const (return empty)) $
    zauthUserId
      .&. (capture "cnv" :: HasCaptures r => Predicate r Predicate.Error ConvId)
      .&. request

  get "/i/conversations/:cnv/members/:usr" (continue Query.internalGetMemberH) $
    capture "cnv"
      .&. capture "usr"

  -- This endpoint can lead to the following events being sent:
  -- - ConvCreate event to members
  post "/i/conversations/managed" (continue Create.internalCreateManagedConversationH) $
    zauthUserId
      .&. zauthConnId
      .&. jsonRequest @NewConvManaged

  -- This endpoint can lead to the following events being sent:
  -- - MemberJoin event to you, if the conversation existed and had < 2 members before
  -- - MemberJoin event to other, if the conversation existed and only the other was member
  --   before
  put "/i/conversations/:cnv/accept/v2" (continue Update.acceptConvH) $
    zauthUserId
      .&. opt zauthConnId
      .&. capture "cnv"

  put "/i/conversations/:cnv/block" (continue Update.blockConvH) $
    zauthUserId
      .&. capture "cnv"

  -- This endpoint can lead to the following events being sent:
  -- - MemberJoin event to you, if the conversation existed and had < 2 members before
  -- - MemberJoin event to other, if the conversation existed and only the other was member
  --   before
  put "/i/conversations/:cnv/unblock" (continue Update.unblockConvH) $
    zauthUserId
      .&. opt zauthConnId
      .&. capture "cnv"

  get "/i/conversations/:cnv/meta" (continue Query.getConversationMetaH) $
    capture "cnv"

  -- Team API (internal) ------------------------------------------------

  get "/i/teams/:tid" (continue Teams.getTeamInternalH) $
    capture "tid"
      .&. accept "application" "json"

  get "/i/teams/:tid/name" (continue Teams.getTeamNameInternalH) $
    capture "tid"
      .&. accept "application" "json"

  put "/i/teams/:tid" (continue Teams.createBindingTeamH) $
    zauthUserId
      .&. capture "tid"
      .&. jsonRequest @BindingNewTeam
      .&. accept "application" "json"

  delete "/i/teams/:tid" (continue Teams.internalDeleteBindingTeamWithOneMemberH) $
    capture "tid"

  put "/i/teams/:tid/status" (continue Teams.updateTeamStatusH) $
    capture "tid"
      .&. jsonRequest @TeamStatusUpdate
      .&. accept "application" "json"

  post "/i/teams/:tid/members" (continue Teams.uncheckedAddTeamMemberH) $
    capture "tid"
      .&. jsonRequest @NewTeamMember
      .&. accept "application" "json"

  get "/i/teams/:tid/members" (continue Teams.uncheckedGetTeamMembersH) $
    capture "tid"
      .&. def (unsafeRange hardTruncationLimit) (query "maxResults")
      .&. accept "application" "json"

  get "/i/teams/:tid/members/:uid" (continue Teams.uncheckedGetTeamMemberH) $
    capture "tid"
      .&. capture "uid"
      .&. accept "application" "json"

  get "/i/teams/:tid/is-team-owner/:uid" (continue Teams.userIsTeamOwnerH) $
    capture "tid"
      .&. capture "uid"
      .&. accept "application" "json"

  get "/i/teams/:tid/members/check" (continue Teams.canUserJoinTeamH) $
    capture "tid"

  -- Misc API (internal) ------------------------------------------------

  get "/i/users/:uid/team/members" (continue Teams.getBindingTeamMembersH) $
    capture "uid"

  get "/i/users/:uid/team" (continue Teams.getBindingTeamIdH) $
    capture "uid"

  get "/i/test/clients" (continue Clients.getClientsH) $
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
  post "/i/bots" (continue Update.addBotH) $
    zauthUserId
      .&. zauthConnId
      .&. jsonRequest @AddBot

  -- This endpoint can lead to the following events being sent:
  -- - MemberLeave event to members
  delete "/i/bots" (continue Update.rmBotH) $
    zauthUserId
      .&. opt zauthConnId
      .&. jsonRequest @RemoveBot

  put "/i/custom-backend/by-domain/:domain" (continue CustomBackend.internalPutCustomBackendByDomainH) $
    capture "domain"
      .&. jsonRequest @CustomBackend

  delete "/i/custom-backend/by-domain/:domain" (continue CustomBackend.internalDeleteCustomBackendByDomainH) $
    capture "domain"
      .&. accept "application" "json"

  get "/i/teams/:tid/search-visibility" (continue Teams.getSearchVisibilityInternalH) $
    capture "tid"
      .&. accept "application" "json"

  put "/i/teams/:tid/search-visibility" (continue Teams.setSearchVisibilityInternalH) $
    capture "tid"
      .&. jsonRequest @TeamSearchVisibilityView
      .&. accept "application" "json"

  put "/i/guard-legalhold-policy-conflicts" (continue guardLegalholdPolicyConflictsH) $
    jsonRequest @GuardLegalholdPolicyConflicts
      .&. accept "application" "json"

  put "/i/legalhold/whitelisted-teams/:tid" (continue setTeamLegalholdWhitelistedH) $
    capture "tid"

  delete "/i/legalhold/whitelisted-teams/:tid" (continue unsetTeamLegalholdWhitelistedH) $
    capture "tid"

  get "/i/legalhold/whitelisted-teams/:tid" (continue getTeamLegalholdWhitelistedH) $
    capture "tid"

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
        SelfConv -> return Nothing
        One2OneConv -> deleteMembers (Data.convId c) (UserList [tUnqualified lusr] []) $> Nothing
        ConnectConv -> deleteMembers (Data.convId c) (UserList [tUnqualified lusr] []) $> Nothing
        RegularConv
          | tUnqualified lusr `isMember` Data.convLocalMembers c -> do
            deleteMembers (Data.convId c) (UserList [tUnqualified lusr] [])
            let e =
                  Event
                    MemberLeave
                    (qUntagged (qualifyAs lusr (Data.convId c)))
                    (qUntagged lusr)
                    now
                    (EdMembersLeave (QualifiedUserIdList [qUser]))
            for_ (bucketRemote (fmap rmId (Data.convRemoteMembers c))) $ notifyRemoteMembers now qUser (Data.convId c)
            pure $
              Intra.newPushLocal ListComplete (tUnqualified lusr) (Intra.ConvEvent e) (Intra.recipient <$> Data.convLocalMembers c)
                <&> set Intra.pushConn conn
                  . set Intra.pushRoute Intra.RouteDirect
          | otherwise -> return Nothing

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
                cuAction = ConversationActionRemoveMembers (pure qUser)
              }
      let rpc = fedClient @'Galley @VL @"on-conversation-updated" convUpdate
      runFederatedEither remotes rpc
        >>= logAndIgnoreError "Error in onConversationUpdated call" (qUnqualified qUser)

    leaveRemoteConversations :: Range 1 UserDeletedNotificationMaxConvs [Remote ConvId] -> Sem r ()
    leaveRemoteConversations cids = do
      for_ (bucketRemote (fromRange cids)) $ \remoteConvs -> do
        let userDelete = UserDeletedConversationsNotification (tUnqualified lusr) (unsafeRange (tUnqualified remoteConvs))
        let rpc = fedClient @'Galley @VL @"on-user-deleted-conversations" userDelete
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
       Error LegalHoldError,
       Input Opts,
       TeamStore,
       P.TinyLog,
       WaiRoutes
     ]
    r =>
  (JsonRequest GuardLegalholdPolicyConflicts ::: JSON) ->
  Sem r Response
guardLegalholdPolicyConflictsH (req ::: _) = do
  glh <- fromJsonBody req
  mapError @LegalholdConflicts (const MissingLegalholdConsent) $
    guardLegalholdPolicyConflicts (glhProtectee glh) (glhUserClients glh)
  pure $ Network.Wai.Utilities.setStatus status200 empty
