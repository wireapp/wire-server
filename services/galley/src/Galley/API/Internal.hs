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

import qualified Cassandra as Cql
import Control.Exception.Safe (catchAny)
import Control.Lens hiding ((.=))
import Control.Monad.Catch (MonadCatch)
import Data.Id as Id
import Data.List1 (List1, list1, maybeList1)
import Data.Range
import Data.String.Conversions (cs)
import GHC.TypeLits (AppendSymbol)
import qualified Galley.API.Clients as Clients
import qualified Galley.API.Create as Create
import qualified Galley.API.CustomBackend as CustomBackend
import Galley.API.Error (throwErrorDescription)
import Galley.API.LegalHold (getTeamLegalholdWhitelistedH, setTeamLegalholdWhitelistedH, unsetTeamLegalholdWhitelistedH)
import Galley.API.LegalHold.Conflicts (guardLegalholdPolicyConflicts)
import qualified Galley.API.Query as Query
import Galley.API.Teams (uncheckedDeleteTeamMember)
import qualified Galley.API.Teams as Teams
import Galley.API.Teams.Features (DoAuth (..))
import qualified Galley.API.Teams.Features as Features
import qualified Galley.API.Update as Update
import Galley.API.Util (JSON, isMember, viewFederationDomain)
import Galley.App
import qualified Galley.Data as Data
import qualified Galley.Intra.Push as Intra
import qualified Galley.Queue as Q
import Galley.Types
import Galley.Types.Bot (AddBot, RemoveBot)
import Galley.Types.Bot.Service
import Galley.Types.Teams
import Galley.Types.Teams.Intra
import Galley.Types.Teams.SearchVisibility
import Imports hiding (head)
import Network.HTTP.Types (status200)
import Network.Wai
import Network.Wai.Predicate hiding (err)
import qualified Network.Wai.Predicate as P
import Network.Wai.Routing hiding (route, toList)
import Network.Wai.Utilities
import Network.Wai.Utilities.ZAuth
import Servant.API hiding (JSON)
import qualified Servant.API as Servant
import Servant.API.Generic
import Servant.Server
import Servant.Server.Generic (genericServerT)
import System.Logger.Class hiding (Path, name)
import Wire.API.ErrorDescription (missingLegalholdConsent)
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
        :- IFeatureStatusGet 'Public.TeamFeatureSSO,
    iTeamFeatureStatusSSOPut ::
      routes
        :- IFeatureStatusPut 'Public.TeamFeatureSSO,
    iTeamFeatureStatusLegalHoldGet ::
      routes
        :- IFeatureStatusGet 'Public.TeamFeatureLegalHold,
    iTeamFeatureStatusLegalHoldPut ::
      routes
        :- IFeatureStatusPut 'Public.TeamFeatureLegalHold,
    iTeamFeatureStatusSearchVisibilityGet ::
      routes
        :- IFeatureStatusGet 'Public.TeamFeatureSearchVisibility,
    iTeamFeatureStatusSearchVisibilityPut ::
      routes
        :- IFeatureStatusPut 'Public.TeamFeatureSearchVisibility,
    iTeamFeatureStatusSearchVisibilityDeprecatedGet ::
      routes
        :- IFeatureStatusDeprecatedGet 'Public.TeamFeatureSearchVisibility,
    iTeamFeatureStatusSearchVisibilityDeprecatedPut ::
      routes
        :- IFeatureStatusDeprecatedPut 'Public.TeamFeatureSearchVisibility,
    iTeamFeatureStatusValidateSAMLEmailsGet ::
      routes
        :- IFeatureStatusGet 'Public.TeamFeatureValidateSAMLEmails,
    iTeamFeatureStatusValidateSAMLEmailsPut ::
      routes
        :- IFeatureStatusPut 'Public.TeamFeatureValidateSAMLEmails,
    iTeamFeatureStatusValidateSAMLEmailsDeprecatedGet ::
      routes
        :- IFeatureStatusDeprecatedGet 'Public.TeamFeatureValidateSAMLEmails,
    iTeamFeatureStatusValidateSAMLEmailsDeprecatedPut ::
      routes
        :- IFeatureStatusDeprecatedPut 'Public.TeamFeatureValidateSAMLEmails,
    iTeamFeatureStatusDigitalSignaturesGet ::
      routes
        :- IFeatureStatusGet 'Public.TeamFeatureDigitalSignatures,
    iTeamFeatureStatusDigitalSignaturesPut ::
      routes
        :- IFeatureStatusPut 'Public.TeamFeatureDigitalSignatures,
    iTeamFeatureStatusDigitalSignaturesDeprecatedGet ::
      routes
        :- IFeatureStatusDeprecatedGet 'Public.TeamFeatureDigitalSignatures,
    iTeamFeatureStatusDigitalSignaturesDeprecatedPut ::
      routes
        :- IFeatureStatusDeprecatedPut 'Public.TeamFeatureDigitalSignatures,
    iTeamFeatureStatusAppLockGet ::
      routes
        :- IFeatureStatusGet 'Public.TeamFeatureAppLock,
    iTeamFeatureStatusAppLockPut ::
      routes
        :- IFeatureStatusPut 'Public.TeamFeatureAppLock,
    iTeamFeatureStatusFileSharingGet ::
      routes
        :- IFeatureStatusGet 'Public.TeamFeatureFileSharing,
    iTeamFeatureStatusFileSharingPut ::
      routes
        :- IFeatureStatusPut 'Public.TeamFeatureFileSharing,
    iTeamFeatureStatusClassifiedDomainsGet ::
      routes
        :- IFeatureStatusGet 'Public.TeamFeatureClassifiedDomains,
    iTeamFeatureStatusConferenceCallingPut ::
      routes
        :- IFeatureStatusPut 'Public.TeamFeatureConferenceCalling,
    iTeamFeatureStatusConferenceCallingGet ::
      routes
        :- IFeatureStatusGet 'Public.TeamFeatureConferenceCalling
  }
  deriving (Generic)

type ServantAPI = ToServantApi InternalApi

type IFeatureStatusGet featureName =
  Summary (AppendSymbol "Get config for " (Public.KnownTeamFeatureNameSymbol featureName))
    :> "i"
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> Public.KnownTeamFeatureNameSymbol featureName
    :> Get '[Servant.JSON] (Public.TeamFeatureStatus featureName)

type IFeatureStatusPut featureName =
  Summary (AppendSymbol "Put config for " (Public.KnownTeamFeatureNameSymbol featureName))
    :> "i"
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> Public.KnownTeamFeatureNameSymbol featureName
    :> ReqBody '[Servant.JSON] (Public.TeamFeatureStatus featureName)
    :> Put '[Servant.JSON] (Public.TeamFeatureStatus featureName)

-- | A type for a GET endpoint for a feature with a deprecated path
type IFeatureStatusDeprecatedGet featureName =
  Summary (AppendSymbol "[deprecated] Get config for " (Public.KnownTeamFeatureNameSymbol featureName))
    :> "i"
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> Public.DeprecatedFeatureName featureName
    :> Get '[Servant.JSON] (Public.TeamFeatureStatus featureName)

-- | A type for a PUT endpoint for a feature with a deprecated path
type IFeatureStatusDeprecatedPut featureName =
  Summary (AppendSymbol "[deprecated] Put config for " (Public.KnownTeamFeatureNameSymbol featureName))
    :> "i"
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> Public.DeprecatedFeatureName featureName
    :> ReqBody '[Servant.JSON] (Public.TeamFeatureStatus featureName)
    :> Put '[Servant.JSON] (Public.TeamFeatureStatus featureName)

servantSitemap :: ServerT ServantAPI Galley
servantSitemap =
  genericServerT $
    InternalApi
      { iStatusGet = pure NoContent,
        iStatusHead = pure NoContent,
        iTeamFeatureStatusSSOGet = iGetTeamFeature @'Public.TeamFeatureSSO Features.getSSOStatusInternal,
        iTeamFeatureStatusSSOPut = iPutTeamFeature @'Public.TeamFeatureSSO Features.setSSOStatusInternal,
        iTeamFeatureStatusLegalHoldGet = iGetTeamFeature @'Public.TeamFeatureLegalHold Features.getLegalholdStatusInternal,
        iTeamFeatureStatusLegalHoldPut = iPutTeamFeature @'Public.TeamFeatureLegalHold Features.setLegalholdStatusInternal,
        iTeamFeatureStatusSearchVisibilityGet = iGetTeamFeature @'Public.TeamFeatureSearchVisibility Features.getTeamSearchVisibilityAvailableInternal,
        iTeamFeatureStatusSearchVisibilityPut = iPutTeamFeature @'Public.TeamFeatureLegalHold Features.setTeamSearchVisibilityAvailableInternal,
        iTeamFeatureStatusSearchVisibilityDeprecatedGet = iGetTeamFeature @'Public.TeamFeatureSearchVisibility Features.getTeamSearchVisibilityAvailableInternal,
        iTeamFeatureStatusSearchVisibilityDeprecatedPut = iPutTeamFeature @'Public.TeamFeatureLegalHold Features.setTeamSearchVisibilityAvailableInternal,
        iTeamFeatureStatusValidateSAMLEmailsGet = iGetTeamFeature @'Public.TeamFeatureValidateSAMLEmails Features.getValidateSAMLEmailsInternal,
        iTeamFeatureStatusValidateSAMLEmailsPut = iPutTeamFeature @'Public.TeamFeatureValidateSAMLEmails Features.setValidateSAMLEmailsInternal,
        iTeamFeatureStatusValidateSAMLEmailsDeprecatedGet = iGetTeamFeature @'Public.TeamFeatureValidateSAMLEmails Features.getValidateSAMLEmailsInternal,
        iTeamFeatureStatusValidateSAMLEmailsDeprecatedPut = iPutTeamFeature @'Public.TeamFeatureValidateSAMLEmails Features.setValidateSAMLEmailsInternal,
        iTeamFeatureStatusDigitalSignaturesGet = iGetTeamFeature @'Public.TeamFeatureDigitalSignatures Features.getDigitalSignaturesInternal,
        iTeamFeatureStatusDigitalSignaturesPut = iPutTeamFeature @'Public.TeamFeatureDigitalSignatures Features.setDigitalSignaturesInternal,
        iTeamFeatureStatusDigitalSignaturesDeprecatedGet = iGetTeamFeature @'Public.TeamFeatureDigitalSignatures Features.getDigitalSignaturesInternal,
        iTeamFeatureStatusDigitalSignaturesDeprecatedPut = iPutTeamFeature @'Public.TeamFeatureDigitalSignatures Features.setDigitalSignaturesInternal,
        iTeamFeatureStatusAppLockGet = iGetTeamFeature @'Public.TeamFeatureAppLock Features.getAppLockInternal,
        iTeamFeatureStatusAppLockPut = iPutTeamFeature @'Public.TeamFeatureAppLock Features.setAppLockInternal,
        iTeamFeatureStatusFileSharingGet = iGetTeamFeature @'Public.TeamFeatureFileSharing Features.getFileSharingInternal,
        iTeamFeatureStatusFileSharingPut = iPutTeamFeature @'Public.TeamFeatureFileSharing Features.setFileSharingInternal,
        iTeamFeatureStatusClassifiedDomainsGet = iGetTeamFeature @'Public.TeamFeatureClassifiedDomains Features.getClassifiedDomainsInternal,
        iTeamFeatureStatusConferenceCallingPut = iPutTeamFeature @'Public.TeamFeatureConferenceCalling Features.setConferenceCallingInternal,
        iTeamFeatureStatusConferenceCallingGet = iGetTeamFeature @'Public.TeamFeatureConferenceCalling Features.getConferenceCallingInternal
      }

iGetTeamFeature ::
  forall a.
  Public.KnownTeamFeatureName a =>
  (Maybe TeamId -> Galley (Public.TeamFeatureStatus a)) ->
  TeamId ->
  Galley (Public.TeamFeatureStatus a)
iGetTeamFeature getter = Features.getFeatureStatus @a getter DontDoAuth

iPutTeamFeature ::
  forall a.
  Public.KnownTeamFeatureName a =>
  (TeamId -> Public.TeamFeatureStatus a -> Galley (Public.TeamFeatureStatus a)) ->
  TeamId ->
  Public.TeamFeatureStatus a ->
  Galley (Public.TeamFeatureStatus a)
iPutTeamFeature setter = Features.setFeatureStatus @a setter DontDoAuth

sitemap :: Routes a Galley ()
sitemap = do
  -- Conversation API (internal) ----------------------------------------

  put "/i/conversations/:cnv/channel" (continue $ const (return empty)) $
    zauthUserId
      .&. (capture "cnv" :: HasCaptures r => Predicate r P.Error ConvId)
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
  -- - ConvCreate event to self, if conversation did not exist before
  -- - ConvConnect event to self, if other didn't join the connect conversation before
  post "/i/conversations/connect" (continue Create.createConnectConversationH) $
    zauthUserId
      .&. opt zauthConnId
      .&. jsonRequest @Connect

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

  -- This endpoint can lead to the following events being sent:
  -- - MemberLeave event to members for all conversations the user was in
  delete "/i/user" (continue rmUserH) $
    zauthUserId .&. opt zauthConnId

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

rmUserH :: UserId ::: Maybe ConnId -> Galley Response
rmUserH (user ::: conn) = do
  empty <$ rmUser user conn

rmUser :: UserId -> Maybe ConnId -> Galley ()
rmUser user conn = do
  let n = unsafeRange 100 :: Range 1 100 Int32
  tids <- Data.teamIdsForPagination user Nothing (rcast n)
  leaveTeams tids
  cids <- Data.conversationIdRowsForPagination user Nothing (rcast n)
  let u = list1 user []
  leaveConversations u cids
  Data.eraseClients user
  where
    leaveTeams tids = for_ (Cql.result tids) $ \tid -> do
      mems <- Data.teamMembersForFanout tid
      uncheckedDeleteTeamMember user conn tid user mems
      leaveTeams =<< Cql.liftClient (Cql.nextPage tids)
    leaveConversations :: List1 UserId -> Cql.Page ConvId -> Galley ()
    leaveConversations u ids = do
      localDomain <- viewFederationDomain
      cc <- Data.conversations (Cql.result ids)
      pp <- for cc $ \c -> case Data.convType c of
        SelfConv -> return Nothing
        One2OneConv -> Data.removeMember user (Data.convId c) >> return Nothing
        ConnectConv -> Data.removeMember user (Data.convId c) >> return Nothing
        RegularConv
          | user `isMember` Data.convLocalMembers c -> do
            -- FUTUREWORK: deal with remote members, too, see removeMembers
            e <- Data.removeLocalMembers localDomain c user u
            return $
              (Intra.newPushLocal ListComplete user (Intra.ConvEvent e) (Intra.recipient <$> Data.convLocalMembers c))
                <&> set Intra.pushConn conn
                  . set Intra.pushRoute Intra.RouteDirect
          | otherwise -> return Nothing
      for_
        (maybeList1 (catMaybes pp))
        Intra.push
      unless (null $ Cql.result ids) $
        leaveConversations u =<< Cql.liftClient (Cql.nextPage ids)

deleteLoop :: Galley ()
deleteLoop = do
  q <- view deleteQueue
  safeForever "deleteLoop" $ do
    i@(TeamItem tid usr con) <- Q.pop q
    Teams.uncheckedDeleteTeam usr con tid `catchAny` someError q i
  where
    someError q i x = do
      err $ "error" .= show x ~~ msg (val "failed to delete")
      ok <- Q.tryPush q i
      unless ok $
        err (msg (val "delete queue is full, dropping item") ~~ "item" .= show i)
      liftIO $ threadDelay 1000000

safeForever :: (MonadIO m, MonadLogger m, MonadCatch m) => String -> m () -> m ()
safeForever funName action =
  forever $
    action `catchAny` \exc -> do
      err $ "error" .= show exc ~~ msg (val $ cs funName <> " failed")
      threadDelay 60000000 -- pause to keep worst-case noise in logs manageable

guardLegalholdPolicyConflictsH :: (JsonRequest GuardLegalholdPolicyConflicts ::: JSON) -> Galley Response
guardLegalholdPolicyConflictsH (req ::: _) = do
  glh <- fromJsonBody req
  guardLegalholdPolicyConflicts (glhProtectee glh) (glhUserClients glh)
    >>= either (const (throwErrorDescription missingLegalholdConsent)) pure
  pure $ Network.Wai.Utilities.setStatus status200 empty
