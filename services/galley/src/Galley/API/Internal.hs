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
    deleteLoop,
    safeForever,
  )
where

import qualified Cassandra as Cql
import Control.Exception.Safe (catchAny)
import Control.Lens hiding ((.=))
import Control.Monad.Catch (MonadCatch, throwM)
import Data.Id as Id
import Data.IdMapping (MappedOrLocalId (Local), partitionMappedOrLocalIds)
import Data.List.NonEmpty (nonEmpty)
import Data.List1 (List1, list1, maybeList1)
import Data.Range
import Data.String.Conversions (cs)
import qualified Galley.API.Clients as Clients
import qualified Galley.API.Create as Create
import qualified Galley.API.CustomBackend as CustomBackend
import qualified Galley.API.Error as Error
import qualified Galley.API.IdMapping as IdMapping
import qualified Galley.API.Query as Query
import Galley.API.Teams (uncheckedDeleteTeamMember)
import qualified Galley.API.Teams as Teams
import qualified Galley.API.Update as Update
import Galley.API.Util (isMember)
import Galley.App
import qualified Galley.Data as Data
import qualified Galley.Intra.Push as Intra
import qualified Galley.Queue as Q
import Galley.Types
import Galley.Types.Bot (AddBot, RemoveBot)
import Galley.Types.Bot.Service
import Galley.Types.IdMapping (PostIdMappingRequest)
import Galley.Types.Teams
import Galley.Types.Teams.Intra
import Galley.Types.Teams.SearchVisibility
import Imports hiding (head)
import Network.Wai
import Network.Wai.Predicate hiding (err)
import qualified Network.Wai.Predicate as P
import Network.Wai.Routing hiding (route)
import Network.Wai.Utilities
import Network.Wai.Utilities.ZAuth
import System.Logger.Class hiding (Path)
import qualified Wire.API.Team.Feature as Public

sitemap :: Routes a Galley ()
sitemap = do
  head "/i/status" (continue $ const (return empty)) true
  get "/i/status" (continue $ const (return empty)) true

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

  -- Team Feature Flag API (internal) -----------------------------------
  --
  -- Enabling this should only be possible internally.
  -- Viewing the status should be allowed for any admin.

  get "/i/teams/:tid/features/:feature" (continue Teams.getFeatureStatusInternalH) $
    capture "tid"
      .&. capture "feature"
      .&. accept "application" "json"

  put "/i/teams/:tid/features/:feature" (continue Teams.setFeatureStatusInternalH) $
    capture "tid"
      .&. capture "feature"
      .&. jsonRequest @Public.TeamFeatureStatus
      .&. accept "application" "json"

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

  -- Id Mapping ---------------------------------------------------------

  get "/i/id-mapping/:uid" (continue IdMapping.getIdMappingH) $
    capture "uid"
      .&. accept "application" "json"

  post "/i/id-mapping" (continue IdMapping.postIdMappingH) $
    jsonRequest @PostIdMappingRequest
      .&. accept "application" "json"

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
    leaveConversations :: List1 UserId -> Cql.Page (Data.MappedOrLocalIdRow Id.C) -> Galley ()
    leaveConversations u ids = do
      (localConvIds, remoteConvIds) <-
        partitionMappedOrLocalIds <$> traverse Data.toMappedOrLocalId (Cql.result ids)
      -- FUTUREWORK(federation, #1275): leave remote conversations.
      -- If we could just get all conversation IDs at once and then leave conversations
      -- in batches, it would make everything much easier.
      for_ (nonEmpty remoteConvIds) $
        throwM . Error.federationNotImplemented
      cc <- Data.conversations localConvIds
      pp <- for cc $ \c -> case Data.convType c of
        SelfConv -> return Nothing
        One2OneConv -> Data.removeMember (Local user) (Data.convId c) >> return Nothing
        ConnectConv -> Data.removeMember (Local user) (Data.convId c) >> return Nothing
        RegularConv
          | Local user `isMember` Data.convMembers c -> do
            e <- Data.removeMembers c user (Local <$> u)
            return $
              (Intra.newPush ListComplete (evtFrom e) (Intra.ConvEvent e) (Intra.recipient <$> Data.convMembers c))
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
