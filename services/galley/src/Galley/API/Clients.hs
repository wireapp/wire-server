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

module Galley.API.Clients
  ( getClients,
    rmClient,
  )
where

import Data.Id
import Data.Proxy
import Data.Qualified
import Data.Range
import Galley.API.Error
import Galley.API.MLS.Removal
import Galley.API.Query qualified as Query
import Galley.API.Util
import Galley.Effects
import Galley.Effects.BackendNotificationQueueAccess
import Galley.Effects.ClientStore qualified as E
import Galley.Env
import Galley.Types.Clients (clientIds)
import Imports
import Network.AMQP qualified as Q
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog qualified as P
import System.Logger.Message
import Wire.API.Conversation hiding (Member)
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.Routes.MultiTablePaging
import Wire.ConversationStore (getConversation)
import Wire.NotificationSubsystem
import Wire.Sem.Now (Now)
import Wire.Sem.Paging.Cassandra (CassandraPaging)
import Wire.TeamCollaboratorsSubsystem

getClients ::
  ( Member BrigAPIAccess r,
    Member ClientStore r
  ) =>
  UserId ->
  Sem r [ClientId]
getClients usr = clientIds usr <$> getBrigClients [usr]

-- | Remove a client from conversations it is part of according to the
-- conversation protocol (Proteus or MLS). In addition, remove the client from
-- the "clients" table in Galley.
rmClient ::
  forall p1 r.
  ( p1 ~ CassandraPaging,
    Member ClientStore r,
    Member ConversationStore r,
    Member (Error FederationError) r,
    Member ExternalAccess r,
    Member BackendNotificationQueueAccess r,
    Member NotificationSubsystem r,
    Member (Input Env) r,
    Member (Input (Local ())) r,
    Member Now r,
    Member (ListItems p1 ConvId) r,
    Member (ListItems p1 (Remote ConvId)) r,
    Member (Error InternalError) r,
    Member ProposalStore r,
    Member Random r,
    Member P.TinyLog r,
    Member TeamCollaboratorsSubsystem r
  ) =>
  UserId ->
  ClientId ->
  Sem r ()
rmClient usr cid = do
  clients <- E.getClients [usr]
  if (cid `elem` clientIds usr clients)
    then do
      lusr <- qualifyLocal usr
      let nRange1000 = toRange (Proxy @1000) :: Range 1 1000 Int32
      firstConvIds <- Query.conversationIdsPageFrom lusr (GetPaginatedConversationIds Nothing nRange1000)
      goConvs nRange1000 firstConvIds lusr
      E.deleteClient usr cid
    else
      P.debug
        ( field "user" (idToText usr)
            . field "client" (clientToText cid)
            . msg (val "rmClientH: client already gone")
        )
  where
    goConvs :: Range 1 1000 Int32 -> ConvIdsPage -> Local UserId -> Sem r ()
    goConvs range page lusr = do
      let (localConvs, remoteConvs) = partitionQualified lusr (mtpResults page)
      for_ localConvs $ \convId -> do
        mConv <- getConversation convId
        for_ mConv $ \conv -> do
          lconv <- qualifyLocal conv
          removeClient lconv (tUntagged lusr) cid
      traverse_ removeRemoteMLSClients (rangedChunks remoteConvs)
      when (mtpHasMore page) $ do
        let nextState = mtpPagingState page
            nextQuery = GetPaginatedConversationIds (Just nextState) range
        newCids <- Query.conversationIdsPageFrom lusr nextQuery
        goConvs range newCids lusr

    removeRemoteMLSClients :: Range 1 1000 [Remote ConvId] -> Sem r ()
    removeRemoteMLSClients convIds = do
      for_ (bucketRemote (fromRange convIds)) $ \remoteConvs ->
        let rpc =
              fedQueueClient
                @'OnClientRemovedTag
                (ClientRemovedRequest usr cid (tUnqualified remoteConvs))
         in enqueueNotification Q.Persistent remoteConvs rpc
