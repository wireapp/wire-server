{-# LANGUAGE TemplateHaskell #-}

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

module Wire.UserClientIndexStore
  ( -- * UserClientIndexStore Effect
    UserClientIndexStore (..),

    -- * Create client
    createClient,

    -- * Get client
    getClients,

    -- * Delete client
    deleteClient,
    deleteClients,

    -- * Helpers
    internalGetClientIds,
    rmClient,
    getClientsId,
  )
where

import Data.Domain (Domain)
import Data.Id
import Data.Proxy (Proxy (..))
import Data.Qualified
import Data.Range
import Galley.Types.Clients
import Imports
import Network.AMQP qualified as Q
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog qualified as P
import System.Logger.Message
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Config (ConversationSubsystemConfig (..))
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.Routes.MultiTablePaging
import Wire.BackendNotificationQueueAccess
import Wire.BrigAPIAccess
import Wire.ConversationSubsystem qualified as ConversationSubsystem

data UserClientIndexStore m a where
  GetClients :: [UserId] -> UserClientIndexStore m Clients
  CreateClient :: UserId -> ClientId -> UserClientIndexStore m ()
  DeleteClient :: UserId -> ClientId -> UserClientIndexStore m ()
  DeleteClients :: UserId -> UserClientIndexStore m ()

makeSem ''UserClientIndexStore

internalGetClientIds ::
  ( Member BrigAPIAccess r,
    Member UserClientIndexStore r,
    Member (Input ConversationSubsystemConfig) r
  ) =>
  [UserId] ->
  Sem r Clients
internalGetClientIds users = do
  cfg <- input
  let isInternal = cfg.listClientsUsingBrig
  if isInternal
    then fromUserClients <$> lookupClients users
    else getClients users

rmClient ::
  forall r.
  ( Member UserClientIndexStore r,
    Member ConversationSubsystem.ConversationSubsystem r,
    Member (Error FederationError) r,
    Member BackendNotificationQueueAccess r,
    Member (Input (Local ())) r,
    Member P.TinyLog r
  ) =>
  UserId ->
  ClientId ->
  Sem r ()
rmClient usr cid = do
  clients <- getClients [usr]
  if (cid `elem` clientIds usr clients)
    then do
      lusr <- qualifyLocal usr
      let nRange1000 = toRange (Proxy @1000) :: Range 1 1000 Int32
      firstConvIds <- ConversationSubsystem.conversationIdsPageFrom lusr (GetPaginatedConversationIds Nothing nRange1000)
      goConvs nRange1000 firstConvIds lusr
      deleteClient usr cid
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
        mConv <- ConversationSubsystem.internalGetConversation convId
        for_ mConv $ \conv -> do
          lconv <- qualifyLocal conv
          ConversationSubsystem.removeClient lconv (tUntagged lusr) cid
      traverse_ removeRemoteMLSClients (rangedChunks remoteConvs)
      when (mtpHasMore page) $ do
        let nextState = mtpPagingState page
            nextQuery = GetPaginatedConversationIds (Just nextState) range
        newCids <- ConversationSubsystem.conversationIdsPageFrom lusr nextQuery
        goConvs range newCids lusr

    removeRemoteMLSClients :: Range 1 1000 [Remote ConvId] -> Sem r ()
    removeRemoteMLSClients convIds = do
      for_ (bucketRemote (fromRange convIds)) $ \remoteConvs ->
        let rpc =
              fedQueueClient
                @'OnClientRemovedTag
                (ClientRemovedRequest usr cid (tUnqualified remoteConvs))
         in enqueueNotification Q.Persistent remoteConvs rpc

getClientsId ::
  ( Member BrigAPIAccess r,
    Member UserClientIndexStore r,
    Member (Input ConversationSubsystemConfig) r
  ) =>
  UserId ->
  Sem r [ClientId]
getClientsId usr = clientIds usr <$> internalGetClientIds [usr]

qualifyLocal :: (Member (Input (Local ())) r) => a -> Sem r (Local a)
qualifyLocal a = toLocalUnsafe <$> fmap getDomain input <*> pure a
  where
    getDomain :: Local () -> Domain
    getDomain = tDomain
