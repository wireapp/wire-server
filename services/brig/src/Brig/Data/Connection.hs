{-# LANGUAGE RecordWildCards #-}

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

module Brig.Data.Connection
  ( -- * DB Operations
    insertConnection,
    updateConnection,
    updateConnectionStatus,
    lookupConnection,
    lookupRelation,
    lookupLocalConnectionsPage,
    lookupRemoteConnectionsPage,
    lookupRelationWithHistory,
    lookupLocalConnections,
    lookupConnectionStatus,
    lookupConnectionStatus',
    lookupContactList,
    lookupContactListWithRelation,
    lookupLocalConnectionStatuses,
    lookupRemoteConnectionStatuses,
    lookupAllStatuses,
    countConnections,
    deleteConnections,
    remoteConnectionInsert,
    remoteConnectionSelect,
    remoteConnectionSelectFrom,
    remoteConnectionDelete,
    remoteConnectionClear,

    -- * Re-exports
    module T,
  )
where

import Brig.App (AppIO, qualifyLocal)
import Brig.Data.Instances ()
import Brig.Data.Types as T
import Brig.Types
import Cassandra
import Control.Monad.Morph
import Control.Monad.Trans.Maybe
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as C
import Data.Domain (Domain)
import Data.Id
import Data.Json.Util (UTCTimeMillis, toUTCTimeMillis)
import Data.Qualified
import Data.Range
import Data.Time (getCurrentTime)
import Imports hiding (local)
import UnliftIO.Async (pooledMapConcurrentlyN, pooledMapConcurrentlyN_)
import Wire.API.Connection
import Wire.API.Routes.Internal.Brig.Connection

insertConnection ::
  Local UserId ->
  Qualified UserId ->
  RelationWithHistory ->
  Qualified ConvId ->
  AppIO UserConnection
insertConnection self target rel qcnv@(Qualified cnv cdomain) = do
  now <- toUTCTimeMillis <$> liftIO getCurrentTime
  let local (tUnqualified -> ltarget) =
        write connectionInsert $
          params Quorum (tUnqualified self, ltarget, rel, now, cnv)
  let remote (qUntagged -> Qualified rtarget domain) =
        write remoteConnectionInsert $
          params Quorum (tUnqualified self, domain, rtarget, rel, now, cdomain, cnv)
  retry x5 $ foldQualified self local remote target
  pure $
    UserConnection
      { ucFrom = tUnqualified self,
        ucTo = target,
        ucStatus = relationDropHistory rel,
        ucLastUpdate = now,
        ucConvId = Just qcnv
      }

updateConnection :: UserConnection -> RelationWithHistory -> AppIO UserConnection
updateConnection c status = do
  self <- qualifyLocal (ucFrom c)
  now <- updateConnectionStatus self (ucTo c) status
  pure $
    c
      { ucStatus = relationDropHistory status,
        ucLastUpdate = now
      }

updateConnectionStatus :: Local UserId -> Qualified UserId -> RelationWithHistory -> AppIO UTCTimeMillis
updateConnectionStatus self target status = do
  now <- toUTCTimeMillis <$> liftIO getCurrentTime
  let local (tUnqualified -> ltarget) =
        write connectionUpdate $
          params Quorum (status, now, tUnqualified self, ltarget)
  let remote (qUntagged -> Qualified rtarget domain) =
        write remoteConnectionUpdate $
          params Quorum (status, now, tUnqualified self, domain, rtarget)
  retry x5 $ foldQualified self local remote target
  pure now

-- | Lookup the connection from a user 'A' to a user 'B' (A -> B).
lookupConnection :: Local UserId -> Qualified UserId -> AppIO (Maybe UserConnection)
lookupConnection self target = runMaybeT $ do
  let local (tUnqualified -> ltarget) = do
        (_, _, rel, time, mcnv) <-
          MaybeT . query1 connectionSelect $
            params Quorum (tUnqualified self, ltarget)
        pure (rel, time, fmap (qUntagged . qualifyAs self) mcnv)
  let remote (qUntagged -> Qualified rtarget domain) = do
        (rel, time, cdomain, cnv) <-
          MaybeT . query1 remoteConnectionSelectFrom $
            params Quorum (tUnqualified self, domain, rtarget)
        pure (rel, time, Just (Qualified cnv cdomain))
  (rel, time, mqcnv) <- hoist (retry x1) $ foldQualified self local remote target
  pure $
    UserConnection
      { ucFrom = tUnqualified self,
        ucTo = target,
        ucStatus = relationDropHistory rel,
        ucLastUpdate = time,
        ucConvId = mqcnv
      }

-- | 'lookupConnection' with more 'Relation' info.
lookupRelationWithHistory ::
  -- | User 'A'
  Local UserId ->
  -- | User 'B'
  Qualified UserId ->
  AppIO (Maybe RelationWithHistory)
lookupRelationWithHistory self target = do
  let local (tUnqualified -> ltarget) =
        query1 relationSelect (params Quorum (tUnqualified self, ltarget))
  let remote (qUntagged -> Qualified rtarget domain) =
        query1 remoteRelationSelect (params Quorum (tUnqualified self, domain, rtarget))
  runIdentity <$$> retry x1 (foldQualified self local remote target)

lookupRelation :: Local UserId -> Qualified UserId -> AppIO Relation
lookupRelation self target =
  lookupRelationWithHistory self target <&> \case
    Nothing -> Cancelled
    Just relh -> (relationDropHistory relh)

-- | For a given user 'A', lookup their outgoing connections (A -> X) to other users.
lookupLocalConnections :: Local UserId -> Maybe UserId -> Range 1 500 Int32 -> AppIO (ResultPage UserConnection)
lookupLocalConnections lfrom start (fromRange -> size) =
  toResult <$> case start of
    Just u ->
      retry x1 $
        paginate connectionsSelectFrom (paramsP Quorum (tUnqualified lfrom, u) (size + 1))
    Nothing ->
      retry x1 $
        paginate connectionsSelect (paramsP Quorum (Identity (tUnqualified lfrom)) (size + 1))
  where
    toResult = cassandraResultPage . fmap (toLocalUserConnection lfrom) . trim
    trim p = p {result = take (fromIntegral size) (result p)}

-- | For a given user 'A', lookup their outgoing connections (A -> X) to other users.
-- Similar to lookupLocalConnections
lookupLocalConnectionsPage ::
  (MonadClient m) =>
  Local UserId ->
  Maybe PagingState ->
  Range 1 1000 Int32 ->
  m (PageWithState UserConnection)
lookupLocalConnectionsPage self pagingState (fromRange -> size) =
  fmap (toLocalUserConnection self) <$> paginateWithState connectionsSelect (paramsPagingState Quorum (Identity (tUnqualified self)) size pagingState)

-- | For a given user 'A', lookup their outgoing connections (A -> X) to remote users.
lookupRemoteConnectionsPage ::
  (MonadClient m) =>
  Local UserId ->
  Maybe PagingState ->
  Int32 ->
  m (PageWithState UserConnection)
lookupRemoteConnectionsPage self pagingState size =
  fmap (toRemoteUserConnection self)
    <$> paginateWithState
      remoteConnectionSelect
      (paramsPagingState Quorum (Identity (tUnqualified self)) size pagingState)

-- | Lookup all relations between two sets of users (cartesian product).
lookupConnectionStatus :: [UserId] -> [UserId] -> AppIO [ConnectionStatus]
lookupConnectionStatus from to =
  map toConnectionStatus
    <$> retry x1 (query connectionStatusSelect (params Quorum (from, to)))

-- | Lookup all relations between two sets of users (cartesian product).
lookupConnectionStatus' :: [UserId] -> AppIO [ConnectionStatus]
lookupConnectionStatus' from =
  map toConnectionStatus
    <$> retry x1 (query connectionStatusSelect' (params Quorum (Identity from)))

lookupLocalConnectionStatuses :: [UserId] -> Local [UserId] -> AppIO [ConnectionStatusV2]
lookupLocalConnectionStatuses froms tos = do
  concat <$> pooledMapConcurrentlyN 16 lookupStatuses froms
  where
    lookupStatuses :: UserId -> AppIO [ConnectionStatusV2]
    lookupStatuses from =
      map (uncurry $ toConnectionStatusV2 from (tDomain tos))
        <$> retry x1 (query relationsSelect (params Quorum (from, tUnqualified tos)))

lookupRemoteConnectionStatuses :: [UserId] -> Remote [UserId] -> AppIO [ConnectionStatusV2]
lookupRemoteConnectionStatuses froms tos = do
  concat <$> pooledMapConcurrentlyN 16 lookupStatuses froms
  where
    lookupStatuses :: UserId -> AppIO [ConnectionStatusV2]
    lookupStatuses from =
      map (uncurry $ toConnectionStatusV2 from (tDomain tos))
        <$> retry x1 (query remoteRelationsSelect (params Quorum (from, tDomain tos, tUnqualified tos)))

lookupAllStatuses :: Local [UserId] -> AppIO [ConnectionStatusV2]
lookupAllStatuses lfroms = do
  let froms = tUnqualified lfroms
  concat <$> pooledMapConcurrentlyN 16 lookupAndCombine froms
  where
    lookupAndCombine :: UserId -> AppIO [ConnectionStatusV2]
    lookupAndCombine u = (<>) <$> lookupLocalStatuses u <*> lookupRemoteStatuses u

    lookupLocalStatuses :: UserId -> AppIO [ConnectionStatusV2]
    lookupLocalStatuses from =
      map (uncurry $ toConnectionStatusV2 from (tDomain lfroms))
        <$> retry x1 (query relationsSelectAll (params Quorum (Identity from)))
    lookupRemoteStatuses :: UserId -> AppIO [ConnectionStatusV2]
    lookupRemoteStatuses from =
      map (\(d, u, r) -> toConnectionStatusV2 from d u r)
        <$> retry x1 (query remoteRelationsSelectAll (params Quorum (Identity from)))

-- | See 'lookupContactListWithRelation'.
lookupContactList :: UserId -> AppIO [UserId]
lookupContactList u =
  fst <$$> (filter ((== AcceptedWithHistory) . snd) <$> lookupContactListWithRelation u)

-- | For a given user 'A', lookup the list of users that form his contact list,
-- i.e. the users to whom 'A' has an outgoing 'Accepted' relation (A -> B).
lookupContactListWithRelation :: UserId -> AppIO [(UserId, RelationWithHistory)]
lookupContactListWithRelation u =
  retry x1 (query contactsSelect (params Quorum (Identity u)))

-- | Count the number of connections a user has in a specific relation status.
-- (If you want to distinguish 'RelationWithHistory', write a new function.)
-- Note: The count is eventually consistent.
countConnections :: Local UserId -> [Relation] -> AppIO Int64
countConnections u r = do
  rels <- retry x1 . query selectStatus $ params One (Identity (tUnqualified u))
  relsRemote <- retry x1 . query selectStatusRemote $ params One (Identity (tUnqualified u))

  return $ foldl' count 0 rels + foldl' count 0 relsRemote
  where
    selectStatus :: QueryString R (Identity UserId) (Identity RelationWithHistory)
    selectStatus = "SELECT status FROM connection WHERE left = ?"

    selectStatusRemote :: QueryString R (Identity UserId) (Identity RelationWithHistory)
    selectStatusRemote = "SELECT status FROM connection_remote WHERE left = ?"

    count n (Identity s) | (relationDropHistory s) `elem` r = n + 1
    count n _ = n

deleteConnections :: UserId -> AppIO ()
deleteConnections u = do
  runConduit $
    paginateC contactsSelect (paramsP Quorum (Identity u) 100) x1
      .| C.mapM_ (pooledMapConcurrentlyN_ 16 delete)
  retry x1 . write connectionClear $ params Quorum (Identity u)
  where
    delete (other, _status) = write connectionDelete $ params Quorum (other, u)

-- Queries

connectionInsert :: PrepQuery W (UserId, UserId, RelationWithHistory, UTCTimeMillis, ConvId) ()
connectionInsert = "INSERT INTO connection (left, right, status, last_update, conv) VALUES (?, ?, ?, ?, ?)"

connectionUpdate :: PrepQuery W (RelationWithHistory, UTCTimeMillis, UserId, UserId) ()
connectionUpdate = "UPDATE connection SET status = ?, last_update = ? WHERE left = ? AND right = ?"

connectionSelect :: PrepQuery R (UserId, UserId) (UserId, UserId, RelationWithHistory, UTCTimeMillis, Maybe ConvId)
connectionSelect = "SELECT left, right, status, last_update, conv FROM connection WHERE left = ? AND right = ?"

relationSelect :: PrepQuery R (UserId, UserId) (Identity RelationWithHistory)
relationSelect = "SELECT status FROM connection WHERE left = ? AND right = ?"

relationsSelect :: PrepQuery R (UserId, [UserId]) (UserId, RelationWithHistory)
relationsSelect = "SELECT right, status FROM connection where left = ? AND right IN ?"

relationsSelectAll :: PrepQuery R (Identity UserId) (UserId, RelationWithHistory)
relationsSelectAll = "SELECT right, status FROM connection where left = ?"

-- FUTUREWORK: Delete this query, we shouldn't use `IN` with the primary key of
-- the table.
connectionStatusSelect :: PrepQuery R ([UserId], [UserId]) (UserId, UserId, RelationWithHistory)
connectionStatusSelect = "SELECT left, right, status FROM connection WHERE left IN ? AND right IN ?"

-- FUTUREWORK: Delete this query, we shouldn't use `IN` with the primary key of
-- the table.
connectionStatusSelect' :: PrepQuery R (Identity [UserId]) (UserId, UserId, RelationWithHistory)
connectionStatusSelect' = "SELECT left, right, status FROM connection WHERE left IN ?"

contactsSelect :: PrepQuery R (Identity UserId) (UserId, RelationWithHistory)
contactsSelect = "SELECT right, status FROM connection WHERE left = ?"

connectionsSelect :: PrepQuery R (Identity UserId) (UserId, UserId, RelationWithHistory, UTCTimeMillis, Maybe ConvId)
connectionsSelect = "SELECT left, right, status, last_update, conv FROM connection WHERE left = ? ORDER BY right ASC"

connectionsSelectFrom :: PrepQuery R (UserId, UserId) (UserId, UserId, RelationWithHistory, UTCTimeMillis, Maybe ConvId)
connectionsSelectFrom = "SELECT left, right, status, last_update, conv FROM connection WHERE left = ? AND right > ? ORDER BY right ASC"

connectionDelete :: PrepQuery W (UserId, UserId) ()
connectionDelete = "DELETE FROM connection WHERE left = ? AND right = ?"

connectionClear :: PrepQuery W (Identity UserId) ()
connectionClear = "DELETE FROM connection WHERE left = ?"

-- Remote connections

remoteConnectionInsert :: PrepQuery W (UserId, Domain, UserId, RelationWithHistory, UTCTimeMillis, Domain, ConvId) ()
remoteConnectionInsert = "INSERT INTO connection_remote (left, right_domain, right_user, status, last_update, conv_domain, conv_id) VALUES (?, ?, ?, ?, ?, ?, ?)"

remoteConnectionSelect :: PrepQuery R (Identity UserId) (Domain, UserId, RelationWithHistory, UTCTimeMillis, Domain, ConvId)
remoteConnectionSelect = "SELECT right_domain, right_user, status, last_update, conv_domain, conv_id FROM connection_remote where left = ?"

remoteConnectionSelectFrom :: PrepQuery R (UserId, Domain, UserId) (RelationWithHistory, UTCTimeMillis, Domain, ConvId)
remoteConnectionSelectFrom = "SELECT status, last_update, conv_domain, conv_id FROM connection_remote where left = ? AND right_domain = ? AND right_user = ?"

remoteConnectionUpdate :: PrepQuery W (RelationWithHistory, UTCTimeMillis, UserId, Domain, UserId) ()
remoteConnectionUpdate = "UPDATE connection_remote set status = ?, last_update = ? WHERE left = ? and right_domain = ? and right_user = ?"

remoteConnectionDelete :: PrepQuery W (UserId, Domain, UserId) ()
remoteConnectionDelete = "DELETE FROM connection_remote where left = ? AND right_domain = ? AND right_user = ?"

remoteConnectionClear :: PrepQuery W (Identity UserId) ()
remoteConnectionClear = "DELETE FROM connection_remote where left = ?"

remoteRelationSelect :: PrepQuery R (UserId, Domain, UserId) (Identity RelationWithHistory)
remoteRelationSelect = "SELECT status FROM connection_remote WHERE left = ? AND right_domain = ? AND right_user = ?"

remoteRelationsSelect :: PrepQuery R (UserId, Domain, [UserId]) (UserId, RelationWithHistory)
remoteRelationsSelect = "SELECT right_user, status FROM connection_remote WHERE left = ? AND right_domain = ? AND right_user IN ?"

remoteRelationsSelectAll :: PrepQuery R (Identity UserId) (Domain, UserId, RelationWithHistory)
remoteRelationsSelectAll = "SELECT right_domain, right_user, status FROM connection_remote WHERE left = ?"

-- Conversions

toLocalUserConnection ::
  Local x ->
  (UserId, UserId, RelationWithHistory, UTCTimeMillis, Maybe ConvId) ->
  UserConnection
toLocalUserConnection loc (l, r, relationDropHistory -> rel, time, cid) =
  UserConnection l (qUntagged (qualifyAs loc r)) rel time (fmap (qUntagged . qualifyAs loc) cid)

toRemoteUserConnection ::
  Local UserId ->
  (Domain, UserId, RelationWithHistory, UTCTimeMillis, Domain, ConvId) ->
  UserConnection
toRemoteUserConnection l (rDomain, r, relationDropHistory -> rel, time, cDomain, cid) =
  UserConnection (tUnqualified l) (Qualified r rDomain) rel time (Just $ Qualified cid cDomain)

toConnectionStatus :: (UserId, UserId, RelationWithHistory) -> ConnectionStatus
toConnectionStatus (l, r, relationDropHistory -> rel) = ConnectionStatus l r rel

toConnectionStatusV2 :: UserId -> Domain -> UserId -> RelationWithHistory -> ConnectionStatusV2
toConnectionStatusV2 from toDomain toUser relWithHistory =
  ConnectionStatusV2 from (Qualified toUser toDomain) (relationDropHistory relWithHistory)
