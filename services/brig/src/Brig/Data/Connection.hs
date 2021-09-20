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
  ( -- * DB Types
    LocalConnection (..),
    RemoteConnection (..),
    localToUserConn,

    -- * DB Operations
    insertLocalConnection,
    updateLocalConnection,
    lookupLocalConnection,
    lookupRelationWithHistory,
    lookupLocalConnections,
    lookupConnectionStatus,
    lookupConnectionStatus',
    lookupContactList,
    lookupContactListWithRelation,
    countConnections,
    deleteConnections,

    -- * Re-exports
    module T,
  )
where

import Brig.App (AppIO)
import Brig.Data.Instances ()
import Brig.Data.Types as T
import Brig.Types
import Brig.Types.Intra
import Cassandra
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as C
import Data.Domain (Domain)
import Data.Id
import Data.Json.Util (UTCTimeMillis, toUTCTimeMillis)
import Data.Qualified
import Data.Range
import Data.Time (getCurrentTime)
import Imports
import UnliftIO.Async (pooledMapConcurrentlyN_)
import Wire.API.Connection

data LocalConnection = LocalConnection
  { lcFrom :: UserId,
    lcTo :: UserId,
    lcStatus :: Relation,
    -- | Why is this a Maybe? Are there actually any users who have this as null in DB?
    lcConv :: Maybe ConvId,
    lcLastUpdated :: UTCTimeMillis
  }

localToUserConn :: Domain -> LocalConnection -> UserConnection
localToUserConn localDomain lc =
  UserConnection (lcFrom lc) (Qualified (lcTo lc) localDomain) (lcStatus lc) (lcLastUpdated lc) (lcConv lc)

data RemoteConnection = RemoteConnection
  { rcFrom :: UserId,
    rcTo :: Qualified UserId,
    rcRelationWithHistory :: Relation,
    rcConv :: Qualified ConvId
  }

insertLocalConnection ::
  -- | From
  UserId ->
  -- | To
  UserId ->
  RelationWithHistory ->
  ConvId ->
  AppIO LocalConnection
insertLocalConnection from to status cid = do
  now <- toUTCTimeMillis <$> liftIO getCurrentTime
  retry x5 . write connectionInsert $ params Quorum (from, to, status, now, cid)
  return $ toLocalUserConnection (from, to, status, now, Just cid)

-- insertRemoteConnection :: UserId -> Qualified UserId -> RelationWithHistory -> AppIO UserConnection
-- insertRemoteConnection = undefined

updateLocalConnection :: LocalConnection -> RelationWithHistory -> AppIO LocalConnection
updateLocalConnection c@LocalConnection {..} status = do
  now <- toUTCTimeMillis <$> liftIO getCurrentTime
  retry x5 . write connectionUpdate $ params Quorum (status, now, lcFrom, lcTo)
  return $
    c
      { lcStatus = relationDropHistory status,
        lcLastUpdated = now
      }

-- | Lookup the connection from a user 'A' to a user 'B' (A -> B).
lookupLocalConnection ::
  -- | User 'A'
  UserId ->
  -- | User 'B'
  UserId ->
  AppIO (Maybe LocalConnection)
lookupLocalConnection from to =
  toLocalUserConnection <$$> retry x1 (query1 connectionSelect (params Quorum (from, to)))

-- | 'lookupConnection' with more 'Relation' info.
lookupRelationWithHistory ::
  -- | User 'A'
  UserId ->
  -- | User 'B'
  UserId ->
  AppIO (Maybe RelationWithHistory)
lookupRelationWithHistory from to =
  runIdentity
    <$$> retry x1 (query1 relationSelect (params Quorum (from, to)))

-- | For a given user 'A', lookup his outgoing connections (A -> X) to other users.
lookupLocalConnections :: UserId -> Maybe UserId -> Range 1 500 Int32 -> AppIO (ResultPage LocalConnection)
lookupLocalConnections from start (fromRange -> size) =
  toResult <$> case start of
    Just u -> retry x1 $ paginate connectionsSelectFrom (paramsP Quorum (from, u) (size + 1))
    Nothing -> retry x1 $ paginate connectionsSelect (paramsP Quorum (Identity from) (size + 1))
  where
    toResult = cassandraResultPage . fmap toLocalUserConnection . trim
    trim p = p {result = take (fromIntegral size) (result p)}

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
countConnections :: UserId -> [Relation] -> AppIO Int64
countConnections u r = do
  rels <- retry x1 . query selectStatus $ params One (Identity u)
  return $ foldl' count 0 rels
  where
    selectStatus :: QueryString R (Identity UserId) (Identity RelationWithHistory)
    selectStatus = "SELECT status FROM connection WHERE left = ?"

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

connectionStatusSelect :: PrepQuery R ([UserId], [UserId]) (UserId, UserId, RelationWithHistory)
connectionStatusSelect = "SELECT left, right, status FROM connection WHERE left IN ? AND right IN ?"

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

-- Conversions

toLocalUserConnection :: (UserId, UserId, RelationWithHistory, UTCTimeMillis, Maybe ConvId) -> LocalConnection
toLocalUserConnection (l, r, relationDropHistory -> rel, time, cid) = LocalConnection l r rel cid time

toConnectionStatus :: (UserId, UserId, RelationWithHistory) -> ConnectionStatus
toConnectionStatus (l, r, relationDropHistory -> rel) = ConnectionStatus l r rel
