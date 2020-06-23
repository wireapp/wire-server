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
  ( module T,
    connectUsers,
    insertConnection,
    updateConnection,
    lookupConnection,
    lookupConnections,
    lookupConnectionStatus,
    lookupContactList,
    countConnections,
    deleteConnections,
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
import Data.Id
import Data.Json.Util (UTCTimeMillis, toUTCTimeMillis)
import Data.Range
import Data.Time (getCurrentTime)
import Imports
import UnliftIO.Async (pooledMapConcurrentlyN_)

connectUsers :: UserId -> [(UserId, ConvId)] -> AppIO [UserConnection]
connectUsers from to = do
  now <- toUTCTimeMillis <$> liftIO getCurrentTime
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency Quorum
    forM_ to $ \(u, c) -> do
      addPrepQuery connectionInsert (from, u, Accepted, now, Nothing, c)
      addPrepQuery connectionInsert (u, from, Accepted, now, Nothing, c)
  return . concat . (`map` to) $ \(u, c) ->
    [ UserConnection from u Accepted now Nothing (Just c),
      UserConnection u from Accepted now Nothing (Just c)
    ]

insertConnection ::
  -- | From
  UserId ->
  -- | To
  UserId ->
  Relation ->
  Maybe Message ->
  ConvId ->
  AppIO UserConnection
insertConnection from to status msg cid = do
  now <- toUTCTimeMillis <$> liftIO getCurrentTime
  retry x5 . write connectionInsert $ params Quorum (from, to, status, now, msg, cid)
  return $ toUserConnection (from, to, status, now, msg, Just cid)

updateConnection :: UserConnection -> Relation -> AppIO UserConnection
updateConnection c@UserConnection {..} status = do
  now <- toUTCTimeMillis <$> liftIO getCurrentTime
  retry x5 . write connectionUpdate $ params Quorum (status, now, ucFrom, ucTo)
  return $
    c
      { ucStatus = status,
        ucLastUpdate = now
      }

-- | Lookup the connection from a user 'A' to a user 'B' (A -> B).
lookupConnection ::
  -- | User 'A'
  UserId ->
  -- | User 'B'
  UserId ->
  AppIO (Maybe UserConnection)
lookupConnection from to =
  liftM toUserConnection
    <$> retry x1 (query1 connectionSelect (params Quorum (from, to)))

-- | For a given user 'A', lookup his outgoing connections (A -> X) to other users.
lookupConnections :: UserId -> Maybe UserId -> Range 1 500 Int32 -> AppIO (ResultPage UserConnection)
lookupConnections from start (fromRange -> size) =
  toResult <$> case start of
    Just u -> retry x1 $ paginate connectionsSelectFrom (paramsP Quorum (from, u) (size + 1))
    Nothing -> retry x1 $ paginate connectionsSelect (paramsP Quorum (Identity from) (size + 1))
  where
    toResult = cassandraResultPage . fmap toUserConnection . trim
    trim p = p {result = take (fromIntegral size) (result p)}

-- | Lookup all relations between two sets of users (cartesian product).
lookupConnectionStatus :: [UserId] -> [UserId] -> AppIO [ConnectionStatus]
lookupConnectionStatus from to =
  map toConnectionStatus
    <$> retry x1 (query connectionStatusSelect (params Quorum (from, to)))

-- | For a given user 'A', lookup the list of users that form his contact list,
-- i.e. the users to whom 'A' has an outgoing 'Accepted' relation (A -> B).
lookupContactList :: UserId -> AppIO [UserId]
lookupContactList u =
  map fst . filter ((== Accepted) . snd)
    <$> retry x1 (query contactsSelect (params Quorum (Identity u)))

-- | Count the number of connections a user has in a specific relation status.
-- Note: The count is eventually consistent.
countConnections :: UserId -> [Relation] -> AppIO Int64
countConnections u r = do
  rels <- retry x1 . query selectStatus $ params One (Identity u)
  return $ foldl' count 0 rels
  where
    selectStatus :: QueryString R (Identity UserId) (Identity Relation)
    selectStatus = "SELECT status FROM connection WHERE left = ?"
    count n (Identity s) | s `elem` r = n + 1
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

connectionInsert :: PrepQuery W (UserId, UserId, Relation, UTCTimeMillis, Maybe Message, ConvId) ()
connectionInsert = "INSERT INTO connection (left, right, status, last_update, message, conv) VALUES (?, ?, ?, ?, ?, ?)"

connectionUpdate :: PrepQuery W (Relation, UTCTimeMillis, UserId, UserId) ()
connectionUpdate = "UPDATE connection SET status = ?, last_update = ? WHERE left = ? AND right = ?"

connectionSelect :: PrepQuery R (UserId, UserId) (UserId, UserId, Relation, UTCTimeMillis, Maybe Message, Maybe ConvId)
connectionSelect = "SELECT left, right, status, last_update, message, conv FROM connection WHERE left = ? AND right = ?"

connectionStatusSelect :: PrepQuery R ([UserId], [UserId]) (UserId, UserId, Relation)
connectionStatusSelect = "SELECT left, right, status FROM connection WHERE left IN ? AND right IN ?"

contactsSelect :: PrepQuery R (Identity UserId) (UserId, Relation)
contactsSelect = "SELECT right, status FROM connection WHERE left = ?"

connectionsSelect :: PrepQuery R (Identity UserId) (UserId, UserId, Relation, UTCTimeMillis, Maybe Message, Maybe ConvId)
connectionsSelect = "SELECT left, right, status, last_update, message, conv FROM connection WHERE left = ? ORDER BY right ASC"

connectionsSelectFrom :: PrepQuery R (UserId, UserId) (UserId, UserId, Relation, UTCTimeMillis, Maybe Message, Maybe ConvId)
connectionsSelectFrom = "SELECT left, right, status, last_update, message, conv FROM connection WHERE left = ? AND right > ? ORDER BY right ASC"

connectionDelete :: PrepQuery W (UserId, UserId) ()
connectionDelete = "DELETE FROM connection WHERE left = ? AND right = ?"

connectionClear :: PrepQuery W (Identity UserId) ()
connectionClear = "DELETE FROM connection WHERE left = ?"

-- Conversions

toUserConnection :: (UserId, UserId, Relation, UTCTimeMillis, Maybe Message, Maybe ConvId) -> UserConnection
toUserConnection (l, r, rel, time, msg, cid) = UserConnection l r rel time msg cid

toConnectionStatus :: (UserId, UserId, Relation) -> ConnectionStatus
toConnectionStatus (l, r, rel) = ConnectionStatus l r rel
