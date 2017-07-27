{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Brig.Data.Connection
    ( module T
    , connectUsers
    , insertConnection
    , updateConnection
    , lookupConnection
    , lookupConnections
    , lookupConnectionStatus
    , lookupContactList
    , countConnections
    , deleteConnections
    ) where

import Brig.App (AppIO)
import Brig.Data.Instances ()
import Brig.Data.Types as T
import Brig.Types
import Brig.Types.Intra
import Cassandra
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.Id
import Data.Int
import Data.List (foldl')
import Data.Range
import Data.Time (UTCTime, getCurrentTime)

connectUsers :: UserId -> [(UserId, ConvId)] -> AppIO [UserConnection]
connectUsers from to = do
    now <- liftIO getCurrentTime
    retry x5 $ batch $ do
        setType BatchLogged
        setConsistency Quorum
        forM_ to $ \(u, c) -> do
            addPrepQuery connectionInsert (from, u, Accepted, now, Nothing, c)
            addPrepQuery connectionInsert (u, from, Accepted, now, Nothing, c)
    return $ concat $ (`map` to) $ \(u, c) ->
        [ UserConnection from u Accepted now Nothing (Just c)
        , UserConnection u from Accepted now Nothing (Just c)
        ]

insertConnection :: UserId -- ^ From
                 -> UserId -- ^ To
                 -> Relation
                 -> Maybe Message
                 -> ConvId
                 -> AppIO UserConnection
insertConnection from to status msg cid = do
    now <- liftIO getCurrentTime
    retry x5 . write connectionInsert $ params Quorum (from, to, status, now, msg, cid)
    return $ toUserConnection (from, to, status, now, msg, Just cid)

updateConnection :: UserConnection -> Relation -> AppIO UserConnection
updateConnection c@UserConnection{..} status = do
    now <- liftIO getCurrentTime
    retry x5 . write connectionUpdate $ params Quorum (status, now, ucFrom, ucTo)
    return $ c { ucStatus     = status
               , ucLastUpdate = now
               }

-- | Lookup the connection from a user 'A' to a user 'B' (A -> B).
lookupConnection :: UserId -- ^ User 'A'
                 -> UserId -- ^ User 'B'
                 -> AppIO (Maybe UserConnection)
lookupConnection from to = liftM toUserConnection <$>
    retry x1 (query1 connectionSelect (params Quorum (from, to)))

-- | For a given user 'A', lookup his outgoing connections (A -> X) to other users.
lookupConnections :: UserId -> Maybe UserId -> Range 1 500 Int32 -> AppIO (ResultPage UserConnection)
lookupConnections from start (fromRange -> size) = toResult <$> case start of
    Just  u -> retry x1 $ paginate connectionsSelectFrom (paramsP Quorum (from, u) (size + 1))
    Nothing -> retry x1 $ paginate connectionsSelect (paramsP Quorum (Identity from) (size + 1))
  where
    toResult = cassandraResultPage . fmap toUserConnection . trim
    trim   p = p { result = take (fromIntegral size) (result p) }

-- | Lookup all relations between two sets of users (cartesian product).
lookupConnectionStatus :: [UserId] -> [UserId] -> AppIO [ConnectionStatus]
lookupConnectionStatus from to = map toConnectionStatus <$>
    retry x1 (query connectionStatusSelect (params Quorum (from, to)))

-- | For a given user 'A', lookup the list of users that form his contact list,
-- i.e. the users to whom 'A' has an outgoing 'Accepted' relation (A -> B).
lookupContactList :: UserId -> AppIO [UserId]
lookupContactList u = map fst . filter ((== Accepted) . snd) <$>
    retry x1 (query contactsSelect (params Quorum (Identity u)))

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
    count n _                         = n

deleteConnections :: UserId -> AppIO ()
deleteConnections u = do
    page <- retry x1 (paginate contactsSelect (paramsP Quorum (Identity u) 100))
    deleteAll page
    retry x1 . write connectionClear $ params Quorum (Identity u)
  where
    deleteAll page = do
        forM_ (result page) $ \(other, _status) ->
            retry x1 . write connectionDelete $ params Quorum (other, u)
        when (hasMore page) $
            liftClient (nextPage page) >>= deleteAll

-- Queries

connectionInsert :: PrepQuery W (UserId, UserId, Relation, UTCTime, Maybe Message, ConvId) ()
connectionInsert = "INSERT INTO connection (left, right, status, last_update, message, conv) VALUES (?, ?, ?, ?, ?, ?)"

connectionUpdate :: PrepQuery W (Relation, UTCTime, UserId, UserId) ()
connectionUpdate = "UPDATE connection SET status = ?, last_update = ? WHERE left = ? AND right = ?"

connectionSelect :: PrepQuery R (UserId, UserId) (UserId, UserId, Relation, UTCTime, Maybe Message, Maybe ConvId)
connectionSelect = "SELECT left, right, status, last_update, message, conv FROM connection WHERE left = ? AND right = ?"

connectionStatusSelect :: PrepQuery R ([UserId], [UserId]) (UserId, UserId, Relation)
connectionStatusSelect = "SELECT left, right, status FROM connection WHERE left IN ? AND right IN ?"

contactsSelect :: PrepQuery R (Identity UserId) (UserId, Relation)
contactsSelect = "SELECT right, status FROM connection WHERE left = ?"

connectionsSelect :: PrepQuery R (Identity UserId) (UserId, UserId, Relation, UTCTime, Maybe Message, Maybe ConvId)
connectionsSelect = "SELECT left, right, status, last_update, message, conv FROM connection WHERE left = ? ORDER BY right ASC"

connectionsSelectFrom :: PrepQuery R (UserId, UserId) (UserId, UserId, Relation, UTCTime, Maybe Message, Maybe ConvId)
connectionsSelectFrom = "SELECT left, right, status, last_update, message, conv FROM connection WHERE left = ? AND right > ? ORDER BY right ASC"

connectionDelete :: PrepQuery W (UserId, UserId) ()
connectionDelete = "DELETE FROM connection WHERE left = ? AND right = ?"

connectionClear :: PrepQuery W (Identity UserId) ()
connectionClear = "DELETE FROM connection WHERE left = ?"

-- Conversions

toUserConnection :: (UserId, UserId, Relation, UTCTime, Maybe Message, Maybe ConvId) -> UserConnection
toUserConnection (l, r, rel, time, msg, cid) = UserConnection l r rel time msg cid

toConnectionStatus :: (UserId, UserId, Relation) -> ConnectionStatus
toConnectionStatus (l, r, rel) = ConnectionStatus l r rel
