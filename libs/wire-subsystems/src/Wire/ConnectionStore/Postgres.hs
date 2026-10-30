-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.ConnectionStore.Postgres
  ( interpretConnectionStoreToPostgres,
    insertRemoteConnectionRaw,
  )
where

import Cassandra (Page (..), PageWithState (..), PagingState (..), emptyPage)
import Cassandra qualified as Cas
import Cassandra.Exec (GeneralPaginationState (..))
import Control.Exception (ErrorCall (..), throwIO)
import Data.ByteString qualified as BS
import Data.ByteString.Conversion
import Data.ByteString.Lazy qualified as LBS
import Data.Domain (Domain)
import Data.Id
import Data.Json.Util
import Data.Qualified
import Data.Range
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Vector qualified as Vector
import Hasql.Pool.Extended qualified as HasqlPoolExt
import Hasql.Session (statement)
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Wire.API.Connection
import Wire.API.PostgresMarshall (PostgresMarshall (..), PostgresUnmarshall (..), dimapPG, lmapPG)
import Wire.ConnectionStore
import Wire.Postgres
import Wire.Sem.Paging.Cassandra (InternalPage (..), InternalPaging)

-- | Postgres interpreter for 'ConnectionStore'. The @status@ column uses the
-- same Int32 mapping as the Cassandra 'C.Cql RelationWithHistory' instance
-- (mirrored by the 'PostgresMarshall Int32 RelationWithHistory' instances).
interpretConnectionStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor (ConnectionStore InternalPaging) r
interpretConnectionStoreToPostgres = interpret $ \case
  InsertConnection self target rel qcnv@(Qualified cnv cdomain) -> do
    now <- embed @IO (toUTCTimeMillis <$> getCurrentTime)
    let relInt = postgresMarshall rel
        ts = fromUTCTimeMillis now
    foldQualified
      self
      ( \(tUnqualified -> ltarget) ->
          runStatement (tUnqualified self, ltarget, relInt, ts, cnv) insertLocal
      )
      ( \(tUntagged -> Qualified rtarget domain) ->
          runStatement (tUnqualified self, domain, rtarget, relInt, ts, cdomain, cnv) insertRemote
      )
      target
    pure $
      UserConnection
        { ucFrom = tUnqualified self,
          ucTo = target,
          ucStatus = relationDropHistory rel,
          ucLastUpdate = now,
          ucConvId = Just qcnv
        }
  UpdateConnectionStatus self target status -> do
    now <- embed @IO (toUTCTimeMillis <$> getCurrentTime)
    let relInt = postgresMarshall status
        ts = fromUTCTimeMillis now
    foldQualified
      self
      ( \(tUnqualified -> ltarget) ->
          runStatement (relInt, ts, tUnqualified self, ltarget) updateLocal
      )
      ( \(tUntagged -> Qualified rtarget domain) ->
          runStatement (relInt, ts, tUnqualified self, domain, rtarget) updateRemote
      )
      target
    pure now
  DeleteConnections u -> do
    runStatement u deleteIncomingLocalConnections
    runStatement u deleteLocalConnections
    runStatement u deleteRemoteConnectionsByLeft
  DeleteRemoteConnections (tUntagged -> Qualified remoteUser remoteDomain) rs ->
    runStatement (remoteUser, remoteDomain, fromRange rs) deleteRemoteConnectionsByUsers
  LookupConnection self target -> do
    foldQualified
      self
      ( \(tUnqualified -> ltarget) -> do
          mrow <- runStatement (tUnqualified self, ltarget) selectLocalConnection
          pure (fmap (localRowToUserConnection self target) mrow)
      )
      ( \(tUntagged -> Qualified rtarget domain) -> do
          mrow <- runStatement (tUnqualified self, domain, rtarget) selectRemoteConnection
          pure (fmap (remoteRowToUserConnection self target) mrow)
      )
      target
  LookupRelationWithHistory self target ->
    foldQualified
      self
      ( \(tUnqualified -> ltarget) ->
          fmap unStatus <$> runStatement (tUnqualified self, ltarget) selectLocalRelation
      )
      ( \(tUntagged -> Qualified rtarget domain) ->
          fmap unStatus <$> runStatement (tUnqualified self, domain, rtarget) selectRemoteRelation
      )
      target
  LookupLocalConnections lfrom start (fromRange -> size) -> do
    rows <- case start of
      Just u -> runStatement (tUnqualified lfrom, u, size + 1) selectLocalConnectionsFrom
      Nothing -> runStatement (tUnqualified lfrom, size + 1) selectLocalConnections
    pure $ trimLocalPage lfrom size rows
  LookupLocalConnectionsPage self pagingState (fromRange -> size) -> do
    let mStart = pagingState >>= decodeLocalMarker . LBS.toStrict . unPagingState
    rows <- case mStart of
      Just u -> runStatement (tUnqualified self, u, size) selectLocalConnectionsFrom
      Nothing -> runStatement (tUnqualified self, size) selectLocalConnections
    pure $ buildPageWithState (localRowToUserConnectionByRight self) (\(rid, _, _, _) -> UUID.toASCIIBytes rid) size rows
  LookupRemoteConnectionsPage self pagingState size -> do
    let mStart = pagingState >>= decodeRemoteMarker . LBS.toStrict . unPagingState
    rows <- case mStart of
      Just (domain, ruser) ->
        runStatement (tUnqualified self, domain, ruser, size) selectRemoteConnectionsFrom
      Nothing -> runStatement (tUnqualified self, size) selectRemoteConnections
    pure $ buildPageWithState (remoteRowToUserConnectionByRow self) (\(d, rid, _, _, _, _) -> encodeRemoteMarker d rid) size rows
  LookupConnectionStatus from to -> do
    rows <- runStatement (from, to) selectConnectionStatuses
    pure $ map (\(l, r, s) -> toConnectionStatus (Id l, Id r, unStatus s)) (Vector.toList rows)
  LookupConnectionStatus' from -> do
    rows <- runStatement from selectConnectionStatuses'
    pure $ map (\(l, r, s) -> toConnectionStatus (Id l, Id r, unStatus s)) (Vector.toList rows)
  LookupLocalConnectionStatuses froms tos ->
    concat
      <$> for
        froms
        ( \from -> do
            rows <- runStatement (from, tUnqualified tos) selectLocalRelations
            pure $ map (\(rid, s) -> toConnectionStatusV2 from (tDomain tos) (Id rid) (unStatus s)) (Vector.toList rows)
        )
  LookupRemoteConnectionStatuses froms tos ->
    concat
      <$> for
        froms
        ( \from -> do
            rows <- runStatement (from, tDomain tos, tUnqualified tos) selectRemoteRelations
            pure $ map (\(rid, s) -> toConnectionStatusV2 from (tDomain tos) (Id rid) (unStatus s)) (Vector.toList rows)
        )
  LookupAllStatuses lfroms -> do
    let froms = tUnqualified lfroms
    concat
      <$> for
        froms
        ( \u -> do
            localRows <- runStatement u selectAllLocalRelations
            let localStatuses =
                  map (\(rid, s) -> toConnectionStatusV2 u (tDomain lfroms) (Id rid) (unStatus s)) (Vector.toList localRows)
            remoteRows <- runStatement u selectAllRemoteRelations
            let remote =
                  map (\(d, rid, s) -> toConnectionStatusV2 u d (Id rid) (unStatus s)) (Vector.toList remoteRows)
            pure (localStatuses <> remote)
        )
  RemoteConnectedUsersPaginated u _mps bounds -> do
    -- This operation is only consumed by Brig.IO.Intra.withChunks, which always
    -- starts from an empty paging state and iterates until the page reports
    -- hasMore == False. The Postgres branch therefore serves the complete result
    -- as one page with a keyset walk instead of a continuation.
    rows <- fetchAllRemoteConnected u (fromRange bounds)
    let items =
          map
            ( \(d, rid, s, ts, cdomain, cid) ->
                toRemoteUnsafe d (remoteRowToUserConnectionByRow u (d, rid, s, ts, cdomain, cid))
            )
            (concatMap Vector.toList rows)
    pure (InternalPage (emptyPage, \() -> error "unreachable: no continuation for a complete page", items))
  LookupContactListWithRelation u -> do
    rows <- runStatement u selectAllLocalRelations
    pure $ map (bimap Id unStatus) (Vector.toList rows)
  CountConnections u r -> do
    let codes = relationStatusCodes r
    localCount <- runStatement (tUnqualified u, codes) countLocalConnectionsByStatus
    remoteCount <- runStatement (tUnqualified u, codes) countRemoteConnectionsByStatus
    pure (localCount + remoteCount)

-- | All stored 'RelationWithHistory' status codes whose relation is in the
-- given set (the Postgres analogue of the Cassandra 'countConnections' fold
-- over 'relationDropHistory').
relationStatusCodes :: [Relation] -> [Int32]
relationStatusCodes rs =
  [ postgresMarshall rwh
  | rwh <-
      [ AcceptedWithHistory,
        BlockedWithHistory,
        PendingWithHistory,
        IgnoredWithHistory,
        SentWithHistory,
        CancelledWithHistory,
        MissingLegalholdConsentFromAccepted,
        MissingLegalholdConsentFromBlocked,
        MissingLegalholdConsentFromPending,
        MissingLegalholdConsentFromIgnored,
        MissingLegalholdConsentFromSent,
        MissingLegalholdConsentFromCancelled
      ],
    relationDropHistory rwh `elem` rs
  ]

countLocalConnectionsByStatus :: Hasql.Statement (UserId, [Int32]) Int64
countLocalConnectionsByStatus =
  lmapPG @(UUID, Vector.Vector Int32) @(UserId, [Int32])
    [singletonStatement|SELECT (count(*) FILTER (WHERE status = ANY ($2 :: int4[])) :: int8)
                        FROM connection WHERE "left" = ($1 :: uuid)|]

countRemoteConnectionsByStatus :: Hasql.Statement (UserId, [Int32]) Int64
countRemoteConnectionsByStatus =
  lmapPG @(UUID, Vector.Vector Int32) @(UserId, [Int32])
    [singletonStatement|SELECT (count(*) FILTER (WHERE status = ANY ($2 :: int4[])) :: int8)
                        FROM connection_remote WHERE "left" = ($1 :: uuid)|]

lastRow :: [a] -> Maybe a
lastRow = listToMaybe . reverse

-- | Shared construction of a Cassandra-style page from a keyset page: the
-- marker is the last row iff the page came back full.
buildPageWithState ::
  (row -> UserConnection) ->
  (row -> ByteString) ->
  Int32 ->
  Vector.Vector row ->
  Cas.PageWithState ByteString UserConnection
buildPageWithState toUC mkMarker size rows =
  PageWithState
    { pwsResults = map toUC xs,
      pwsState = PaginationStatePostgres <$> mMarker
    }
  where
    xs = Vector.toList rows
    mMarker = if length xs == fromIntegral size then mkMarker <$> lastRow xs else Nothing

-- | Walk all pages of a user's remote connections (keyset by
-- (right_domain, right_user)) and return the row vectors as a list.
fetchAllRemoteConnected ::
  (PGConstraints r) =>
  Local UserId ->
  Int32 ->
  Sem r [Vector.Vector (Domain, UUID, Int32, UTCTime, Maybe Domain, Maybe UUID)]
fetchAllRemoteConnected u limitSize = go Nothing []
  where
    go mStart acc = do
      rows <- case mStart of
        Just (d, rid) -> runStatement (tUnqualified u, d, rid, limitSize) selectRemoteConnectionsFrom
        Nothing -> runStatement (tUnqualified u, limitSize) selectRemoteConnections
      let acc' = rows : acc
      if Vector.length rows < fromIntegral limitSize
        then pure (reverse acc')
        else case lastRow (Vector.toList rows) of
          Just (d, rid, _, _, _, _) -> go (Just (d, Id rid)) acc'
          Nothing -> pure (reverse acc')

--------------------------------------------------------------------------------
-- Row conversions

unStatus :: Int32 -> RelationWithHistory
unStatus = either (error . Text.unpack) id . postgresUnmarshall

-- | (right uuid, status, last_update, conv uuid?)
localRowToUserConnection :: Local UserId -> Qualified UserId -> (UUID, Int32, UTCTime, Maybe UUID) -> UserConnection
localRowToUserConnection self target (_rid, s, ts, mcnv) =
  UserConnection
    { ucFrom = tUnqualified self,
      ucTo = target,
      ucStatus = relationDropHistory (unStatus s),
      ucLastUpdate = toUTCTimeMillis ts,
      ucConvId = fmap (tUntagged . qualifyAs self . Id) mcnv
    }

-- | Same, but the target user is taken from the row.
localRowToUserConnectionByRight :: Local UserId -> (UUID, Int32, UTCTime, Maybe UUID) -> UserConnection
localRowToUserConnectionByRight self row@(rid, _, _, _) =
  localRowToUserConnection self (tUntagged (qualifyAs self (Id rid))) row

-- | (status, last_update, conv_domain?, conv_id?)
remoteRowToUserConnection :: Local UserId -> Qualified UserId -> (Int32, UTCTime, Maybe Domain, Maybe UUID) -> UserConnection
remoteRowToUserConnection self target (s, ts, mcdomain, mcnv) =
  UserConnection
    { ucFrom = tUnqualified self,
      ucTo = target,
      ucStatus = relationDropHistory (unStatus s),
      ucLastUpdate = toUTCTimeMillis ts,
      ucConvId = liftA2 Qualified (Id <$> mcnv) mcdomain
    }

-- | (right_domain, right_user, status, last_update, conv_domain?, conv_id?)
remoteRowToUserConnectionByRow :: Local UserId -> (Domain, UUID, Int32, UTCTime, Maybe Domain, Maybe UUID) -> UserConnection
remoteRowToUserConnectionByRow self (d, rid, s, ts, mcdomain, mcnv) =
  remoteRowToUserConnection self (Qualified (Id rid) d) (s, ts, mcdomain, mcnv)

-- | Trim to the requested size, mirroring the Cassandra \"fetch one extra\"
-- behaviour of the original 'lookupLocalConnections'.
trimLocalPage :: Local UserId -> Int32 -> Vector.Vector (UUID, Int32, UTCTime, Maybe UUID) -> Cas.Page UserConnection
trimLocalPage lfrom size rows =
  emptyPage
    { result = take (fromIntegral size) (map (localRowToUserConnectionByRight lfrom) (Vector.toList rows)),
      hasMore = Vector.length rows > fromIntegral size
    }

--------------------------------------------------------------------------------
-- Paging-state markers

-- | Local page marker: the 36-char ASCII encoding of the last connection's
-- @right@ user id. The inverse of the 'UUID.toASCIIBytes' marker built in
-- 'LookupLocalConnectionsPage'.
decodeLocalMarker :: ByteString -> Maybe UserId
decodeLocalMarker = fmap Id . UUID.fromASCIIBytes

-- | Remote page marker: utf8-encoded domain bytes followed by the 36-char
-- ASCII encoding of the user id.
encodeRemoteMarker :: Domain -> UUID -> ByteString
encodeRemoteMarker domain uid = toByteString' domain <> UUID.toASCIIBytes uid

-- | Inverse of 'encodeRemoteMarker': a 36-char ASCII uuid suffix.
decodeRemoteMarker :: ByteString -> Maybe (Domain, UserId)
decodeRemoteMarker bs = do
  guard (BS.length bs >= 36)
  let (domainPart, uidPart) = BS.splitAt (BS.length bs - 36) bs
  (,) <$> fromByteString domainPart <*> decodeLocalMarker uidPart

--------------------------------------------------------------------------------
-- Statements
-- NOTE: \"left\" and \"right\" are reserved words in PostgreSQL and must be
-- quoted.

insertLocal :: Hasql.Statement (UserId, UserId, Int32, UTCTime, ConvId) ()
insertLocal =
  lmapPG
    [resultlessStatement|INSERT INTO connection ("left", "right", status, last_update, conv)
                         VALUES ($1 :: uuid, $2 :: uuid, $3 :: int4, $4 :: timestamptz, $5 :: uuid)
                         ON CONFLICT ("left", "right") DO UPDATE
                         SET status = ($3 :: int4), last_update = ($4 :: timestamptz), conv = ($5 :: uuid)
                        |]

-- | Insert a remote connection row directly, bypassing the 'ConnectionStore'
-- effect (which cannot create a connection to an arbitrary remote domain).
-- Exposed for the brig integration suite, mirroring the raw Cassandra insert
-- via 'Wire.ConnectionStore.Cassandra.remoteConnectionInsert'. 'Hasql.UsageError'
-- has no 'Exception' instance in this hasql version, so failures surface as
-- an 'ErrorCall'.
insertRemoteConnectionRaw ::
  HasqlPoolExt.Pool ->
  (UserId, Domain, UserId, RelationWithHistory, UTCTimeMillis, Domain, ConvId) ->
  IO ()
insertRemoteConnectionRaw pool (l, rdom, r, rel, now, cdom, cnv) =
  useWithResetAndRetry
    pool
    (statement (l, rdom, r, postgresMarshall rel, fromUTCTimeMillis now, cdom, cnv) insertRemote)
    >>= either (throwIO . ErrorCall . show) pure

insertRemote :: Hasql.Statement (UserId, Domain, UserId, Int32, UTCTime, Domain, ConvId) ()
insertRemote =
  lmapPG
    [resultlessStatement|INSERT INTO connection_remote ("left", right_domain, right_user, status, last_update, conv_domain, conv_id)
                         VALUES ($1 :: uuid, $2 :: text, $3 :: uuid, $4 :: int4, $5 :: timestamptz, $6 :: text, $7 :: uuid)
                         ON CONFLICT ("left", right_domain, right_user) DO UPDATE
                         SET status = ($4 :: int4), last_update = ($5 :: timestamptz), conv_domain = ($6 :: text), conv_id = ($7 :: uuid)
                        |]

updateLocal :: Hasql.Statement (Int32, UTCTime, UserId, UserId) ()
updateLocal =
  lmapPG
    [resultlessStatement|UPDATE connection SET status = ($1 :: int4), last_update = ($2 :: timestamptz)
                         WHERE "left" = ($3 :: uuid) AND "right" = ($4 :: uuid)
                        |]

updateRemote :: Hasql.Statement (Int32, UTCTime, UserId, Domain, UserId) ()
updateRemote =
  lmapPG
    [resultlessStatement|UPDATE connection_remote SET status = ($1 :: int4), last_update = ($2 :: timestamptz)
                         WHERE "left" = ($3 :: uuid) AND right_domain = ($4 :: text) AND right_user = ($5 :: uuid)
                        |]

deleteLocalConnections :: Hasql.Statement UserId ()
deleteLocalConnections =
  lmapPG
    [resultlessStatement|DELETE FROM connection WHERE "left" = ($1 :: uuid)|]

-- | Deletes the mirror rows pointing at the deleted user ((left=contact,
-- right=u) for every contact of u), mirroring the Cassandra per-contact
-- 'connectionDelete' calls in 'Wire.ConnectionStore.Cassandra.deleteConnections'.
deleteIncomingLocalConnections :: Hasql.Statement UserId ()
deleteIncomingLocalConnections =
  lmapPG
    [resultlessStatement|DELETE FROM connection c
                         WHERE c."right" = ($1 :: uuid)
                           AND c."left" = ANY (SELECT s."right" FROM connection s WHERE s."left" = ($1 :: uuid))
                        |]

deleteRemoteConnectionsByLeft :: Hasql.Statement UserId ()
deleteRemoteConnectionsByLeft =
  lmapPG
    [resultlessStatement|DELETE FROM connection_remote WHERE "left" = ($1 :: uuid)|]

deleteRemoteConnectionsByUsers :: Hasql.Statement (UserId, Domain, [UserId]) ()
deleteRemoteConnectionsByUsers =
  lmapPG @(UUID, Text, Vector.Vector UUID) @(UserId, Domain, [UserId])
    [resultlessStatement|DELETE FROM connection_remote
                         WHERE "left" = ANY ($3 :: uuid[]) AND right_domain = ($2 :: text) AND right_user = ($1 :: uuid)
                        |]

selectLocalConnection :: Hasql.Statement (UserId, UserId) (Maybe (UUID, Int32, UTCTime, Maybe UUID))
selectLocalConnection =
  dimapPG
    [maybeStatement|SELECT ("right" :: uuid), (status :: int4), (last_update :: timestamptz), (conv :: uuid?)
                    FROM connection WHERE "left" = ($1 :: uuid) AND "right" = ($2 :: uuid)
                   |]

selectRemoteConnection :: Hasql.Statement (UserId, Domain, UserId) (Maybe (Int32, UTCTime, Maybe Domain, Maybe UUID))
selectRemoteConnection =
  dimapPG
    [maybeStatement|SELECT (status :: int4), (last_update :: timestamptz), (conv_domain :: text?), (conv_id :: uuid?)
                    FROM connection_remote
                    WHERE "left" = ($1 :: uuid) AND right_domain = ($2 :: text) AND right_user = ($3 :: uuid)
                   |]

selectLocalRelation :: Hasql.Statement (UserId, UserId) (Maybe Int32)
selectLocalRelation =
  dimapPG
    [maybeStatement|SELECT (status :: int4) FROM connection WHERE "left" = ($1 :: uuid) AND "right" = ($2 :: uuid)|]

selectRemoteRelation :: Hasql.Statement (UserId, Domain, UserId) (Maybe Int32)
selectRemoteRelation =
  dimapPG
    [maybeStatement|SELECT (status :: int4) FROM connection_remote
                    WHERE "left" = ($1 :: uuid) AND right_domain = ($2 :: text) AND right_user = ($3 :: uuid)
                   |]

-- | LIMIT is size + 1 in the caller, mirroring the Cassandra \"fetch one
-- extra\" behaviour.
selectLocalConnections :: Hasql.Statement (UserId, Int32) (Vector.Vector (UUID, Int32, UTCTime, Maybe UUID))
selectLocalConnections =
  dimapPG
    [vectorStatement|SELECT ("right" :: uuid), (status :: int4), (last_update :: timestamptz), (conv :: uuid?)
                     FROM connection WHERE "left" = ($1 :: uuid)
                     ORDER BY "right" ASC LIMIT ($2 :: int4)
                    |]

selectLocalConnectionsFrom :: Hasql.Statement (UserId, UserId, Int32) (Vector.Vector (UUID, Int32, UTCTime, Maybe UUID))
selectLocalConnectionsFrom =
  dimapPG
    [vectorStatement|SELECT ("right" :: uuid), (status :: int4), (last_update :: timestamptz), (conv :: uuid?)
                     FROM connection WHERE "left" = ($1 :: uuid) AND "right" > ($2 :: uuid)
                     ORDER BY "right" ASC LIMIT ($3 :: int4)
                    |]

selectRemoteConnections :: Hasql.Statement (UserId, Int32) (Vector.Vector (Domain, UUID, Int32, UTCTime, Maybe Domain, Maybe UUID))
selectRemoteConnections =
  dimapPG
    [vectorStatement|SELECT (right_domain :: text), (right_user :: uuid), (status :: int4), (last_update :: timestamptz), (conv_domain :: text?), (conv_id :: uuid?)
                     FROM connection_remote WHERE "left" = ($1 :: uuid)
                     ORDER BY right_domain ASC, right_user ASC LIMIT ($2 :: int4)
                    |]

selectRemoteConnectionsFrom :: Hasql.Statement (UserId, Domain, UserId, Int32) (Vector.Vector (Domain, UUID, Int32, UTCTime, Maybe Domain, Maybe UUID))
selectRemoteConnectionsFrom =
  dimapPG
    [vectorStatement|SELECT (right_domain :: text), (right_user :: uuid), (status :: int4), (last_update :: timestamptz), (conv_domain :: text?), (conv_id :: uuid?)
                     FROM connection_remote
                     WHERE "left" = ($1 :: uuid) AND (right_domain, right_user) > (($2 :: text), ($3 :: uuid))
                     ORDER BY right_domain ASC, right_user ASC LIMIT ($4 :: int4)
                    |]

selectConnectionStatuses :: Hasql.Statement ([UserId], [UserId]) (Vector.Vector (UUID, UUID, Int32))
selectConnectionStatuses =
  dimapPG @(Vector.Vector UUID, Vector.Vector UUID) @([UserId], [UserId])
    [vectorStatement|SELECT ("left" :: uuid), ("right" :: uuid), (status :: int4)
                     FROM connection WHERE "left" = ANY ($1 :: uuid[]) AND "right" = ANY ($2 :: uuid[])
                    |]

selectConnectionStatuses' :: Hasql.Statement [UserId] (Vector.Vector (UUID, UUID, Int32))
selectConnectionStatuses' =
  dimapPG @(Vector.Vector UUID) @[UserId]
    [vectorStatement|SELECT ("left" :: uuid), ("right" :: uuid), (status :: int4)
                     FROM connection WHERE "left" = ANY ($1 :: uuid[])
                    |]

selectLocalRelations :: Hasql.Statement (UserId, [UserId]) (Vector.Vector (UUID, Int32))
selectLocalRelations =
  dimapPG @(UUID, Vector.Vector UUID) @(UserId, [UserId])
    [vectorStatement|SELECT ("right" :: uuid), (status :: int4)
                     FROM connection WHERE "left" = ($1 :: uuid) AND "right" = ANY ($2 :: uuid[])
                    |]

selectRemoteRelations :: Hasql.Statement (UserId, Domain, [UserId]) (Vector.Vector (UUID, Int32))
selectRemoteRelations =
  dimapPG @(UUID, Text, Vector.Vector UUID) @(UserId, Domain, [UserId])
    [vectorStatement|SELECT (right_user :: uuid), (status :: int4)
                     FROM connection_remote WHERE "left" = ($1 :: uuid) AND right_domain = ($2 :: text) AND right_user = ANY ($3 :: uuid[])
                    |]

selectAllLocalRelations :: Hasql.Statement UserId (Vector.Vector (UUID, Int32))
selectAllLocalRelations =
  dimapPG
    [vectorStatement|SELECT ("right" :: uuid), (status :: int4)
                     FROM connection WHERE "left" = ($1 :: uuid)
                    |]

selectAllRemoteRelations :: Hasql.Statement UserId (Vector.Vector (Domain, UUID, Int32))
selectAllRemoteRelations =
  dimapPG
    [vectorStatement|SELECT (right_domain :: text), (right_user :: uuid), (status :: int4)
                     FROM connection_remote WHERE "left" = ($1 :: uuid)
                    |]
