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

-- | Postgres interpreter for the client store.
--
-- Unlike the Cassandra interpreter, no locking (local 'MVar' or DynamoDB-based
-- optimistic locking) is needed for 'ClaimPrekey': the single atomic
-- @DELETE … RETURNING@ statement already serialises concurrent claims, so the
-- locking machinery would only add contention, not correctness.
module Wire.ClientStore.Postgres
  ( interpretClientStoreToPostgres,
    deletePrekeyImpl,
  )
where

import Data.ByteString.Conversion (toByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Id
import Data.Json.Util
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Polysemy.TinyLog
import System.Logger.Message
import Wire.API.MLS.CipherSuite
import Wire.API.PostgresMarshall
import Wire.API.User.Auth (CookieLabel)
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.UserMap
import Wire.ClientStore
import Wire.Postgres
import Wire.Sem.Logger qualified as Log

interpretClientStoreToPostgres ::
  (PGConstraints r, Member TinyLog r) =>
  InterpreterFor ClientStore r
interpretClientStoreToPostgres = interpret $ \case
  -- Lifecycle
  Upsert uid cid timestamp nc -> upsertImpl uid cid timestamp nc
  Delete uid cid -> deleteImpl uid cid
  UpdateLabel uid cid lbl -> runStatement (lbl, uid, cid) updateClientLabelStatement
  UpdateCapabilities uid cid caps ->
    runStatement (clientCapsToVector <$> caps, uid, cid) updateClientCapabilitiesStatement
  UpdateLastActive uid cid timestamp ->
    runStatement (timestamp, uid, cid) updateClientLastActiveStatement
  -- Lookups
  LookupClient uid cid -> lookupClientImpl uid cid
  LookupClients uid -> lookupClientsImpl uid
  LookupClientIds uid ->
    V.toList <$> runStatement uid selectClientIdsStatement
  LookupClientIdsBulk uids -> lookupClientIdsBulkImpl uids
  LookupClientsBulk uids -> lookupClientsBulkImpl uids
  LookupPubClientsBulk uids -> lookupPubClientsBulkImpl uids
  LookupPrekeyIds uid cid ->
    V.toList <$> runStatement (uid, cid) selectPrekeyIdsStatement
  LookupPrekeyPresenceBulk pairs -> lookupPrekeyPresenceBulkImpl pairs
  GetActivityTimestamps uid ->
    V.toList <$> runStatement uid selectActivityTimestampsStatement
  -- Proteus
  UpdatePrekeys uid cid prekeys -> updatePrekeysImpl uid cid prekeys
  ClaimPrekey uid cid -> claimPrekeyImpl uid cid
  -- MLS
  AddMLSPublicKeys uid cid keys -> addMLSPublicKeysImpl uid cid keys
  LookupMLSPublicKey uid cid scheme ->
    fmap LBS.fromStrict <$> runStatement (uid, cid, scheme) selectMLSPublicKeyStatement

--------------------------------------------------------------------------------
-- Lifecycle

-- | Mirrors the Cassandra 'upsertImpl': insert prekeys (including the
-- last-resort key), upsert the client row, then guarded-insert the MLS public
-- keys, short-circuiting on the first duplicate.
upsertImpl ::
  (PGConstraints r) =>
  UserId ->
  ClientId ->
  UTCTimeMillis ->
  NewClient ->
  Sem r (Maybe DuplicateMLSPublicKey)
upsertImpl uid cid now c = do
  let keys = unpackLastPrekey (newClientLastKey c) : newClientPrekeys c
  updatePrekeysImpl uid cid keys
  runStatement
    ( uid,
      cid,
      now,
      newClientType c,
      newClientLabel c,
      newClientClass c,
      newClientCookie c,
      newClientModel c,
      clientCapsToVector <$> newClientCapabilities c
    )
    upsertClientStatement
  addMLSPublicKeysImpl uid cid (Map.assocs (newClientMLSPublicKeys c))

deleteImpl :: (PGConstraints r) => UserId -> ClientId -> Sem r ()
deleteImpl uid cid = do
  runStatement (uid, cid) deleteClientStatement
  runStatement (uid, cid) deletePrekeysStatement

clientCapsToVector :: ClientCapabilityList -> Vector Int32
clientCapsToVector =
  V.fromList . map postgresMarshall . Set.toList . fromClientCapabilityList

--------------------------------------------------------------------------------
-- Proteus

updatePrekeysImpl ::
  (PGConstraints r) =>
  UserId ->
  ClientId ->
  [UncheckedPrekeyBundle] ->
  Sem r ()
updatePrekeysImpl uid cid pks =
  runStatement
    (uid, cid, V.fromList (map (postgresMarshall . prekeyId) pks), V.fromList (map prekeyKey pks))
    upsertPrekeysStatement

-- | Atomically claims (deletes and returns) one prekey of the client.
--
-- The Cassandra interpreter needed an 'MVar' or a DynamoDB-based optimistic
-- lock around its select-then-delete to avoid double claims; the Postgres
-- version does the selection and deletion in a single statement, so no
-- additional locking is necessary. Like the Cassandra implementation the
-- last-resort prekey (id 65535 = @lastPrekeyId@, see
-- "Wire.API.User.Client.Prekey") is never consumed, only served: the claim
-- deletes one of the other keys, and if none is left the last-resort key is
-- served via a plain select without deleting it. The literal 65535 below must
-- stay in sync with 'lastPrekeyId'.
claimPrekeyImpl ::
  (PGConstraints r, Member TinyLog r) =>
  UserId ->
  ClientId ->
  Sem r (Maybe ClientPrekey)
claimPrekeyImpl uid cid = do
  mKey <- runStatement (uid, cid) claimPrekeyStatement
  mKey' <- case mKey of
    Just _ -> pure mKey
    -- unless we only have the last-resort prekey left
    Nothing -> runStatement (uid, cid) selectLastPrekeyStatement
  when (fmap fst mKey' == Just (postgresMarshall lastPrekeyId)) $
    Log.debug $
      field "user" (toByteString uid)
        . field "client" (toByteString cid)
        . msg (val "last resort prekey used")
  pure $ do
    (key, data_) <- mKey'
    prekeyId <- either (const Nothing) Just (postgresUnmarshall key)
    pure $ ClientPrekey cid (UncheckedPrekeyBundle prekeyId data_)

--------------------------------------------------------------------------------
-- MLS

addMLSPublicKeysImpl ::
  (PGConstraints r) =>
  UserId ->
  ClientId ->
  [(SignatureSchemeTag, ByteString)] ->
  Sem r (Maybe DuplicateMLSPublicKey)
addMLSPublicKeysImpl uid cid keys = go keys
  where
    go [] = pure Nothing
    go ((ss, pk) : rest) = do
      rows <- runStatement (uid, cid, ss, pk) insertMLSPublicKeyStatement
      if V.null rows
        then pure $ Just DuplicateMLSPublicKey
        else go rest

--------------------------------------------------------------------------------
-- Lookups

lookupClientImpl :: (PGConstraints r) => UserId -> ClientId -> Sem r (Maybe Client)
lookupClientImpl uid cid = do
  mRow <- runStatement (uid, cid) selectClientStatement
  case mRow of
    Nothing -> pure Nothing
    Just row -> do
      keys <- lookupMLSPublicKeysFor uid cid
      pure $ Just (mkClient (Map.findWithDefault [] cid keys) row)

lookupClientsImpl :: (PGConstraints r) => UserId -> Sem r [Client]
lookupClientsImpl uid = do
  rows <- V.toList <$> runStatement uid selectClientsStatement
  keyMap <- lookupMLSPublicKeysForUser uid
  pure
    [ mkClient (Map.findWithDefault [] cid keyMap) row
    | row@(_, cid, _, _, _, _, _, _, _, _) <- rows
    ]

-- | All MLS public keys of one client.
lookupMLSPublicKeysFor ::
  (PGConstraints r) =>
  UserId ->
  ClientId ->
  Sem r (Map ClientId [(SignatureSchemeTag, ByteString)])
lookupMLSPublicKeysFor uid cid =
  Map.singleton cid . V.toList
    <$> runStatement (uid, cid) selectMLSPublicKeysStatement

lookupMLSPublicKeysForUser ::
  (PGConstraints r) =>
  UserId ->
  Sem r (Map ClientId [(SignatureSchemeTag, ByteString)])
lookupMLSPublicKeysForUser uid = do
  rows <- V.toList <$> runStatement uid selectMLSPublicKeysByUserStatement
  pure $ Map.fromListWith (<>) [(cid, [(ss, bs)]) | (cid, ss, bs) <- rows]

lookupClientIdsBulkImpl :: (PGConstraints r) => [UserId] -> Sem r UserClients
lookupClientIdsBulkImpl uids = do
  rows <- V.toList <$> runStatement uids selectClientIdsBulkStatement
  pure . UserClients $
    Map.fromList [(u, Set.fromList [c | (u', c) <- rows, u' == u]) | u <- uids]

lookupClientsBulkImpl :: (PGConstraints r) => [UserId] -> Sem r (UserMap (Set Client))
lookupClientsBulkImpl uids = do
  rows' <- runStatement uids selectClientsByUsersStatement
  keyRows <- V.toList <$> runStatement uids selectMLSPublicKeysByUsersStatement
  let keyMap =
        Map.fromListWith (<>) [(u, Map.singleton cid [(ss, bs)]) | (u, cid, ss, bs) <- keyRows]
      rowsByUser =
        Map.fromListWith
          (<>)
          [ (u, [(cid, row)]) | row@(u, cid, _, _, _, _, _, _, _, _) <- V.toList rows'
          ]
  pure . UserMap $
    Map.mapWithKey
      ( \u cs ->
          let km = Map.findWithDefault Map.empty u keyMap
           in Set.fromList [mkClient (Map.findWithDefault [] cid km) row | (cid, row) <- cs]
      )
      (Map.union rowsByUser (Map.fromList [(u, []) | u <- uids]))

-- | Public client lookups for many users in one query.
lookupPubClientsBulkImpl ::
  (PGConstraints r) =>
  [UserId] ->
  Sem r (UserMap (Set PubClient))
lookupPubClientsBulkImpl uids = do
  rows <- V.toList <$> runStatement uids selectPubClientsBulkStatement
  pure . UserMap $
    Map.fromList
      [ (u, Set.fromList [PubClient c cl | (u', c, cl) <- rows, u' == u])
      | u <- uids
      ]

lookupPrekeyPresenceBulkImpl ::
  (PGConstraints r) =>
  [(UserId, ClientId)] ->
  Sem r (Set (UserId, ClientId))
lookupPrekeyPresenceBulkImpl pairs
  | null pairs = pure Set.empty
  | otherwise = do
      rows <-
        V.toList
          <$> runStatement
            (V.fromList (map (toUUID . fst) pairs), V.fromList (map (postgresMarshall . snd) pairs))
            selectPrekeyPresenceBulkStatement
      pure . Set.fromList $ [(u, c) | (u, c) <- rows]

--------------------------------------------------------------------------------
-- Conversions

mkClient ::
  [(SignatureSchemeTag, ByteString)] ->
  ClientRow ->
  Client
mkClient keys (_uid, cid, cty, tme, lbl, cls, cok, mdl, cps, lastActive) =
  Client
    { clientId = cid,
      clientType = cty,
      clientTime = tme,
      clientClass = cls,
      clientLabel = lbl,
      clientCookie = cok,
      clientModel = mdl,
      clientCapabilities = ClientCapabilityList (maybe Set.empty (fromRight Set.empty . postgresUnmarshall) cps),
      clientMLSPublicKeys = Map.fromList keys,
      clientLastActive = lastActive
    }

--------------------------------------------------------------------------------
-- Statements

upsertClientStatement ::
  Hasql.Statement
    ( UserId,
      ClientId,
      UTCTimeMillis,
      ClientType,
      Maybe Text,
      Maybe ClientClass,
      Maybe CookieLabel,
      Maybe Text,
      Maybe (Vector Int32)
    )
    ()
upsertClientStatement =
  lmapPG
    @( UUID.UUID,
       Text,
       UTCTime,
       Int32,
       Maybe Text,
       Maybe Int32,
       Maybe Text,
       Maybe Text,
       Maybe (Vector Int32)
     )
    @( UserId,
       ClientId,
       UTCTimeMillis,
       ClientType,
       Maybe Text,
       Maybe ClientClass,
       Maybe CookieLabel,
       Maybe Text,
       Maybe (Vector Int32)
     )
    [resultlessStatement|INSERT INTO clients
                           (user_id, client_id, tstamp, type, label, class, cookie, model, capabilities)
                         VALUES
                           ($1 :: uuid, $2 :: text, $3 :: timestamptz, $4 :: int, $5 :: text?, $6 :: int?, $7 :: text?, $8 :: text?, $9 :: int[]?)
                         ON CONFLICT (user_id, client_id) DO UPDATE
                         SET tstamp = ($3 :: timestamptz),
                             type = ($4 :: int),
                             label = ($5 :: text?),
                             class = ($6 :: int?),
                             cookie = ($7 :: text?),
                             model = ($8 :: text?),
                             capabilities = ($9 :: int[]?)
       |]

updateClientLabelStatement :: Hasql.Statement (Maybe Text, UserId, ClientId) ()
updateClientLabelStatement =
  lmapPG
    [resultlessStatement|UPDATE clients
                         SET label = ($1 :: text?)
                         WHERE user_id = ($2 :: uuid) AND client_id = ($3 :: text)
                        |]

updateClientCapabilitiesStatement ::
  Hasql.Statement (Maybe (Vector Int32), UserId, ClientId) ()
updateClientCapabilitiesStatement =
  lmapPG
    @(Maybe (Vector Int32), UUID.UUID, Text)
    @(Maybe (Vector Int32), UserId, ClientId)
    [resultlessStatement|UPDATE clients
                         SET capabilities = ($1 :: int[]?)
                         WHERE user_id = ($2 :: uuid) AND client_id = ($3 :: text)
                        |]

-- | A no-op update if the client does not exist (mirrors the Cassandra
-- interpreter which deliberately skips @IF EXISTS@).
updateClientLastActiveStatement :: Hasql.Statement (UTCTime, UserId, ClientId) ()
updateClientLastActiveStatement =
  lmapPG
    [resultlessStatement|UPDATE clients
                         SET last_active = ($1 :: timestamptz)
                         WHERE user_id = ($2 :: uuid) AND client_id = ($3 :: text)
                        |]

upsertPrekeysStatement ::
  Hasql.Statement (UserId, ClientId, Vector Int32, Vector Text) ()
upsertPrekeysStatement =
  lmapPG
    @(UUID.UUID, Text, Vector Int32, Vector Text)
    @(UserId, ClientId, Vector Int32, Vector Text)
    [resultlessStatement|INSERT INTO prekeys (user_id, client_id, key, data)
                         SELECT ($1 :: uuid), ($2 :: text), key, data
                         FROM unnest(($3 :: int[]), ($4 :: text[])) AS t(key, data)
                         ON CONFLICT (user_id, client_id, key) DO UPDATE
                         SET data = EXCLUDED.data
                        |]

deleteClientStatement :: Hasql.Statement (UserId, ClientId) ()
deleteClientStatement =
  lmapPG
    [resultlessStatement|DELETE FROM clients
                         WHERE user_id = ($1 :: uuid) AND client_id = ($2 :: text)
                        |]

deletePrekeysStatement :: Hasql.Statement (UserId, ClientId) ()
deletePrekeysStatement =
  lmapPG
    [resultlessStatement|DELETE FROM prekeys
                         WHERE user_id = ($1 :: uuid) AND client_id = ($2 :: text)
                        |]

-- | Delete a single claimed prekey; used by the dual-write interpreter to
-- mirror a Cassandra prekey claim to Postgres.
deletePrekeyImpl :: (PGConstraints r) => UserId -> ClientId -> PrekeyId -> Sem r ()
deletePrekeyImpl uid cid k =
  runStatement (uid, cid, postgresMarshall k) deletePrekeyStatement

deletePrekeyStatement :: Hasql.Statement (UserId, ClientId, Int32) ()
deletePrekeyStatement =
  lmapPG
    [resultlessStatement|DELETE FROM prekeys
                         WHERE user_id = ($1 :: uuid) AND client_id = ($2 :: text) AND key = ($3 :: int)
                        |]

claimPrekeyStatement :: Hasql.Statement (UserId, ClientId) (Maybe (Int32, Text))
claimPrekeyStatement =
  lmapPG
    @(UUID.UUID, Text)
    @(UserId, ClientId)
    [maybeStatement|DELETE FROM prekeys
                    WHERE user_id = ($1 :: uuid)
                      AND client_id = ($2 :: text)
                      AND key = (SELECT key FROM prekeys
                                 WHERE user_id = ($1 :: uuid) AND client_id = ($2 :: text)
                                   AND key <> 65535
                                 ORDER BY random()
                                 LIMIT 1)
                    RETURNING key :: int, data :: text
                   |]

-- | The last-resort prekey is served but never consumed; mirrors the
-- Cassandra @pickRandomPrekey@ fallback when it is the only key left.
selectLastPrekeyStatement :: Hasql.Statement (UserId, ClientId) (Maybe (Int32, Text))
selectLastPrekeyStatement =
  lmapPG
    @(UUID.UUID, Text)
    @(UserId, ClientId)
    [maybeStatement|SELECT key :: int, data :: text
                    FROM prekeys
                    WHERE user_id = ($1 :: uuid) AND client_id = ($2 :: text) AND key = 65535
                   |]

insertMLSPublicKeyStatement ::
  Hasql.Statement (UserId, ClientId, SignatureSchemeTag, ByteString) (Vector Bool)
insertMLSPublicKeyStatement =
  lmapPG
    [vectorStatement|INSERT INTO mls_public_keys (user_id, client_id, sig_scheme, key)
                     VALUES (($1 :: uuid), ($2 :: text), ($3 :: text), ($4 :: bytea))
                     ON CONFLICT (user_id, client_id, sig_scheme) DO NOTHING
                     RETURNING true :: boolean
                    |]

selectMLSPublicKeyStatement ::
  Hasql.Statement (UserId, ClientId, SignatureSchemeTag) (Maybe ByteString)
selectMLSPublicKeyStatement =
  lmapPG
    [maybeStatement|SELECT key :: bytea
                    FROM mls_public_keys
                    WHERE user_id = ($1 :: uuid) AND client_id = ($2 :: text) AND sig_scheme = ($3 :: text)
                   |]

selectMLSPublicKeysStatement ::
  Hasql.Statement (UserId, ClientId) (Vector (SignatureSchemeTag, ByteString))
selectMLSPublicKeysStatement =
  dimapPG
    [vectorStatement|SELECT sig_scheme :: text, key :: bytea
                     FROM mls_public_keys
                     WHERE user_id = ($1 :: uuid) AND client_id = ($2 :: text)
                    |]

selectMLSPublicKeysByUserStatement ::
  Hasql.Statement UserId (Vector (ClientId, SignatureSchemeTag, ByteString))
selectMLSPublicKeysByUserStatement =
  dimapPG
    [vectorStatement|SELECT client_id :: text, sig_scheme :: text, key :: bytea
                     FROM mls_public_keys
                     WHERE user_id = ($1 :: uuid)
                    |]

selectMLSPublicKeysByUsersStatement ::
  Hasql.Statement [UserId] (Vector (UserId, ClientId, SignatureSchemeTag, ByteString))
selectMLSPublicKeysByUsersStatement =
  dimapPG
    @(Vector UUID.UUID)
    @[UserId]
    @(Vector (UUID.UUID, Text, Text, ByteString))
    @(Vector (UserId, ClientId, SignatureSchemeTag, ByteString))
    [vectorStatement|SELECT user_id :: uuid, client_id :: text, sig_scheme :: text, key :: bytea
                     FROM mls_public_keys
                     WHERE user_id = ANY ($1 :: uuid[])
                    |]

type ClientRow =
  ( UserId,
    ClientId,
    ClientType,
    UTCTimeMillis,
    Maybe Text,
    Maybe ClientClass,
    Maybe CookieLabel,
    Maybe Text,
    Maybe (Vector Int32),
    Maybe UTCTime
  )

selectClientStatement :: Hasql.Statement (UserId, ClientId) (Maybe ClientRow)
selectClientStatement =
  dimapPG
    @(UUID.UUID, Text)
    @(UserId, ClientId)
    @(Maybe (UUID.UUID, Text, Int32, UTCTime, Maybe Text, Maybe Int32, Maybe Text, Maybe Text, Maybe (Vector Int32), Maybe UTCTime))
    @(Maybe ClientRow)
    [maybeStatement|SELECT user_id :: uuid, client_id :: text, type :: int, tstamp :: timestamptz, label :: text?,
                           class :: int?, cookie :: text?, model :: text?,
                           capabilities :: int[]?, last_active :: timestamptz?
                    FROM clients
                    WHERE user_id = ($1 :: uuid) AND client_id = ($2 :: text)
                   |]

selectClientsStatement :: Hasql.Statement UserId (Vector ClientRow)
selectClientsStatement =
  dimapPG
    @UUID.UUID
    @UserId
    @(Vector (UUID.UUID, Text, Int32, UTCTime, Maybe Text, Maybe Int32, Maybe Text, Maybe Text, Maybe (Vector Int32), Maybe UTCTime))
    @(Vector ClientRow)
    [vectorStatement|SELECT user_id :: uuid, client_id :: text, type :: int, tstamp :: timestamptz, label :: text?,
                            class :: int?, cookie :: text?, model :: text?,
                            capabilities :: int[]?, last_active :: timestamptz?
                     FROM clients
                     WHERE user_id = ($1 :: uuid)
                     ORDER BY client_id
                    |]

selectClientsByUsersStatement ::
  Hasql.Statement [UserId] (Vector ClientRow)
selectClientsByUsersStatement =
  dimapPG
    @(Vector UUID.UUID)
    @[UserId]
    @(Vector (UUID.UUID, Text, Int32, UTCTime, Maybe Text, Maybe Int32, Maybe Text, Maybe Text, Maybe (Vector Int32), Maybe UTCTime))
    @(Vector ClientRow)
    [vectorStatement|SELECT user_id :: uuid, client_id :: text, type :: int, tstamp :: timestamptz,
                            label :: text?, class :: int?, cookie :: text?, model :: text?,
                            capabilities :: int[]?, last_active :: timestamptz?
                     FROM clients
                     WHERE user_id = ANY ($1 :: uuid[])
                     ORDER BY user_id, client_id
                    |]

selectClientIdsStatement :: Hasql.Statement UserId (Vector ClientId)
selectClientIdsStatement =
  dimapPG
    [vectorStatement|SELECT client_id :: text
                     FROM clients
                     WHERE user_id = ($1 :: uuid)
                     ORDER BY client_id
                    |]

selectClientIdsBulkStatement ::
  Hasql.Statement [UserId] (Vector (UserId, ClientId))
selectClientIdsBulkStatement =
  dimapPG @(Vector _)
    [vectorStatement|SELECT user_id :: uuid, client_id :: text
                     FROM clients
                     WHERE user_id = ANY ($1 :: uuid[])
                     ORDER BY user_id, client_id
                    |]

selectPubClientsBulkStatement ::
  Hasql.Statement [UserId] (Vector (UserId, ClientId, Maybe ClientClass))
selectPubClientsBulkStatement =
  dimapPG @(Vector _)
    [vectorStatement|SELECT user_id :: uuid, client_id :: text, class :: int?
                     FROM clients
                     WHERE user_id = ANY ($1 :: uuid[])
                     ORDER BY user_id, client_id
                    |]

selectPrekeyIdsStatement ::
  Hasql.Statement (UserId, ClientId) (Vector PrekeyId)
selectPrekeyIdsStatement =
  dimapPG
    [vectorStatement|SELECT key :: int
                     FROM prekeys
                     WHERE user_id = ($1 :: uuid) AND client_id = ($2 :: text)
                     ORDER BY key
                    |]

-- | Zip-matched presence check: both arrays are paired positionally, so a
-- single @JOIN unnest@ must be used rather than two independent @= ANY@
-- comparisons.
selectPrekeyPresenceBulkStatement ::
  Hasql.Statement (Vector UUID.UUID, Vector Text) (Vector (UserId, ClientId))
selectPrekeyPresenceBulkStatement =
  rmapPG
    @(Vector (UUID.UUID, Text))
    @(Vector (UserId, ClientId))
    [vectorStatement|SELECT DISTINCT t.u :: uuid, t.c :: text
                     FROM unnest(($1 :: uuid[]), ($2 :: text[])) AS t(u, c)
                     JOIN prekeys ON prekeys.user_id = t.u AND prekeys.client_id = t.c
                    |]

selectActivityTimestampsStatement ::
  Hasql.Statement UserId (Vector (Maybe UTCTime))
selectActivityTimestampsStatement =
  lmapPG
    [vectorStatement|SELECT last_active :: timestamptz?
                     FROM clients
                     WHERE user_id = ($1 :: uuid)
                     ORDER BY client_id
                    |]
