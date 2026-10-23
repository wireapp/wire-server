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

-- | Background copy loop: full Cassandra scans of @clients@, @prekeys@ and
-- @mls_public_keys@ (brig keyspace) mirrored row-by-row into Postgres.
--
-- Copy-only: Cassandra rows are never deleted. Re-runs are idempotent
-- (@ON CONFLICT DO UPDATE@ for clients/prekeys, @ON CONFLICT DO NOTHING@ for
-- mls_public_keys, mirroring the @IF NOT EXISTS@ semantics of the Cassandra
-- LWT insert).
--
-- Residual race (accepted trade-off, see WPB-22973): a prekey claimed by a
-- concurrent dual-write AFTER the copy loop scanned it is resurrected in
-- Postgres, because the copy's upsert re-materializes the row and the final
-- pass never deletes rows absent from Cassandra. Before cutover to
-- @postgresql@, operators must run a reconciliation pass that removes
-- Postgres prekey rows not present in Cassandra.
module Wire.ClientStore.Migration
  ( migrateClientsLoop,
  )
where

import Cassandra
import Data.ByteString.Conversion (toByteString')
import Data.ByteString.Lazy qualified as LBS
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.IORef qualified as IORef
import Data.Id
import Data.Json.Util (UTCTimeMillis)
import Data.Text qualified as T
import Data.Time
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hasql.Pool.Extended qualified as Hasql
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.AtomicState
import Polysemy.Conc (interpretRace)
import Polysemy.Conc qualified as Conc
import Polysemy.Conc.Effect.Race hiding (Timeout)
import Polysemy.Input
import Polysemy.Resource (Resource, bracket, resourceToIOFinal)
import Polysemy.TinyLog
import Prometheus qualified
import System.Logger qualified as Log
import UnliftIO qualified
import Wire.API.MLS.CipherSuite
import Wire.API.PostgresMarshall
import Wire.API.User.Auth (CookieLabel)
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.Migration
import Wire.Postgres
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)

type EffectStack =
  [ AtomicState Int,
    Input ClientState,
    Input Hasql.Pool,
    Resource,
    Async,
    Race,
    TinyLog,
    Embed IO,
    Final IO
  ]

migrateClientsLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateClientsLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "clients"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "clients")
    (migrateAllClients migOpts migCounter migDuration)

interpreter :: ClientState -> Hasql.Pool -> Log.Logger -> ByteString -> Sem EffectStack a -> IO (Int, a)
interpreter cassClient pgPool logger name =
  runFinal
    . embedToFinal
    . loggerToTinyLog logger
    . mapLogger (Log.field "migration" (Log.val name) .)
    . raiseUnder
    . interpretRace
    . asyncToIOFinal
    . resourceToIOFinal
    . runInputConst pgPool
    . runInputConst cassClient
    . atomicStateToIO 0

-- | One row to copy, tagged by source table.
data CopyRow
  = ClientCopyRow (UserId, ClientRow)
  | PrekeyCopyRow (UserId, ClientId, PrekeyId, Text)
  | MLSKeyCopyRow (UserId, ClientId, SignatureSchemeTag, ByteString)

type ClientRow =
  ( ClientId,
    ClientType,
    UTCTimeMillis,
    Maybe Text,
    Maybe ClientClass,
    Maybe CookieLabel,
    Maybe Text,
    Maybe (Vector Int32),
    Maybe UTCTime
  )

selectAllClients :: PrepQuery R () (UserId, ClientId, UTCTimeMillis, ClientType, Maybe Text, Maybe ClientClass, Maybe CookieLabel, Maybe Text, Maybe (Cassandra.Set ClientCapability), Maybe UTCTime)
selectAllClients =
  "SELECT user, client, tstamp, type, label, class, cookie, model, capabilities, last_active FROM clients"

selectAllPrekeys :: PrepQuery R () (UserId, ClientId, PrekeyId, Text)
selectAllPrekeys =
  "SELECT user, client, key, data FROM prekeys"

selectAllMLSPublicKeys :: PrepQuery R () (UserId, ClientId, SignatureSchemeTag, Blob)
selectAllMLSPublicKeys =
  "SELECT user, client, sig_scheme, key FROM mls_public_keys"

-- | Chain three full Cassandra scans into one source of copy rows.
migrateAllClients ::
  ( Member (Input Hasql.Pool) r,
    Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member (AtomicState Int) r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  ConduitM () Void (Sem r) ()
migrateAllClients migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllClients")
  withCount
    ( paginateSem selectAllClients (paramsP LocalQuorum () migOpts.pageSize) x5 .| C.map (fmap toClientCopyRow)
        >> paginateSem selectAllPrekeys (paramsP LocalQuorum () migOpts.pageSize) x5 .| C.map (fmap PrekeyCopyRow)
        >> paginateSem selectAllMLSPublicKeys (paramsP LocalQuorum () migOpts.pageSize) x5 .| C.map (fmap toMLSKeyCopyRow)
    )
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_
      ( traverse_ $ \row ->
          handleErrors
            (rowKeyByteString row)
            (migrateRow migOpts migCounter migDuration row)
      )
  where
    toClientCopyRow (u, c, ts, ty, lbl, cls, cok, mdl, caps, la) =
      ClientCopyRow
        ( u,
          ( c,
            ty,
            ts,
            lbl,
            cls,
            cok,
            mdl,
            fmap (V.fromList . map postgresMarshall . Cassandra.fromSet) caps,
            la
          )
        )

    toMLSKeyCopyRow (u, c, ss, Blob b) = MLSKeyCopyRow (u, c, ss, LBS.toStrict b)

rowKeyText :: CopyRow -> Text
rowKeyText = \case
  ClientCopyRow (u, (cid, _, _, _, _, _, _, _, _)) -> tshow u <> "/" <> clientToText cid
  PrekeyCopyRow (u, cid, k, _) -> tshow u <> "/" <> clientToText cid <> "/prekey/" <> tshow (postgresMarshall @Int32 k)
  MLSKeyCopyRow (u, cid, ss, _) -> tshow u <> "/" <> clientToText cid <> "/" <> postgresMarshall @Text ss

rowKeyByteString :: CopyRow -> ByteString
rowKeyByteString = toByteString' . rowKeyText

tshow :: (Show a) => a -> Text
tshow = T.pack . show

-- | Copy-only row upsert into Postgres; Cassandra rows are not deleted.
migrateRow ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  CopyRow ->
  Sem r ()
migrateRow migOpts migCounter migDuration row = do
  outcomeRef <- liftIO $ IORef.newIORef @Text "error"
  bracket
    (liftIO getCurrentTime)
    (observeDuration migDuration outcomeRef)
    ( const $ do
        timeoutResult <-
          Conc.timeout (migOpts.timeout <$ handleTimeout) migOpts.timeout $ do
            case row of
              ClientCopyRow (u, (cid, ty, ts, lbl, cls, cok, mdl, caps, la)) ->
                runStatement (u, cid, ty, ts, lbl, cls, cok, mdl, caps, la) upsertClientRowStatement
              PrekeyCopyRow (u, c, k, d) -> runStatement (u, c, k, d) upsertPrekeyRowStatement
              MLSKeyCopyRow (u, c, ss, pk) -> runStatement (u, c, ss, pk) insertMLSPublicKeyRowStatement
            liftIO $ Prometheus.incCounter migCounter
        case timeoutResult of
          Left timedOutAfter -> do
            markOutcome outcomeRef "timeout"
            liftIO . UnliftIO.throwIO $ MigrationTimedOut (rowKeyText row) timedOutAfter
          Right () -> markOutcome outcomeRef "success"
    )
  where
    handleTimeout =
      err $
        Log.msg (Log.val "client row migration timed out")
          . Log.field "row" (rowKeyText row)
          . Log.field "timeout" (show migOpts.timeout)

    markOutcome ref outcome = liftIO $ IORef.writeIORef ref outcome

    observeDuration metric outcomeRef start = do
      outcome <- liftIO $ IORef.readIORef outcomeRef
      end <- liftIO getCurrentTime
      liftIO $ Prometheus.withLabel metric outcome (`Prometheus.observe` realToFrac (diffUTCTime end start))

upsertClientRowStatement ::
  Hasql.Statement
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
    ()
upsertClientRowStatement =
  lmapPG
    @(UUID.UUID, Text, Int32, UTCTime, Maybe Text, Maybe Int32, Maybe Text, Maybe Text, Maybe (Vector Int32), Maybe UTCTime)
    @(UserId, ClientId, ClientType, UTCTimeMillis, Maybe Text, Maybe ClientClass, Maybe CookieLabel, Maybe Text, Maybe (Vector Int32), Maybe UTCTime)
    [resultlessStatement|INSERT INTO clients
                           (user_id, client_id, type, tstamp, label, class, cookie, model, capabilities, last_active)
                         VALUES
                           ($1 :: uuid, $2 :: text, $3 :: int, $4 :: timestamptz, $5 :: text?, $6 :: int?, $7 :: text?, $8 :: text?, $9 :: int[]?, $10 :: timestamptz?)
                         ON CONFLICT (user_id, client_id) DO UPDATE
                         SET type = ($3 :: int),
                             tstamp = ($4 :: timestamptz),
                             label = ($5 :: text?),
                             class = ($6 :: int?),
                             cookie = ($7 :: text?),
                             model = ($8 :: text?),
                             capabilities = ($9 :: int[]?),
                             last_active = ($10 :: timestamptz?)
       |]

upsertPrekeyRowStatement :: Hasql.Statement (UserId, ClientId, PrekeyId, Text) ()
upsertPrekeyRowStatement =
  lmapPG
    [resultlessStatement|INSERT INTO prekeys (user_id, client_id, key, data)
                         VALUES (($1 :: uuid), ($2 :: text), ($3 :: int), ($4 :: text))
                         ON CONFLICT (user_id, client_id, key) DO UPDATE
                         SET data = EXCLUDED.data
                        |]

insertMLSPublicKeyRowStatement ::
  Hasql.Statement (UserId, ClientId, SignatureSchemeTag, ByteString) ()
insertMLSPublicKeyRowStatement =
  lmapPG
    [resultlessStatement|INSERT INTO mls_public_keys (user_id, client_id, sig_scheme, key)
                         VALUES (($1 :: uuid), ($2 :: text), ($3 :: text), ($4 :: bytea))
                         ON CONFLICT (user_id, client_id, sig_scheme) DO NOTHING
                        |]
