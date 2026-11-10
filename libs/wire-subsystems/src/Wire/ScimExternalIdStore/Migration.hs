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

module Wire.ScimExternalIdStore.Migration (migrateScimExternalIdsLoop) where

import Cassandra hiding (Value)
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.Text.Encoding (encodeUtf8)
import Data.UUID (UUID)
import Hasql.Pool.Extended qualified as HasqlPoolExt
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.AtomicState
import Polysemy.Conc (interpretRace)
import Polysemy.Conc.Effect.Race hiding (Timeout)
import Polysemy.Error (runError)
import Polysemy.Input
import Polysemy.Resource (Resource, resourceToIOFinal)
import Polysemy.TinyLog
import Prometheus qualified
import System.Logger qualified as Log
import Wire.API.PostgresMarshall
import Wire.Migration
import Wire.MigrationLock (MigrationLockError, ScimExternalIdLockKey (..))
import Wire.Postgres
import Wire.ScimExternalIdStore.Postgres qualified as Postgres
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)
import Wire.Util (embedClient)

type EffectStack =
  [ AtomicState Int,
    Input ClientState,
    Input HasqlPoolExt.Pool,
    Resource,
    Async,
    Race,
    TinyLog,
    Embed IO,
    Final IO
  ]

migrateScimExternalIdsLoop ::
  MigrationOptions ->
  ClientState ->
  HasqlPoolExt.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateScimExternalIdsLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "scim external ids"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "scim external ids")
    (migrateAllScimExternalIds migOpts migCounter migDuration)

interpreter :: ClientState -> HasqlPoolExt.Pool -> Log.Logger -> ByteString -> Sem EffectStack a -> IO (Int, a)
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

selectAllScimExternalIds :: PrepQuery R () (UUID, Text, UUID, Maybe Int32)
selectAllScimExternalIds =
  "SELECT team, external_id, \"user\", creation_status FROM scim_external"

migrateAllScimExternalIds ::
  ( Member (Input HasqlPoolExt.Pool) r,
    Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member (AtomicState Int) r,
    Member Resource r,
    Member Race r,
    Member Async r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  ConduitM () Void (Sem r) ()
migrateAllScimExternalIds migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllScimExternalIds")
  withCount (paginateSem selectAllScimExternalIds (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_ (traverse_ (\row@(_, eid, _, _) -> handleErrors (encodeUtf8 eid) (migrateScimExternalIdRow migOpts migCounter migDuration row)))

migrateScimExternalIdRow ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Resource r,
    Member Race r,
    Member Async r,
    Member (Input ClientState) r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  (UUID, Text, UUID, Maybe Int32) ->
  Sem r ()
migrateScimExternalIdRow migOpts migCounter migDuration (team, eid, _uid, _status) =
  -- Rows that fail to decode indicate corruption; skip them rather than fail
  -- the migration (same convention as the Postgres read path).
  for_ (hush (postgresUnmarshall team)) $ \team' ->
    -- Copy under the exclusive (team, external-id) migration lock and re-read
    -- the row from Cassandra inside the locked section: the scanned page can
    -- be arbitrarily stale, so a mapping deleted by a concurrent dual-write
    -- 'Delete' must not be resurrected in Postgres, and a newer creation
    -- status (or user re-assignment) must not be overwritten by the stale
    -- values. Dual-write mutations take the shared lock on the same key
    -- ('Wire.ScimExternalIdStore.DualWrite'), so they are serialized against
    -- this copy.
    void . runError @MigrationLockError $
      withExclusiveMigrationLockAndTimeout migOpts.timeout migDuration [ScimExternalIdLockKey (team', eid)] $ do
        client <- input @ClientState
        freshRows <- embedClient client $ retry x1 (query selectFreshScimExternalId (params LocalQuorum (team, eid)))
        for_ (listToMaybe freshRows) $ \(freshUser, freshStatus) ->
          for_
            ( (,) <$> hush (postgresUnmarshall freshUser)
                <*> hush (traverse postgresUnmarshall freshStatus)
            )
            $ \(uid'', status'') -> do
              Postgres.upsertScimExternalIdRow team' eid uid'' status''
              liftIO $ Prometheus.incCounter migCounter

-- | Fresh point-read of one scim_external row, executed inside the exclusive
-- migration lock.
selectFreshScimExternalId :: PrepQuery R (UUID, Text) (UUID, Maybe Int32)
selectFreshScimExternalId =
  "SELECT \"user\", creation_status FROM scim_external WHERE team = ? AND external_id = ?"

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just
