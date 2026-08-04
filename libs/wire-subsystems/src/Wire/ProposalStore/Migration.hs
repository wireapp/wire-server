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

module Wire.ProposalStore.Migration (migrateProposalsLoop) where

import Cassandra hiding (Value)
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.IORef qualified as IORef
import Data.Text qualified as T
import Data.Time
import Hasql.Pool.Extended qualified as Hasql
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.Conc (interpretRace)
import Polysemy.Conc qualified as Conc
import Polysemy.Conc.Effect.Race hiding (Timeout)
import Polysemy.Input
import Polysemy.Resource (Resource, bracket, resourceToIOFinal)
import Polysemy.State
import Polysemy.TinyLog
import Prometheus qualified
import System.Logger qualified as Log
import UnliftIO qualified
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation
import Wire.API.PostgresMarshall
import Wire.Migration
import Wire.Postgres
import Wire.ProposalStore.Cassandra qualified as Cql
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)

type EffectStack =
  [ State Int,
    Input ClientState,
    Input Hasql.Pool,
    Resource,
    Async,
    Race,
    TinyLog,
    Embed IO,
    Final IO
  ]

migrateProposalsLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateProposalsLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "mls proposal refs"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "mls proposal refs")
    (migrateAllProposals migOpts migCounter migDuration)

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
    . runState 0

migrateAllProposals ::
  ( Member (Input Hasql.Pool) r,
    Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member (State Int) r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  ConduitM () Void (Sem r) ()
migrateAllProposals migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllProposals")
  withCount (paginateSem Cql.selectAllProposals (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_ (traverse_ (\row@(groupId, _, _, _, _, _) -> handleErrors (unGroupId groupId) (migrateProposalRow migOpts migCounter migDuration row)))

migrateProposalRow ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  (GroupId, Epoch, ProposalRef, Maybe ProposalOrigin, Int32, RawMLS Proposal) ->
  Sem r ()
migrateProposalRow migOpts migCounter migDuration (groupId, epoch, ref, origin, ttl, proposal) =
  when (ttl > 0) $ do
    let keyText = T.pack (show groupId)
    outcomeRef <- liftIO $ IORef.newIORef @Text "error"
    bracket
      (liftIO getCurrentTime)
      (observeDuration migDuration outcomeRef)
      ( const $ do
          timeoutResult <-
            Conc.timeout
              (migOpts.timeout <$ handleTimeout)
              migOpts.timeout
              $ runStatement (groupId, epoch, ref, origin, proposal, ttl) upsert
          case timeoutResult of
            Left timedOutAfter -> do
              markOutcome outcomeRef "timeout"
              liftIO . UnliftIO.throwIO $ MigrationTimedOut keyText timedOutAfter
            Right () -> do
              markOutcome outcomeRef "success"
              liftIO $ Prometheus.incCounter migCounter
      )
  where
    upsert ::
      Hasql.Statement (GroupId, Epoch, ProposalRef, Maybe ProposalOrigin, RawMLS Proposal, Int32) ()
    upsert =
      lmapPG
        [resultlessStatement|
          INSERT INTO mls_proposal_refs (group_id, epoch, ref, origin, proposal, expires_at)
          VALUES ($1 :: bytea, $2 :: int8, $3 :: bytea, $4 :: int4?, $5 :: bytea, now () + make_interval(secs => $6 :: int))
          ON CONFLICT (group_id, epoch, ref) DO UPDATE
          SET origin = ($4 :: int4?),
              proposal = ($5 :: bytea),
              expires_at = now () + make_interval(secs => $6 :: int)
        |]

    handleTimeout =
      err $
        Log.msg (Log.val "mls proposal ref migration timed out")
          . Log.field "group_id" (show groupId)
          . Log.field "epoch" (show epoch)
          . Log.field "ref" (show ref)
          . Log.field "timeout" (show migOpts.timeout)

    markOutcome ref' outcome = liftIO $ IORef.writeIORef ref' outcome

    observeDuration metric outcomeRef start = do
      outcome <- liftIO $ IORef.readIORef outcomeRef
      end <- liftIO getCurrentTime
      liftIO $ Prometheus.withLabel metric outcome (`Prometheus.observe` realToFrac (diffUTCTime end start))
