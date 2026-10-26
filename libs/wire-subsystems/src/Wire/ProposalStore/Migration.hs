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

import Cassandra
import Data.ByteString.Conversion
import Data.Conduit
import Data.Conduit.List qualified as C
import Data.IORef qualified as IORef
import Data.Text qualified as T
import Data.Time
import Hasql.Pool.Extended qualified as Hasql
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
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation
import Wire.ConversationStore.Cassandra.Instances ()
import Wire.Migration
import Wire.Postgres
import Wire.ProposalStore.Postgres qualified as Postgres
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
    "proposal refs"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "proposal refs")
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
    . atomicStateToIO 0

selectAllProposals :: PrepQuery R () (GroupId, Epoch, ProposalRef, Maybe ProposalOrigin, RawMLS Proposal, Int32)
selectAllProposals = "select group_id, epoch, ref, origin, proposal, ttl(proposal) from mls_proposal_refs"

migrateAllProposals ::
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
migrateAllProposals migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllProposals")
  withCount (paginateSem selectAllProposals (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| C.mapM_ (traverse_ (\row@(_, _, ref, _, _, _) -> handleErrors (toByteString' (unProposalRef ref)) (migrateProposalRow migOpts migCounter migDuration row)))

migrateProposalRow ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  (GroupId, Epoch, ProposalRef, Maybe ProposalOrigin, RawMLS Proposal, Int32) ->
  Sem r ()
migrateProposalRow migOpts migCounter migDuration (gid, epoch, ref, origin, proposal, ttl) =
  when (ttl > 0) $ do
    let keyText = T.pack (show (unProposalRef ref))
    outcomeRef <- liftIO $ IORef.newIORef @Text "error"
    bracket
      (liftIO getCurrentTime)
      (observeDuration migDuration outcomeRef)
      ( const $ do
          timeoutResult <- Conc.timeout (migOpts.timeout <$ handleTimeout) migOpts.timeout $ Postgres.upsertProposal gid epoch ref origin proposal ttl
          case timeoutResult of
            Left timedOutAfter -> do
              markOutcome outcomeRef "timeout"
              liftIO . UnliftIO.throwIO $ MigrationTimedOut keyText timedOutAfter
            Right () -> do
              markOutcome outcomeRef "success"
              liftIO $ Prometheus.incCounter migCounter
      )
  where
    handleTimeout =
      err $
        Log.msg (Log.val "proposal migration timed out")
          . Log.field "ref" (show (unProposalRef ref))
          . Log.field "timeout" (show migOpts.timeout)

    markOutcome outcomeRef' outcome = liftIO $ IORef.writeIORef outcomeRef' outcome

    observeDuration metric outcomeRef start = do
      outcome <- liftIO $ IORef.readIORef outcomeRef
      end <- liftIO getCurrentTime
      liftIO $ Prometheus.withLabel metric outcome (`Prometheus.observe` realToFrac (diffUTCTime end start))
