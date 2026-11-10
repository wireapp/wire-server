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

module Wire.ProviderServiceStore.Migration
  ( migrateProviderServicesLoop,
    migrateProviderServiceTagsLoop,
    migrateProviderServicePrefixesLoop,
    migrateProviderServiceWhitelistLoop,
    migrateProviderServiceWhitelistRevLoop,
  )
where

import Cassandra hiding (Value)
import Cassandra qualified as C
import Data.ByteString.Conversion (toByteString')
import Data.Conduit
import Data.Conduit.List qualified as CL
import Data.IORef qualified as IORef
import Data.Id
import Data.Misc (Fingerprint, HttpsUrl, Rsa)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
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
import Wire.API.Provider.Service
import Wire.API.Provider.Service.Tag
import Wire.API.User.Profile (Asset, Name (..))
import Wire.Migration
import Wire.Postgres
import Wire.ProviderServiceStore.Cassandra qualified as Cassandra
import Wire.ProviderServiceStore.Postgres qualified as Postgres
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

-- | Row-table backfill: unlike the galley projection table (single
-- @writetime(base_url)@ timestamp), brig's provider-platform service writes are
-- field-granular — @DB.updateService@ and @DB.updateServiceConn@ each touch
-- different disjoint column subsets — so no single column's writetime is
-- written by every path. The row timestamp is therefore the /maximum/ of the
-- per-column writetimes.
--
-- The index and whitelist tables are PK-only (no writetime exists in
-- Cassandra), so their backfill is copy-only: deletes during the
-- backfill-to-cutover window can survive in Postgres (accepted trade-off, see
-- Wire.ProviderServiceStore.DualWrite).
migrateProviderServicesLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateProviderServicesLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "provider-services"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "provider-services")
    (migrateAllProviderServices migOpts migCounter migDuration)

migrateProviderServiceTagsLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateProviderServiceTagsLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "provider-service-tags"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "provider-service-tags")
    (migrateAllProviderServiceTags migOpts migCounter migDuration)

migrateProviderServicePrefixesLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateProviderServicePrefixesLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "provider-service-prefixes"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "provider-service-prefixes")
    (migrateAllProviderServicePrefixes migOpts migCounter migDuration)

migrateProviderServiceWhitelistLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateProviderServiceWhitelistLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "provider-service-whitelist"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "provider-service-whitelist")
    (migrateAllProviderServiceWhitelist migOpts migCounter migDuration)

migrateProviderServiceWhitelistRevLoop ::
  MigrationOptions ->
  ClientState ->
  Hasql.Pool ->
  Log.Logger ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  IO ()
migrateProviderServiceWhitelistRevLoop migOpts cassClient pgPool logger migCounter migFinished migFailed migDuration =
  migrationLoop
    logger
    "provider-service-whitelist-rev"
    migFinished
    migFailed
    (interpreter cassClient pgPool logger "provider-service-whitelist-rev")
    (migrateAllProviderServiceWhitelistRev migOpts migCounter migDuration)

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

--------------------------------------------------------------------------------
-- Row table

migrateAllProviderServices ::
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
migrateAllProviderServices migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllProviderServices")
  withCount (paginateSem Cassandra.selectAllProviderServices (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| CL.mapM_
      ( traverse_ $
          \row@(pid, sid, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) ->
            handleErrors (toByteString' pid <> toByteString' sid) (migrateProviderServiceRow migOpts migCounter migDuration row)
      )

migrateProviderServiceRow ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  ( ProviderId,
    ServiceId,
    Maybe Name,
    Maybe Text,
    Maybe Text,
    HttpsUrl,
    [ServiceToken],
    [ServiceKey],
    [Fingerprint Rsa],
    [Asset],
    C.Set ServiceTag,
    Bool,
    Int64,
    Maybe Int64,
    Maybe Int64,
    Int64,
    Int64
  ) ->
  Sem r ()
migrateProviderServiceRow migOpts migCounter migDuration (pid, sid, mbName, mbSummary, mbDescr, url, toks, keys, fps, assets, tags, ena, w1, w2, w3, w4, w5) =
  let name = fromMaybe (Name "") mbName
      summary = fromMaybe "" mbSummary
      descr = fromMaybe "" mbDescr
      rowTs =
        posixSecondsToUTCTime
          (fromIntegral (maximum (catMaybes [Just w1, w2, w3, Just w4, Just w5])) / 1000000)
   in copyRow
        migOpts
        migCounter
        migDuration
        "provider service migration timed out"
        (pid, sid)
        ( runStatement
            (pid, sid, name, summary, descr, url, toks, keys, fps, assets, tagSet tags, ena, rowTs)
            Postgres.upsertProviderServiceIfNewer
        )

tagSet :: C.Set ServiceTag -> Set.Set ServiceTag
tagSet = Set.fromList . fromSet

--------------------------------------------------------------------------------
-- Index tables (copy-only)

migrateAllProviderServiceTags ::
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
migrateAllProviderServiceTags migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllProviderServiceTags")
  withCount (paginateSem Cassandra.selectAllProviderServiceTags (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| CL.mapM_
      ( traverse_ $ \row@(_, _, _, sid, pid) ->
          handleErrors (toByteString' pid <> toByteString' sid) (copyProviderServiceTagRow migOpts migCounter migDuration row)
      )

copyProviderServiceTagRow ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  (Bucket, Int64, Name, ServiceId, ProviderId) ->
  Sem r ()
copyProviderServiceTagRow migOpts migCounter migDuration (bucket, tag, name, sid, pid) =
  copyRow migOpts migCounter migDuration "provider service companion table migration timed out" (pid, sid) $
    runStatement (bucket, tag, name, sid, pid) Postgres.insertProviderServiceTagRow

migrateAllProviderServicePrefixes ::
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
migrateAllProviderServicePrefixes migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllProviderServicePrefixes")
  withCount (paginateSem Cassandra.selectAllProviderServicePrefixes (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| CL.mapM_
      ( traverse_ $ \row@(_, _, sid, pid) ->
          handleErrors (toByteString' pid <> toByteString' sid) (copyProviderServicePrefixRow migOpts migCounter migDuration row)
      )

copyProviderServicePrefixRow ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  (Text, Name, ServiceId, ProviderId) ->
  Sem r ()
copyProviderServicePrefixRow migOpts migCounter migDuration (prefix, name, sid, pid) =
  copyRow migOpts migCounter migDuration "provider service companion table migration timed out" (pid, sid) $
    runStatement (prefix, name, sid, pid) Postgres.insertProviderServicePrefixRow

--------------------------------------------------------------------------------
-- Whitelist tables (copy-only)

migrateAllProviderServiceWhitelist ::
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
migrateAllProviderServiceWhitelist migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllProviderServiceWhitelist")
  withCount (paginateSem Cassandra.selectAllProviderServiceWhitelist (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| CL.mapM_
      ( traverse_ $ \row@(tid, pid, sid) ->
          handleErrors (toByteString' tid <> toByteString' pid <> toByteString' sid) (copyProviderServiceWhitelistRow migOpts migCounter migDuration row)
      )

copyProviderServiceWhitelistRow ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  (TeamId, ProviderId, ServiceId) ->
  Sem r ()
copyProviderServiceWhitelistRow migOpts migCounter migDuration (tid, pid, sid) =
  copyRow migOpts migCounter migDuration "provider service companion table migration timed out" (pid, sid) $
    runStatement (tid, pid, sid) Postgres.insertProviderServiceWhitelist

migrateAllProviderServiceWhitelistRev ::
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
migrateAllProviderServiceWhitelistRev migOpts migCounter migDuration = do
  lift $ info $ Log.msg (Log.val "migrateAllProviderServiceWhitelistRev")
  withCount (paginateSem Cassandra.selectAllProviderServiceWhitelistRev (paramsP LocalQuorum () migOpts.pageSize) x5)
    .| logRetrievedPage migOpts.pageSize id
    .| CL.mapM_
      ( traverse_ $ \row@(tid, pid, sid) ->
          handleErrors (toByteString' tid <> toByteString' pid <> toByteString' sid) (copyProviderServiceWhitelistRevRow migOpts migCounter migDuration row)
      )

copyProviderServiceWhitelistRevRow ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  (TeamId, ProviderId, ServiceId) ->
  Sem r ()
copyProviderServiceWhitelistRevRow migOpts migCounter migDuration (tid, pid, sid) =
  copyRow migOpts migCounter migDuration "provider service companion table migration timed out" (pid, sid) $
    runStatement (tid, pid, sid) Postgres.insertProviderServiceWhitelistRev

--------------------------------------------------------------------------------
-- Shared copy helper

copyRow ::
  ( PGConstraints r,
    Member TinyLog r,
    Member Resource r,
    Member Race r
  ) =>
  MigrationOptions ->
  Prometheus.Counter ->
  Prometheus.Vector Text Prometheus.Histogram ->
  -- | Timeout log message
  ByteString ->
  (ProviderId, ServiceId) ->
  Sem r () ->
  Sem r ()
copyRow migOpts migCounter migDuration timeoutMsg (pid, sid) action = do
  let keyText = T.pack (show (pid, sid))
  outcomeRef <- liftIO $ IORef.newIORef @Text "error"
  bracket
    (liftIO getCurrentTime)
    (observeDuration migDuration outcomeRef)
    ( const $ do
        timeoutResult <-
          Conc.timeout
            (migOpts.timeout <$ handleTimeout)
            migOpts.timeout
            action
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
        Log.msg (Log.val timeoutMsg)
          . Log.field "provider" (show pid)
          . Log.field "service" (show sid)
          . Log.field "timeout" (show migOpts.timeout)

    markOutcome ref outcome = liftIO $ IORef.writeIORef ref outcome

    observeDuration metric outcomeRef start = do
      outcome <- liftIO $ IORef.readIORef outcomeRef
      end <- liftIO getCurrentTime
      liftIO $ Prometheus.withLabel metric outcome (`Prometheus.observe` realToFrac (diffUTCTime end start))
