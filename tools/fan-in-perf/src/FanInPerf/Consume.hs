-- | Experiment B: writers as in "FanInPerf.Produce" plus simulated reader
-- clients that fetch the streams they subscribed to and keep their cursors in
-- memory (no acknowledgements).
module FanInPerf.Consume
  ( Sub (..),
    loadCursors,
    readerPass,
    readerRound,
    describeUsageError,
    lagNs,
    runConsume,
  )
where

import Control.Concurrent.Async
import Control.Exception (AsyncException (UserInterrupt), handleJust)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID.Types (UUID)
import Data.UUID.Types qualified as UUID
import FanInPerf.ConsumeStats
import FanInPerf.Metrics
import FanInPerf.Options (ConsumeOptions (..), ProduceOptions (..))
import FanInPerf.Population
import FanInPerf.Produce
import FanInPerf.Stats
import FanInPerf.Store
import FanInPerf.Targets
import FanInPerf.Terminal
import GHC.Clock (getMonotonicTime, getMonotonicTimeNSec)
import Hasql.Errors (ConnectionError (..), ServerError (..), SessionError (..), StatementError (..))
import Hasql.Pool (UsageError (..))
import Imports
import Polysemy
import Polysemy.Error qualified as PE
import System.Exit (die)
import System.Random
import UnliftIO.Exception (tryAny)
import Wire.FanInNotificationsStore
import Wire.FanInNotificationsStore.Postgres (notificationIdMillis)

-- | A stream a client reads and its cursor: the id of the last notification seen.
data Sub = Sub {kind :: !TargetKind, stream :: !Stream, cursor :: !UUID}

-- | Start position of every stream of a client: the latest notification id
-- ("start at latest"), the nil UUID for an empty stream.
loadCursors :: (Member FanInNotificationsStore r) => Client -> Sem r [Sub]
loadCursors c =
  for c.streams $ \s -> do
    lastId <- getLastNotificationId s
    pure Sub {kind = streamKind s, stream = s, cursor = fromMaybe UUID.nil lastId}

-- | Fetches every stream of one client once. Returns the advanced cursors,
-- whether some stream returned a full page (more is waiting) and the messages
-- of failed fetches (rendered by @render@). A failed fetch leaves its cursor
-- unchanged.
readerPass ::
  forall e r.
  (Members '[FanInNotificationsStore, PE.Error e, Embed IO] r) =>
  (e -> Text) ->
  Int ->
  ReaderStats ->
  [Sub] ->
  Sem r ([Sub], Bool, [Text])
readerPass render limit stats subs = do
  results <- for subs $ \sub -> do
    t0 <- embed getMonotonicTimeNSec
    fetched <- PE.try @e (fetchNotifications sub.stream sub.cursor limit)
    t1 <- embed getMonotonicTimeNSec
    case fetched of
      Left err -> do
        embed (recordFetchError stats sub.kind)
        pure (sub, False, [kindName sub.kind <> ": fetch failed: " <> render err])
      Right page -> do
        nowMs <- embed currentMillis
        embed (recordFetch stats sub.kind (t1 - t0) (map (lagNs nowMs . fst) page))
        let cursor' = maybe sub.cursor fst (listToMaybe (reverse page))
        pure (sub {cursor = cursor'}, length page >= limit, [])
  pure ([s | (s, _, _) <- results], or [full | (_, full, _) <- results], concat [errs | (_, _, errs) <- results])

currentMillis :: IO Word64
currentMillis = floor . (* 1000) <$> getPOSIXTime

-- | @lagNs nowMs notificationId@: time since the notification was created (ms
-- timestamp of its UUIDv7) in ns. Clamped to 0 when the id lies in the future
-- (clock skew between hosts): compared before subtracting, so 'Word64' cannot
-- wrap around into the top histogram bucket.
lagNs :: Word64 -> UUID -> Word64
lagNs nowMs notifId =
  let created = notificationIdMillis notifId
   in if nowMs > created then (nowMs - created) * 1_000_000 else 0

-- | Describes a store error without SQL text, parameters or server messages
-- (they can contain stream ids and cursors): constructor names and the
-- SQLSTATE only.
describeUsageError :: UsageError -> Text
describeUsageError = \case
  AcquisitionTimeoutUsageError -> "AcquisitionTimeoutUsageError"
  ConnectionError e -> "ConnectionError " <> connectionError e
  SessionError e -> "SessionError " <> sessionError e
  where
    connectionError = \case
      NetworkingConnectionError _ -> "NetworkingConnectionError"
      AuthenticationConnectionError _ -> "AuthenticationConnectionError"
      CompatibilityConnectionError _ -> "CompatibilityConnectionError"
      OtherConnectionError _ -> "OtherConnectionError"
    sessionError = \case
      StatementSessionError _ _ _ _ _ e -> "StatementSessionError " <> statementError e
      ScriptSessionError _ e -> "ScriptSessionError " <> serverError e
      ConnectionSessionError _ -> "ConnectionSessionError"
      MissingTypesSessionError _ -> "MissingTypesSessionError"
      DriverSessionError _ -> "DriverSessionError"
    statementError = \case
      ServerStatementError e -> "ServerStatementError " <> serverError e
      UnexpectedRowCountStatementError {} -> "UnexpectedRowCountStatementError"
      UnexpectedColumnCountStatementError {} -> "UnexpectedColumnCountStatementError"
      UnexpectedColumnTypeStatementError {} -> "UnexpectedColumnTypeStatementError"
      RowStatementError {} -> "RowStatementError"
      UnexpectedResultStatementError {} -> "UnexpectedResultStatementError"
    serverError (ServerError code _ _ _ _) = "SQLSTATE " <> code

-- | One pass over the streams of a client, each fetched in isolation by
-- @fetchOne@ (a 'readerPass' over that stream alone): an exception or error
-- only keeps the cursor of its own stream (counted as fetch error), so cursors
-- advanced by earlier fetches survive. Synchronous exceptions are caught like
-- in 'writerStep'; async ones (cancel) propagate.
readerRound ::
  (e -> Text) ->
  ReaderStats ->
  (Sub -> IO (Either e ([Sub], Bool, [Text]))) ->
  [Sub] ->
  IO ([Sub], Bool, [Text])
readerRound render stats fetchOne subs = do
  results <- for subs $ \sub ->
    tryAny (fetchOne sub) >>= \case
      Left e -> failed sub (T.pack (displayException e))
      Right (Left e) -> failed sub (render e)
      Right (Right r) -> pure r
  pure (concat [s | (s, _, _) <- results], or [full | (_, full, _) <- results], concat [errs | (_, _, errs) <- results])
  where
    failed sub msg = do
      recordFetchError stats sub.kind
      pure ([sub], False, ["reader: " <> kindName sub.kind <> ": " <> msg])

-- | One reader client: fetch all streams, then sleep unless a full page says
-- more is waiting. Failures are reported and slow the loop down, never stop it.
readerLoop :: Env -> ConsumeOptions -> ReaderStats -> (Text -> IO ()) -> [Sub] -> IO ()
readerLoop env opts stats reportError = go
  where
    fetchOne sub = runStore env (readerPass @UsageError describeUsageError opts.fetchLimit stats [sub])
    go subs = do
      (subs', more, errs) <- readerRound describeUsageError stats fetchOne subs
      traverse_ reportError errs
      unless more (threadDelay (opts.pollIntervalMs * 1000))
      go subs'

-- | @runConsume console writerEnv readerEnv options@. The environments differ
-- only in their connection pool.
runConsume :: Console -> Env -> Env -> ConsumeOptions -> IO ()
runConsume console writerEnv readerEnv opts = do
  let p = opts.writerOptions
  seed <- maybe randomIO pure p.seed
  let (entries, gen') = mkEntries p.clientsPerUser p.targets (mkStdGen seed)
      payload = mkPayload p.payloadBytes
      writerGens = take p.writers (unfoldr (Just . split) gen')
  population <- either (die . T.unpack) pure (mkPopulation opts.clients opts.epochsPerClient entries)
  printLine console $
    "consume: writers="
      <> tshow p.writers
      <> " clients="
      <> tshow opts.clients
      <> " targets="
      <> renderTargetSpec p.targets
      <> " seed="
      <> tshow seed
  traverse_ (printLine console) (formatCoverage (coverage population.subscribers entries))
  -- Start positions are read before any writer runs: a push committed in
  -- between could otherwise sit below a cursor and show up as a false `missing`.
  printLine console "consume: loading start positions"
  loaded <- forConcurrently population.simClients (runStore readerEnv . loadCursors)
  clientSubs <- either (\e -> die ("loading start positions failed: " <> T.unpack (describeUsageError e))) pure (sequence loaded)

  metrics <- newMetrics "consume" p.writers
  consumeMetrics <- newConsumeMetrics "consume" opts.clients
  writerStats <- replicateM p.writers newWriterStats
  readerStats <- replicateM opts.clients newReaderStats
  errors <- newTBQueueIO 1000
  stopWriters <- newIORef False
  let -- error path only; drops messages when the ticker falls behind
      reportError msg = atomically $ do
        full <- isFullTBQueue errors
        unless full (writeTBQueue errors msg)
      -- A writer stops between pushes, so every committed push has its
      -- expected deliveries recorded before the drain starts.
      writer (ws, g0) =
        let doPush push = do
              outcome <- storePush writerEnv push
              case outcome of
                PushOk -> recordExpected ws (expectedDeliveries population.subscribers push)
                PushFailed _ -> pure ()
              pure outcome
            loop !g = do
              stop <- readIORef stopWriters
              unless stop (writerStep doPush payload entries ws reportError g >>= loop)
         in loop g0
      readerClient (stats, subs) = readerLoop readerEnv opts stats reportError subs
      totals = do
        expected <- totalExpected . sumSnapshots <$> traverse readSnapshot writerStats
        delivered <- totalDelivered . sumReaderSnapshots <$> traverse readReaderSnapshot readerStats
        pure (expected, delivered)
      -- waits until readers caught up or the timeout passed
      drain = do
        started <- getMonotonicTime
        let deadline = started + fromIntegral opts.drainTimeout
            go = do
              (expected, delivered) <- totals
              now <- getMonotonicTime
              if delivered >= expected
                then pure (DrainComplete (now - started))
                else
                  if now >= deadline
                    then pure (DrainTimedOut (now - started))
                    else threadDelay 100_000 >> go
        go
  start <- getMonotonicTime
  stateRef <- newIORef (initialTickState start, initialConsumeTickState start)
  let warmup = fromIntegral p.warmup
      -- the writer phase ends when the writers stopped (or at Ctrl-C)
      writerPhaseEnd = do
        now <- getMonotonicTime
        (_, delivered) <- totals
        pure WriterPhase {duration = now - start, delivered}
      doTick = do
        now <- getMonotonicTime
        writerTotal <- sumSnapshots <$> traverse readSnapshot writerStats
        readerTotal <- sumReaderSnapshots <$> traverse readReaderSnapshot readerStats
        (writerState, readerState) <- readIORef stateRef
        let (writerReport, writerState') = tick warmup now writerTotal writerState
            (readerReport, readerState') = consumeTick warmup now (totalExpected writerTotal) readerTotal readerState
        writeIORef stateRef (writerState', readerState')
        printQueuedErrors console errors
        publish metrics writerReport
        publishConsume consumeMetrics readerReport
        drawStatus console (formatConsumeStatus writerReport readerReport)
        pure (writerReport, readerReport)
  printLine console "consume: running"
  (writerPhase, drainResult) <-
    withAsync (forever (threadDelay 1_000_000 >> void doTick)) $ \tickerA -> do
      link tickerA
      withAsync (mapConcurrently_ readerClient (zip readerStats clientSubs)) $ \readersA -> do
        link readersA
        withAsync (mapConcurrently_ writer (zip writerStats writerGens)) $ \writersA -> do
          link writersA
          waitForStop p.duration
          atomicWriteIORef stopWriters True
          -- The first Ctrl-C while waiting for writers or draining still
          -- prints the summary (a second one kills the process: GHC resets
          -- the SIGINT handler). 'UserInterrupt' is async, so UnliftIO's
          -- handlers miss it.
          phaseRef <- newIORef Nothing
          let interrupted = do
                phase <- maybe writerPhaseEnd pure =<< readIORef phaseRef
                pure (phase, DrainInterrupted)
          handleJust isInterrupt (const interrupted) $ do
            wait writersA
            phase <- writerPhaseEnd
            writeIORef phaseRef (Just phase)
            printLine console "consume: writers stopped, draining"
            (phase,) <$> drain
  -- ticker, readers and writers are cancelled now; the final tick and summary run here only
  (writerReport, readerReport) <- doTick
  traverse_ (printLine console) (formatConsumeSummary (totalExpected writerReport.total) writerPhase drainResult writerReport readerReport)
  where
    tshow :: (Show a) => a -> Text
    tshow = T.pack . show
    isInterrupt UserInterrupt = Just ()
    isInterrupt _ = Nothing
