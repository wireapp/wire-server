-- | Experiment B: writers as in "FanInPerf.Produce" plus simulated reader
-- clients that fetch the streams they subscribed to and keep their cursors in
-- memory (no acknowledgements).
module FanInPerf.Consume
  ( Sub (..),
    loadCursors,
    readerPass,
    lagNs,
    runConsume,
  )
where

import Control.Concurrent.Async
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
import Hasql.Pool (UsageError)
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
-- of failed fetches. A failed fetch leaves its cursor unchanged.
readerPass ::
  forall e r.
  (Show e, Members '[FanInNotificationsStore, PE.Error e, Embed IO] r) =>
  Int ->
  ReaderStats ->
  [Sub] ->
  Sem r ([Sub], Bool, [Text])
readerPass limit stats subs = do
  results <- for subs $ \sub -> do
    t0 <- embed getMonotonicTimeNSec
    fetched <- PE.try @e (fetchNotifications sub.stream sub.cursor limit)
    t1 <- embed getMonotonicTimeNSec
    case fetched of
      Left err -> do
        embed (recordFetchError stats sub.kind)
        pure (sub, False, [kindName sub.kind <> ": fetch failed: " <> T.pack (show err)])
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

-- | One reader client: fetch all streams, then sleep unless a full page says
-- more is waiting. Failures are reported and slow the loop down, never stop it.
readerLoop :: Env -> ConsumeOptions -> ReaderStats -> (Text -> IO ()) -> [Sub] -> IO ()
readerLoop env opts stats reportError = go
  where
    pause = threadDelay (opts.pollIntervalMs * 1000)
    failed msg subs = reportError ("reader: " <> msg) >> pause >> go subs
    go subs =
      -- synchronous exceptions are caught like in 'writerStep'; async ones
      -- (cancel) propagate
      tryAny (runStore env (readerPass @UsageError opts.fetchLimit stats subs)) >>= \case
        Left e -> failed (T.pack (displayException e)) subs
        Right (Left e) -> failed (T.pack (show e)) subs
        Right (Right (subs', more, errs)) -> do
          traverse_ reportError errs
          unless more pause
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
  clientSubs <- either (\e -> die ("loading start positions failed: " <> show e)) pure (sequence loaded)

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
      -- True when readers caught up before the timeout
      drain = do
        deadline <- (+ fromIntegral opts.drainTimeout) <$> getMonotonicTime
        let go = do
              (expected, delivered) <- totals
              now <- getMonotonicTime
              if delivered >= expected
                then pure True
                else
                  if now >= deadline
                    then pure False
                    else threadDelay 100_000 >> go
        go
  start <- getMonotonicTime
  stateRef <- newIORef (initialTickState start, initialConsumeTickState start)
  let warmup = fromIntegral p.warmup
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
  drained <-
    withAsync (forever (threadDelay 1_000_000 >> void doTick)) $ \tickerA -> do
      link tickerA
      withAsync (mapConcurrently_ readerClient (zip readerStats clientSubs)) $ \readersA -> do
        link readersA
        withAsync (mapConcurrently_ writer (zip writerStats writerGens)) $ \writersA -> do
          link writersA
          waitForStop p.duration
          atomicWriteIORef stopWriters True
          wait writersA
          printLine console "consume: writers stopped, draining"
          drain
  -- ticker and readers are cancelled now; the final tick and summary run here only
  (writerReport, readerReport) <- doTick
  traverse_ (printLine console) (formatConsumeSummary (totalExpected writerReport.total) drained writerReport readerReport)
  where
    tshow :: (Show a) => a -> Text
    tshow = T.pack . show
