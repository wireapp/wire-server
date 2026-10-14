-- | Experiment B: writers as in "FanInPerf.Produce" plus simulated reader
-- clients that fetch the streams they subscribed to and keep their cursors in
-- memory (no acknowledgements).
module FanInPerf.Consume
  ( Subscription (..),
    newSubscription,
    millisLowerBound,
    loadCursors,
    readerPass,
    readerRound,
    lagNs,
    runConsume,
  )
where

import Control.Concurrent.Async
import Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import Control.Exception (AsyncException (UserInterrupt), handleJust)
import Data.Bits (shiftL)
import Data.Set qualified as Set
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
import UnliftIO.Exception (bracket_, tryAny)
import Wire.FanInNotificationsStore
import Wire.FanInNotificationsStore.Postgres (notificationIdMillis)

-- | A stream a client reads. @cursor@ is the highest id delivered (or the
-- start position). With a safety window, @start@ (the start position) is the
-- lowest possible fetch bound, @seen@ holds the delivered ids inside the
-- window and @resumeAfter@ is where an unfinished scan of the window continues.
data Subscription = Subscription
  { kind :: !TargetKind,
    stream :: !Stream,
    cursor :: !UUID,
    start :: !UUID,
    seen :: !(Set UUID),
    resumeAfter :: !(Maybe UUID)
  }

newSubscription :: TargetKind -> Stream -> UUID -> Subscription
newSubscription kind stream cursor =
  Subscription {start = cursor, seen = mempty, resumeAfter = Nothing, ..}

-- | Start position of every stream of a client: the latest notification id
-- ("start at latest"), the nil UUID for an empty stream.
loadCursors :: (Member FanInNotificationsStore r) => Client -> Sem r [Subscription]
loadCursors c =
  for c.streams $ \s -> do
    lastId <- getLastNotificationId s
    pure $ newSubscription (streamKind s) s (fromMaybe UUID.nil lastId)

-- | The smallest UUID with the given UUIDv7 ms timestamp (all other bits
-- zero): every UUIDv7 of that ms or later is greater (its version bits are
-- set), every one of an earlier ms is smaller.
millisLowerBound :: Word64 -> UUID
millisLowerBound ms =
  UUID.fromWords64 (ms `shiftL` 16) 0

-- | Ids up to the bound are never fetched again. Window 0: the cursor itself
-- (the plain @> lastId@ reader). Otherwise the ms of the cursor minus the
-- window, never below the start position. Monotone, as the cursor only grows.
lowerBound :: Word64 -> Subscription -> UUID
lowerBound 0 sub = sub.cursor
lowerBound windowMs sub =
  let ms = notificationIdMillis sub.cursor
   in max sub.start (millisLowerBound (if ms > windowMs then ms - windowMs else 0))

-- | Fetches every stream of one client once. Returns the advanced cursors,
-- whether some stream returned a full page (more is waiting) and the messages
-- of failed fetches (rendered by @render@). A failed fetch leaves its cursor
-- unchanged.
--
-- With a safety window (@windowMs@ > 0) a fetch starts at 'lowerBound', so
-- notifications committed late (lower id than the cursor) within the window
-- are still delivered; ids in @seen@ are not delivered again. Termination: the
-- window can hold more already-seen rows than the fetch limit. A full raw page
-- sets @resumeAfter@ to its last id and the next pass continues after it (each such
-- pass advances strictly, rows are finite); a non-full page ends the scan
-- (@resumeAfter = Nothing@), so the next pass restarts at the bound and picks up late
-- commits. @more@ is "the raw page was full", so a reader never spins on the
-- same page. With window 0 all this reduces to the plain @> cursor@ reader.
readerPass ::
  forall e r.
  (Members '[FanInNotificationsStore, PE.Error e, Embed IO] r) =>
  (e -> Text) ->
  Int ->
  Word64 ->
  ReaderStats ->
  [Subscription] ->
  Sem r ([Subscription], Bool, [Text])
readerPass render limit windowMs stats subs = do
  results <- for subs $ \sub -> do
    let bound = lowerBound windowMs sub
        from = maybe bound (max bound) sub.resumeAfter
    t0 <- embed getMonotonicTimeNSec
    fetched <- PE.try @e (fetchNotifications sub.stream from limit)
    t1 <- embed getMonotonicTimeNSec
    case fetched of
      Left err -> do
        embed (recordFetchError stats sub.kind)
        pure (sub, False, [kindName sub.kind <> ": fetch failed: " <> render err])
      Right page -> do
        let new = filter (`Set.notMember` sub.seen) (map fst page)
            full = length page >= limit
            cursor' = foldl' max sub.cursor new
            sub' = sub {cursor = cursor', resumeAfter = if full then fst <$> listToMaybe (reverse page) else Nothing}
            bound' = lowerBound windowMs sub'
            seen'
              | windowMs == 0 = mempty
              | otherwise = Set.filter (> bound') (foldl' (flip Set.insert) sub.seen new)
        nowMs <- embed currentMillis
        embed (recordFetch stats sub.kind (t1 - t0) (map (lagNs nowMs) new))
        pure (sub' {seen = seen'}, full, [])
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

-- | One pass over the streams of a client, each fetched in isolation by
-- @fetchOne@ (a 'readerPass' over that stream alone): an exception or error
-- only keeps the cursor of its own stream (counted as fetch error), so the
-- cursors of the other streams survive. Synchronous exceptions are caught like
-- in 'writerStep'; async ones (cancel) propagate.
readerRound ::
  (e -> Text) ->
  ReaderStats ->
  (Subscription -> IO (Either e ([Subscription], Bool, [Text]))) ->
  [Subscription] ->
  IO ([Subscription], Bool, [Text])
readerRound render stats fetchOne subs = do
  results <- for subs $ \sub ->
    tryAny (fetchOne sub) >>= \case
      Left e -> failed sub (T.pack (displayException e))
      Right (Left e) -> failed sub (render e)
      Right (Right r) -> pure r
  pure
    ( concat [s | (s, _, _) <- results],
      or [full | (_, full, _) <- results],
      concat [errs | (_, _, errs) <- results]
    )
  where
    failed sub msg = do
      recordFetchError stats sub.kind
      pure ([sub], False, ["reader: " <> kindName sub.kind <> ": " <> msg])

-- | Lets at most as many readers as the pool has connections wait for it.
-- Thousands of readers blocked in the pool's STM retry are all woken on every
-- connection release; that thundering herd saturated the CPU and delayed the
-- ticker and drain by tens of seconds. A 'QSem' wakes one waiter at a time.
throughGate :: QSem -> IO a -> IO a
throughGate gate = bracket_ (waitQSem gate) (signalQSem gate)

-- | One reader client: fetch all streams, then sleep unless a full page says
-- more is waiting. Failures are reported and slow the loop down, never stop it.
readerLoop :: QSem -> Env -> ConsumeOptions -> ReaderStats -> (Text -> IO ()) -> [Subscription] -> IO ()
readerLoop gate env opts stats reportError = go
  where
    render :: UsageError -> Text
    render = T.pack . show
    fetchOne sub =
      throughGate gate $
        runStore env (readerPass @UsageError render opts.fetchLimit (fromIntegral opts.safetyWindowMs) stats [sub])
    go subs = do
      (subs', more, errs) <- readerRound render stats fetchOne subs
      traverse_ reportError errs
      unless more (threadDelay (opts.pollIntervalMs * 1000))
      go subs'

-- | @runConsume console writerEnv readerEnv options@. The environments differ
-- only in their connection pool.
runConsume :: Console -> Env -> Env -> ConsumeOptions -> IO ()
runConsume console writerEnv readerEnv opts = do
  gate <- newQSem opts.readerPoolSize
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
  loaded <- forConcurrently population.simClients (throughGate gate . runStore readerEnv . loadCursors)
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
      readerClient (stats, subs) = readerLoop gate readerEnv opts stats reportError subs
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
