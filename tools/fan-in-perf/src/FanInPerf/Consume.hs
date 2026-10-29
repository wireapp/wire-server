-- | Experiments B and C: writers as in "FanInPerf.Produce" plus simulated
-- reader clients that fetch the streams they subscribed to and keep their
-- cursors in memory; in 'Acks' mode they also acknowledge (see 'acknowledge').
-- Experiments E and F ('runConsumeOnly') run the readers alone.
module FanInPerf.Consume
  ( Subscription (..),
    newSubscription,
    millisLowerBound,
    PassResult (..),
    startPositions,
    acksResetLine,
    readerPass,
    readerRound,
    throughGate,
    isolated,
    ackOne,
    ackPending,
    acknowledge,
    lagNs,
    runConsume,
    runConsumeOnly,
    reachMilestones,
  )
where

import Control.Concurrent.Async
import Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import Control.Exception (evaluate, handleJust)
import Data.Bits (shiftL)
import Data.Map.Strict qualified as Map
import Data.Misc (showT)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID.Types (UUID)
import Data.UUID.Types qualified as UUID
import Data.Vector qualified as V
import FanInPerf.Acks
import FanInPerf.ConsumeStats
import FanInPerf.GcImpact (DropEvent, detectDrops, gcImpact, sampleOf)
import FanInPerf.Metrics
import FanInPerf.NotificationsAdmin (countStreams, oldestPartitions)
import FanInPerf.NotificationsStore
import FanInPerf.NotificationsStore.Postgres (notificationIdMillis)
import FanInPerf.Options (ConsumeOnlyOptions (..), ConsumeOptions (..), PopulationOptions (..), ProduceOptions (..), ReaderMode (..), ReaderOptions (..), consumeOnlyLabel, experimentLabel, readerOptions)
import FanInPerf.Population
import FanInPerf.Population qualified as Pop
import FanInPerf.Produce
import FanInPerf.Stats
import FanInPerf.Store
import FanInPerf.Summary (RunSummary (gc, rowsTotal), consumeOnlySummary, consumeSummary)
import FanInPerf.Targets
import FanInPerf.Terminal
import FanInPerf.Worker
import GHC.Clock (getMonotonicTime, getMonotonicTimeNSec)
import Hasql.Pool (UsageError)
import Imports
import Polysemy
import Polysemy.Error qualified as PE
import System.Exit (die)
import System.Random (StdGen)
import System.Timeout qualified
import UnliftIO.Exception (bracket_, tryAny)

-- | A stream a client reads. Ids ascend with their UUIDv7 time, left to right.
-- With @--safety-window@ W > 0 the next fetch returns ids greater than
-- @max bound resumeAfter@, where @bound@ is the cursor minus W ms, never below
-- @start@ ('lowerBound'):
--
-- @
--  start        bound         resumeAfter           cursor        newest
--    |            |                |                   |             |
--  --+--x-x-x-----+--o-x-o-x-x-----+-o-x-x-o-x-x-------+-------------+-->
--    |            |<------------- window W ----------->|
--    |            |  seen = ids delivered in here      |
--    |<- never fetched again                           |<- not yet delivered
--
--  x  delivered, in @seen@        o  late commit: below cursor, delivered
--     (skipped when fetched again)    on the re-read of the window
-- @
--
-- W = 0: @bound = cursor@, @seen@ is empty, plain @> lastId@.
data Subscription = Subscription
  { kind :: !TargetKind,
    stream :: !Stream,
    -- | highest id delivered (initially the `start` position)
    cursor :: !UUID,
    -- | start position: the lowest possible fetch bound
    start :: !UUID,
    -- | delivered ids inside the window, not delivered again
    seenInWindow :: !(Set UUID),
    -- | last id of a full page: the next fetch continues after it instead of
    -- re-reading the page (the window holds more rows than @--fetch-limit@);
    -- a non-full page resets it
    resumeAfter :: !(Maybe UUID)
  }

newSubscription :: TargetKind -> Stream -> UUID -> Subscription
newSubscription kind stream cursor =
  Subscription {start = cursor, seenInWindow = mempty, resumeAfter = Nothing, ..}

-- | Start position of every stream of a reader: B reads the latest
-- notification id ("start at latest"); C resets the reader's acks to it (RFC
-- "when nothing is acknowledged"). The nil UUID for an empty stream.
startPositions ::
  (Member FanInNotificationsStore r) =>
  ReaderMode ->
  Set TargetKind ->
  Pop.Reader ->
  Sem r [Subscription]
startPositions mode kinds r =
  for (readerStreams kinds r) $ \s -> do
    start <- case mode of
      NoAcks -> getLastNotificationId s
      Acks _ -> initAcks r.user.uid r.cid s
    pure (newSubscription (streamKind s) s (fromMaybe UUID.nil start))

-- | Startup line of 'Acks' mode: all reader streams, and those with
-- notifications (start position not nil; InitAcks writes no row for the others).
acksResetLine :: Text -> [Subscription] -> Text
acksResetLine label subs =
  label
    <> ": acks reset to latest ("
    <> showT (length subs)
    <> " streams, "
    <> showT (length (filter ((/= UUID.nil) . (.start)) subs))
    <> " with notifications)"

-- | Result of fetching some streams of one client once.
data PassResult = PassResult
  { subs :: [Subscription],
    -- | some stream returned a full page: more is waiting
    more :: Bool,
    -- | newly delivered ids with their stream, in page order
    delivered :: [(Stream, UUID)],
    errors :: [Text]
  }

instance Semigroup PassResult where
  a <> b =
    PassResult
      (a.subs <> b.subs)
      (a.more || b.more)
      (a.delivered <> b.delivered)
      (a.errors <> b.errors)

instance Monoid PassResult where
  mempty = PassResult [] False [] []

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

-- | Fetches every stream of one client once. Returns a 'PassResult': the
-- advanced cursors, whether some stream returned a full page (more is
-- waiting), the newly delivered ids and the messages of failed fetches
-- (rendered by @render@). A failed fetch leaves its cursor unchanged.
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
  Sem r PassResult
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
        pure (PassResult [sub] False [] [kindName sub.kind <> ": fetch failed: " <> render err])
      Right page -> do
        let new = filter (`Set.notMember` sub.seenInWindow) (map fst page)
            full = length page >= limit
            cursor' = foldl' max sub.cursor new
            sub' = sub {cursor = cursor', resumeAfter = if full then fst <$> listToMaybe (reverse page) else Nothing}
            bound' = lowerBound windowMs sub'
            seen'
              | windowMs == 0 = mempty
              | otherwise = Set.dropWhileAntitone (<= bound') (foldl' (flip Set.insert) sub.seenInWindow new)
        nowMs <- embed currentMillis
        embed (recordFetch stats sub.kind (t1 - t0) (map (lagNs nowMs) new))
        pure (PassResult [sub' {seenInWindow = seen'}] full [(sub.stream, i) | i <- new] [])
  pure (mconcat results)

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

-- | Runs one store call; a synchronous exception or an error result goes to
-- @onFailure@ as text, so one failing stream never affects another. Async
-- exceptions (cancel) propagate.
isolated :: (e -> Text) -> (Text -> IO b) -> IO (Either e b) -> IO b
isolated render onFailure act =
  tryAny act >>= \case
    Left e -> onFailure (T.pack (displayException e))
    Right (Left e) -> onFailure (render e)
    Right (Right b) -> pure b

-- | One pass over the streams of a client, each fetched in isolation by
-- @fetchOne@ (a 'readerPass' over that stream alone): a failure only keeps
-- the cursor of its own stream (counted as fetch error).
readerRound ::
  (e -> Text) ->
  ReaderStats ->
  (Subscription -> IO (Either e PassResult)) ->
  [Subscription] ->
  IO PassResult
readerRound render stats fetchOne subs =
  mconcat <$> for subs (\sub -> isolated render (failed sub) (fetchOne sub))
  where
    -- constructor, not a record update: 'errors' is also a field of 'Metrics'
    failed sub msg = do
      recordFetchError stats sub.kind
      pure (PassResult [sub] False [] ["reader: " <> kindName sub.kind <> ": " <> msg])

-- | Writes one ack as the reader's (user, client) and records its latency
-- (pool wait included, like fetches). A failed write records nothing here
-- ('ackPending' counts it).
ackOne :: (Members '[FanInNotificationsStore, Embed IO] r) => ReaderStats -> Pop.Reader -> Pending -> Sem r ()
ackOne stats r p = do
  t0 <- embed getMonotonicTimeNSec
  ackNotification r.user.uid r.cid p.stream p.maxId
  t1 <- embed getMonotonicTimeNSec
  embed (recordAck stats (streamKind p.stream) (t1 - t0))

-- | Writes due acks one by one through @writeOne@; returns the failed ones
-- (counted and reported) so they stay pending.
ackPending :: (e -> Text) -> ReaderStats -> (Text -> IO ()) -> (Pending -> IO (Either e ())) -> [Pending] -> IO [Pending]
ackPending render stats reportError writeOne due =
  catMaybes <$> for due (\p -> isolated render (failed p) (fmap (Nothing <$) (writeOne p)))
  where
    failed p msg = do
      let k = streamKind p.stream
      recordAckError stats k
      reportError ("ack: " <> kindName k <> ": " <> msg)
      pure (Just p)

-- | Feeds delivered notifications into the ack state; writes acks when due
-- (via @writeAcks@, which returns the failed ones) and keeps failed ones
-- pending ('restoreFailed': retried at the next regular ack point).
acknowledge :: Int -> ([Pending] -> IO [Pending]) -> AckState -> [(Stream, UUID)] -> IO AckState
acknowledge n writeAcks = foldM step
  where
    step st (s, i) = case deliver n s i st of
      ([], st') -> pure st'
      (due, st') -> (`restoreFailed` st') <$> writeAcks due

-- | Lets at most as many readers as the pool has connections wait for it.
-- Thousands of readers blocked in the pool's STM retry are all woken on every
-- connection release; that thundering herd saturated the CPU and delayed the
-- ticker and drain by tens of seconds. A 'QSem' wakes one waiter at a time.
-- The wait for a slot is recorded separately (gate wait), not as part of the
-- fetch/ack latency.
throughGate :: QSem -> ReaderStats -> IO a -> IO a
throughGate gate stats = gateWith (recordGateWait stats) gate

-- | Passes the wait for a slot (ns) to @onWait@ once acquired.
gateWith :: (Word64 -> IO ()) -> QSem -> IO a -> IO a
gateWith onWait gate = bracket_ acquire (signalQSem gate)
  where
    acquire = do
      t0 <- getMonotonicTimeNSec
      waitQSem gate
      t1 <- getMonotonicTimeNSec
      onWait (t1 - t0)

-- | What a reader client needs for its store calls.
data ReaderCtx = ReaderCtx
  { gate :: QSem,
    env :: Env,
    client :: Pop.Reader,
    stats :: ReaderStats,
    reportError :: Text -> IO ()
  }

-- | One context per reader client of the population, in its order.
readerContexts :: QSem -> Env -> (Text -> IO ()) -> Population -> [ReaderStats] -> [ReaderCtx]
readerContexts gate env reportError population readerStats =
  [ReaderCtx {gate, env, client, stats, reportError} | (client, stats) <- zip (readers population) readerStats]

-- | One reader client: fetch all streams, re-fetch only those whose page was
-- full (more is waiting) until none is, then sleep. In 'Acks' mode every
-- delivered notification goes through 'acknowledge'; due acks are written
-- before the next fetch. Failures are reported and slow the loop down, never
-- stop it.
readerLoop :: ReaderCtx -> ReaderOptions -> Int -> [Subscription] -> IO ()
readerLoop ctx opts pollIntervalMs = go emptyAckState
  where
    go acks subs = do
      (subs', acks') <- readerCatchUp ctx opts acks subs
      threadDelay (pollIntervalMs * 1000)
      go acks' subs'

-- | One store call of a reader, through the gate.
gatedStore :: ReaderCtx -> Sem StoreEffects a -> IO (Either UsageError a)
gatedStore ctx = throughGate ctx.gate ctx.stats . runStore ctx.env

-- | Writes the given acks of a reader; returns the failed ones.
ackWriter :: ReaderCtx -> [Pending] -> IO [Pending]
ackWriter ctx =
  ackPending showT ctx.stats ctx.reportError (gatedStore ctx . ackOne ctx.stats ctx.client)

-- | Fetches the streams of one client until none returns a full page. A stream
-- whose fetch failed stays behind (counted as fetch error); 'readerLoop' fetches
-- it again on its next poll, 'runConsumeOnly' on its next pass.
readerCatchUp :: ReaderCtx -> ReaderOptions -> AckState -> [Subscription] -> IO ([Subscription], AckState)
readerCatchUp ctx opts acks0 subs0 = catchUp acks0 (map (,True) subs0)
  where
    fetchOne sub = gatedStore ctx (readerPass @UsageError showT opts.fetchLimit (fromIntegral opts.safetyWindowMs) ctx.stats [sub])
    received acks delivered = case opts.mode of
      NoAcks -> pure acks
      Acks n -> acknowledge n (ackWriter ctx) acks delivered
    -- @(subscription, poll again)@: only those whose last page was full
    pollOne acks (sub, again)
      | not again = pure (acks, [(sub, False)])
      | otherwise = do
          r <- readerRound showT ctx.stats fetchOne [sub]
          traverse_ ctx.reportError r.errors
          acks' <- received acks r.delivered
          pure (acks', map (,r.more) r.subs)
    catchUp acks pending = do
      (acks', polledLists) <- mapAccumM pollOne acks pending
      let polled = concat polledLists
      if any snd polled then catchUp acks' polled else pure (map fst polled, acks')

-- | Start positions are read before any writer runs: a push committed in
-- between could otherwise sit below a cursor and show up as a false `missing`.
loadSubscriptions :: Console -> QSem -> Env -> ReaderMode -> Population -> IO [[Subscription]]
loadSubscriptions console gate env mode population = do
  let label = experimentLabel mode
  printLine console (label <> ": loading start positions")
  loaded <- forConcurrently (readers population) (gateWith (const (pure ())) gate . runStore env . startPositions mode population.kinds)
  subs <- either (\e -> die ("loading start positions failed: " <> show e)) pure (sequence loaded)
  case mode of
    Acks _ -> printLine console (acksResetLine label (concat subs))
    NoAcks -> pure ()
  pure subs

-- | (expected, delivered) notifications so far.
totals :: [WriterStats] -> [ReaderStats] -> IO (Int, Int)
totals writerStats readerStats = do
  expected <- sum <$> traverse (fmap totalExpected . readSnapshot) writerStats
  delivered <- deliveredSoFar readerStats
  pure (expected, delivered)

-- | Waits until readers caught up, stopped receiving anything for
-- 'drainStallSeconds' of the poll interval (what is still missing is not
-- coming: lost, not in flight) or the timeout (seconds) passed.
drain :: Int -> Int -> IO (Int, Int) -> IO Drain
drain timeout pollIntervalMs readTotals = do
  started <- getMonotonicTime
  let deadline = started + fromIntegral timeout
      stallSeconds = drainStallSeconds pollIntervalMs
      go lastDelivered lastProgress = do
        (expected, delivered) <- readTotals
        now <- getMonotonicTime
        let progressAt = if delivered /= lastDelivered then now else lastProgress
        if
          | delivered >= expected -> pure (DrainComplete (now - started))
          | now >= deadline -> pure (DrainTimedOut (now - started))
          | now - progressAt >= stallSeconds -> pure (DrainStalled (now - started))
          | otherwise -> threadDelay 100_000 >> go delivered progressAt
  go (-1) started

-- | Idle readers sleep one poll interval between fetches, so the quiet period
-- that counts as stalled must outlast it: a fixed margin plus two intervals.
drainStallSeconds :: Int -> Double
drainStallSeconds pollIntervalMs = 2 + 2 * fromIntegral pollIntervalMs / 1000

-- | What the reader half of a tick needs, fixed for the whole run.
data ReaderTick = ReaderTick
  { console :: Console,
    consumeMetrics :: ConsumeMetrics,
    ackMetrics :: Maybe AckMetrics,
    warmup :: Double,
    readerStats :: [ReaderStats],
    errorQueue :: TBQueue Text,
    readerStateRef :: IORef ConsumeTickState
  }

-- | Runs on the ticker thread only (plus once after it stopped): aggregates,
-- prints, publishes.
consumeTickOnce ::
  ReaderMode ->
  Metrics ->
  ReaderTick ->
  -- | whether the writers still run (peaks ignore the drain)
  IO Bool ->
  [WriterStats] ->
  IORef TickState ->
  IO (TickReport, ConsumeReport)
consumeTickOnce mode metrics rt writing writerStats writerRef = do
  now <- getMonotonicTime
  writerReport <- tickWriters rt.warmup now writerStats writerRef
  isWriting <- writing
  readerReport <- tickReaders rt isWriting now (totalExpected writerReport.total)
  printQueuedErrors rt.console rt.errorQueue
  publish metrics writerReport
  publishReaders rt readerReport
  drawStatus rt.console (formatConsumeStatus mode writerReport readerReport)
  pure (writerReport, readerReport)

-- | Ack metrics exist in 'Acks' mode only.
ackMetricsFor :: ReaderMode -> Text -> IO (Maybe AckMetrics)
ackMetricsFor mode label = case mode of
  Acks _ -> Just <$> newAckMetrics label
  NoAcks -> pure Nothing

-- | The reader half of a tick, like 'tickWriters': sums the readers' counters,
-- advances the tick state. @tickReaders rt counting now expected@: @counting@
-- says whether peaks count (see 'consumeTick'), @expected@ the deliveries
-- expected so far.
tickReaders :: ReaderTick -> Bool -> Double -> Int -> IO ConsumeReport
tickReaders rt counting now expected = do
  total <- sumReaders rt.readerStats
  readerState <- readIORef rt.readerStateRef
  let (report, readerState') = consumeTick rt.warmup counting now expected total readerState
  writeIORef rt.readerStateRef readerState'
  pure report

-- | Publishes the reader metrics of a tick (the ack metrics in 'Acks' mode).
publishReaders :: ReaderTick -> ConsumeReport -> IO ()
publishReaders rt report = do
  publishConsume rt.consumeMetrics report
  traverse_ (`publishAcks` report) rt.ackMetrics

-- | Waits for the end of the run, stops the writers and drains. The first
-- Ctrl-C while waiting for writers or draining still prints the summary (a
-- second one kills the process: GHC resets the SIGINT handler). 'UserInterrupt'
-- is async, so UnliftIO's handlers miss it.
-- @stopWriters@ signals the writers and waits until they finished; it must be
-- idempotent. A Ctrl-C while stopping still waits for the writers' last
-- pushes: cancelling one after its commit would deliver it without counting it
-- as expected.
stopAndDrain :: Text -> Console -> Maybe Int -> IO () -> IO WriterPhase -> IO Drain -> IO (WriterPhase, Drain)
stopAndDrain label console duration stopWriters writerPhaseEnd runDrain = do
  waitForStop duration
  phaseRef <- newIORef Nothing
  let interrupted = do
        phase <- maybe (stopWriters >> writerPhaseEnd) pure =<< readIORef phaseRef
        pure (phase, DrainInterrupted)
  handleJust isInterrupt (const interrupted) $ do
    stopWriters
    phase <- writerPhaseEnd
    writeIORef phaseRef (Just phase)
    printLine console $ label <> ": writers stopped, draining"
    (phase,) <$> runDrain

-- | @runConsume console writerEnv readerEnv samplerEnv options@. The
-- environments differ only in their connection pool. With a @samplerEnv@ (its
-- own one-connection pool, for partitioned tables) the partition drops are
-- recorded and their impact is part of the summary.
runConsume :: Console -> Env -> Env -> Maybe Env -> ConsumeOptions -> IO RunSummary
runConsume console writerEnv readerEnv samplerEnv opts = do
  gate <- newQSem opts.readerPoolSize
  let p = opts.writerOptions
      label = experimentLabel opts.mode
  setup <- prepareWriters p
  (population, consumeMetrics, readerStats) <- setupReaders label opts.population p.clientsPerUser setup.entries setup.populationGen
  printRunHeader
    console
    label
    [ ("writers", showT p.writers),
      ("readers", showT (length readerStats)),
      ("targets", renderTargetSpec p.targets),
      ("seed", showT setup.seed)
    ]
    setup.entries
    population
  clientSubs <- loadSubscriptions console gate readerEnv opts.mode population

  metrics <- newMetrics label p.writers
  ackMetrics <- ackMetricsFor opts.mode label
  writerStats <- replicateM p.writers newWriterStats
  (errorQueue, reportError) <- newErrorReporter
  stopWriters <- newIORef False
  let -- A writer stops between pushes, so every committed push has its
      -- expected deliveries recorded before the drain starts.
      writer (ws, g) = writerLoop (readIORef stopWriters) (expectedDeliveries population.subscribers) (storePush writerEnv) setup.payload setup.entries ws reportError g
      readerClient (ctx, subs) = readerLoop ctx (readerOptions opts) opts.pollIntervalMs subs
      contexts = readerContexts gate readerEnv reportError population readerStats
      readTotals = totals writerStats readerStats
  -- after the subscriptions, whose acks setup writes
  checkpointBeforeRun writerEnv
  start <- getMonotonicTime
  writerRef <- newIORef (initialTickState start)
  readerStateRef <- newIORef (initialConsumeTickState start)
  let rt = ReaderTick {console, consumeMetrics, ackMetrics, warmup = fromIntegral p.warmup, readerStats, errorQueue, readerStateRef}
  samplesRef <- newIORef []
  dropsRef <- newIORef []
  let doTick = do
        (writerReport, readerReport) <- consumeTickOnce opts.mode metrics rt (not <$> readIORef stopWriters) writerStats writerRef
        -- one small record per second, only for the impact of the drops
        -- (forced: a lazy sample would keep both reports alive)
        when (isJust samplerEnv) $ modifyIORef' samplesRef ((:) $! sampleOf writerReport readerReport)
        pure (writerReport, readerReport)
      -- the writer phase ends when the writers stopped (or at Ctrl-C)
      writerPhaseEnd = do
        now <- getMonotonicTime
        total <- sumReaders readerStats
        pure WriterPhase {duration = now - start, delivered = totalDelivered total, acks = totalAcks total}
  printLine console $ label <> ": running"
  (writerPhase, drainResult) <-
    withAsync (forever (threadDelay 1_000_000 >> void doTick)) $ \tickerA -> do
      link tickerA
      withDropSampler start samplerEnv dropsRef $
        withAsync (mapConcurrently_ readerClient (zip contexts clientSubs)) $ \readersA -> do
          link readersA
          withAsync (mapConcurrently_ writer (zip writerStats setup.gens)) $ \writersA -> do
            link writersA
            let stop = atomicWriteIORef stopWriters True >> wait writersA
            stopAndDrain label console p.duration stop writerPhaseEnd (drain opts.drainTimeout opts.pollIntervalMs readTotals)
  -- ticker, readers and writers are cancelled now; the final tick and summary run here only
  (writerReport, readerReport) <- doTick
  drops <- readIORef dropsRef
  samples <- readIORef samplesRef
  let summary =
        (consumeSummary opts.mode (totalExpected writerReport.total) writerPhase drainResult writerReport readerReport)
          { gc = gcImpact (fromIntegral p.warmup) writerPhase.duration drops samples <$ samplerEnv
          }
  traverse_ (printLine console) (renderSummary summary)
  pure summary

-- | The readers of both consume commands: their population, metrics and one
-- stats record each. Ends the run if the population is invalid.
setupReaders :: Text -> PopulationOptions -> Int -> V.Vector Entry -> StdGen -> IO (Population, ConsumeMetrics, [ReaderStats])
setupReaders label o clientsPerUser entries gen = do
  population <- either (die . T.unpack) pure (mkPopulation o.onlineUsers o.epochsPerUser clientsPerUser entries gen)
  let n = readerCount population
  (population,,) <$> newConsumeMetrics label n <*> replicateM n newReaderStats

-- | The line announcing a run (@label: k=v ...@) and the coverage of the
-- streams by the readers.
printRunHeader :: Console -> Text -> [(Text, Text)] -> V.Vector Entry -> Population -> IO ()
printRunHeader console label fields entries population = do
  printLine console (formatRunHeader label fields)
  traverse_ (printLine console) (formatCoverage (coverage population.subscribers entries))

-- | Polls the oldest partition of every notification table once a second on
-- the sampler's own connection and records a 'DropEvent' (seconds since
-- @start@) for each table whose oldest partition changed. A failed poll ends
-- the run, like a failed metrics server.
withDropSampler :: Double -> Maybe Env -> IORef [DropEvent] -> IO a -> IO a
withDropSampler _ Nothing _ act = act
withDropSampler start (Just env) dropsRef act =
  withAsync (go Nothing) $ \samplerA -> link samplerA >> act
  where
    go previous = do
      current <- Map.fromList <$> runOrAbort "sampling the partitions" env oldestPartitions
      now <- getMonotonicTime
      for_ previous $ \before ->
        atomicModifyIORef' dropsRef (\ds -> (detectDrops (now - start) before current <> ds, ()))
      threadDelay 1_000_000
      go (Just current)

-- | Experiments E and F: readers only, on the rows an earlier @produce@ run
-- left. Every reader starts at the nil UUID in both modes (no 'startPositions':
-- latest would skip the backlog, and 'InitAcks' jumps to latest): a client that
-- was offline while the rows were written and has acknowledged nothing. The
-- expected deliveries come from the row counts of the database, so the run
-- also checks that the readers miss nothing.
runConsumeOnly :: Console -> Env -> ConsumeOnlyOptions -> IO RunSummary
runConsumeOnly console env opts = do
  let label = consumeOnlyLabel opts.reader.mode
      (entries, populationGen, _) = seededEntries opts.seed opts.clientsPerUser opts.targets
  (population, consumeMetrics, readerStats) <- setupReaders label opts.population opts.clientsPerUser entries populationGen
  printRunHeader
    console
    label
    [("readers", showT (length readerStats)), ("targets", renderTargetSpec opts.targets), ("seed", showT opts.seed)]
    entries
    population
  (totalRows, expected) <- expectedRows console label env population
  gate <- newQSem opts.readerPoolSize
  ackMetrics <- ackMetricsFor opts.reader.mode label
  (errorQueue, reportError) <- newErrorReporter
  let clientSubs = [[newSubscription (streamKind s) s UUID.nil | s <- readerStreams population.kinds r] | r <- readers population]
      contexts = readerContexts gate env reportError population readerStats
  -- built before the clock starts, not while the readers spawn
  traverse_ evaluate (concat clientSubs)
  -- after the row count, which can set hint bits (dirty pages)
  checkpointBeforeRun env
  start <- getMonotonicTime
  readerStateRef <- newIORef (initialConsumeTickState start)
  milestones <- newIORef [(p, Nothing) | p <- milestonePercents]
  let rt = ReaderTick {console, consumeMetrics, ackMetrics, warmup = fromIntegral opts.warmup, readerStats, errorQueue, readerStateRef}
      -- cheap once all milestones are reached: one read of the ref
      checkMilestones = do
        pending <- any (isNothing . snd) <$> readIORef milestones
        when pending $ do
          delivered <- deliveredSoFar readerStats
          now <- getMonotonicTime
          modifyIORef' milestones (reachMilestones expected delivered (now - start))
      -- polled finer than the ticker: a run can be over in less than a second
      watchMilestones = forever (threadDelay 50_000 >> checkMilestones)
      doTick = do
        now <- getMonotonicTime
        report <- tickReaders rt True now expected
        printQueuedErrors console errorQueue
        publishReaders rt report
        drawStatus console (formatConsumeOnlyStatus opts.reader.mode (now - start) report)
        pure report
  printLine console $ label <> ": running"
  -- Every reader catches up once and stops, so the run ends when the last one is
  -- done (no polling of empty streams, no stall heuristic that could end the run
  -- while readers still wait for a connection).
  result <-
    withAsync (forever (threadDelay 1_000_000 >> void doTick)) $ \tickerA -> do
      link tickerA
      withAsync watchMilestones $ \watcherA -> do
        link watcherA
        withAsync (mapConcurrently_ (uncurry (consumeOnlyClient opts.reader)) (zip contexts clientSubs)) $ \readersA -> do
          link readersA
          -- the first Ctrl-C still prints the summary
          handleJust isInterrupt (const (pure ConsumeInterrupted)) $ do
            finished <- if opts.timeout == 0 then Just <$> wait readersA else System.Timeout.timeout (opts.timeout * 1_000_000) (wait readersA)
            pure (maybe ConsumeTimedOut (const ConsumeFinished) finished)
  end <- getMonotonicTime
  report <- doTick
  checkMilestones
  reached <- readIORef milestones
  let summary = (consumeOnlySummary opts.reader.mode expected (end - start) result reached report) {rowsTotal = Just totalRows}
  traverse_ (printLine console) (renderSummary summary)
  pure summary

-- | @expectedRows console label env population@: the rows of the database and
-- the deliveries the readers should get from them. Dies when there is nothing
-- to read.
expectedRows :: Console -> Text -> Env -> Population -> IO (Int, Int)
expectedRows console label env population = do
  counts <- runOrAbort "counting rows" env countStreams
  let totalRows = sum (map snd counts)
      subscribed = subscribedRows population.subscribers counts
      expected = expectedFromCounts population.subscribers counts
  printLine console $ label <> ": rows total=" <> showT totalRows <> " in subscribed streams=" <> showT subscribed <> " expected=" <> showT expected
  when (totalRows == 0) $ die "no notifications in the database: run 'produce' first"
  when (subscribed == 0) $ die "none of the notifications is in a stream the readers subscribe to: are --seed, --targets and --clients-per-user those of the 'produce' run?"
  pure (totalRows, expected)

-- | A reader client of 'runConsumeOnly'. A pass with fetch errors leaves
-- streams behind; it is repeated after a constant pause (until it is clean or
-- --timeout), else their rest would count as missing.
consumeOnlyClient :: ReaderOptions -> ReaderCtx -> [Subscription] -> IO ()
consumeOnlyClient opts ctx = go emptyAckState
  where
    go acks subs = do
      before <- totalFetchErrors <$> readReaderSnapshot ctx.stats
      (subs', acks') <- readerCatchUp ctx opts acks subs
      after <- totalFetchErrors <$> readReaderSnapshot ctx.stats
      if after > before
        then threadDelay 100_000 >> go acks' subs'
        else case opts.mode of
          -- a real client acks what it read before it goes idle
          Acks _ -> void (ackWriter ctx (pendingAcks acks'))
          NoAcks -> pure ()

-- | @reachMilestones expected delivered elapsed@: stamps the milestones that
-- @delivered@ of @expected@ reached with @elapsed@ (seconds), keeps the others.
reachMilestones :: Int -> Int -> Double -> [(Int, Maybe Double)] -> [(Int, Maybe Double)]
reachMilestones expected delivered elapsed = map reach
  where
    reach (p, t) = (p, t <|> (elapsed <$ guard (delivered * 100 >= expected * p)))

-- | Shares of the expected deliveries 'runConsumeOnly' reports the time to.
milestonePercents :: [Int]
milestonePercents = [50, 90, 99]
