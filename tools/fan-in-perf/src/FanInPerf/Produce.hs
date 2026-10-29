module FanInPerf.Produce
  ( PushOutcome (..),
    WriterSetup (..),
    writerStep,
    storePush,
    prepareWriters,
    seededEntries,
    writerLoop,
    tickWriters,
    runProduce,
  )
where

import Control.Concurrent.Async
import Control.Exception (evaluate)
import Data.Aeson qualified as A
import Data.List.NonEmpty (NonEmpty)
import Data.Misc (showT)
import Data.Text qualified as T
import Data.Vector qualified as V
import FanInPerf.Metrics
import FanInPerf.NotificationsStore
import FanInPerf.Options (ProduceOptions (..))
import FanInPerf.Stats
import FanInPerf.Store
import FanInPerf.Summary (RunSummary, produceSummary)
import FanInPerf.Targets
import FanInPerf.Terminal
import FanInPerf.Worker
import GHC.Clock (getMonotonicTime, getMonotonicTimeNSec)
import Imports
import System.Random
import UnliftIO.Exception (tryAny)

data PushOutcome = PushOk | PushFailed Text

-- | One push: generate targets, run, record. Synchronous exceptions are
-- counted as errors so a writer never dies; async ones (cancel) propagate.
-- @expectedOf@ gives the deliveries readers should see for a successful push
-- (computed outside the timed store call).
writerStep ::
  (FanInPush -> Int) ->
  (FanInPush -> IO PushOutcome) ->
  Int ->
  V.Vector Entry ->
  WriterStats ->
  (Text -> IO ()) ->
  StdGen ->
  IO StdGen
writerStep expectedOf doPush payloadBytes entries stats reportError g = do
  let (push, g') = genPush payloadBytes entries g
  -- build targets and payload before timing so latency covers only the store call
  evaluate (forceTargets push.targets)
  evaluate (forcePayload push.json)
  t0 <- getMonotonicTimeNSec
  outcome <- either (PushFailed . T.pack . displayException) id <$> tryAny (doPush push)
  t1 <- getMonotonicTimeNSec
  recordOutcome push outcome t0 t1
  pure g'
  where
    -- 'Target' has strict fields, so forcing each element to WHNF is enough.
    forceTargets :: [Target] -> ()
    forceTargets = foldr seq ()

    -- 'mkPayload' values are strings with a strict 'Text', so WHNF is enough.
    forcePayload :: A.Object -> ()
    forcePayload = foldr seq ()

    recordOutcome push outcome t0 t1 = do
      let kind = pushKind push
      case outcome of
        PushOk -> recordSuccess stats kind (length push.targets) (expectedOf push) (t1 - t0)
        PushFailed msg -> do
          recordError stats kind
          reportError (kindName kind <> ": " <> msg)

storePush :: Env -> FanInPush -> IO PushOutcome
storePush env push =
  either (PushFailed . showT) (const PushOk)
    <$> runStore env (pushViaFanIn push)

-- | Everything the writers of a run start from.
data WriterSetup = WriterSetup
  { seed :: Int,
    entries :: V.Vector Entry,
    payloadBytes :: Int,
    gens :: [StdGen],
    populationGen :: StdGen
  }

-- | Seed (random unless given), entries, payload and one generator per writer.
prepareWriters :: ProduceOptions -> IO WriterSetup
prepareWriters opts = do
  seed <- maybe randomIO pure opts.seed
  let (entries, populationGen, writerGen) = seededEntries seed opts.clientsPerUser opts.targets
      payloadBytes = opts.payloadBytes
      gens = take opts.writers (unfoldr (Just . split) writerGen)
  pure WriterSetup {..}

-- | @seededEntries seed clientsPerUser targets@: the stream key pools, the
-- generator of the population and the one the writers' generators are split
-- from. Depends on nothing else, so a later @consume-only@ run with the same
-- arguments rebuilds the pools a @produce@ run wrote to.
seededEntries :: Int -> Int -> NonEmpty TargetConfig -> (V.Vector Entry, StdGen, StdGen)
seededEntries seed clientsPerUser targets =
  let (entries, gen') = mkEntries clientsPerUser targets (mkStdGen seed)
      (populationGen, writerGen) = split gen'
   in (entries, populationGen, writerGen)

-- | Pushes until @stop@ reports true; checked between pushes, so no push is
-- cut off.
writerLoop ::
  IO Bool ->
  (FanInPush -> Int) ->
  (FanInPush -> IO PushOutcome) ->
  Int ->
  V.Vector Entry ->
  WriterStats ->
  (Text -> IO ()) ->
  StdGen ->
  IO ()
writerLoop stop expectedOf doPush payloadBytes entries ws reportError = loop
  where
    loop !g = do
      stopped <- stop
      unless stopped (writerStep expectedOf doPush payloadBytes entries ws reportError g >>= loop)

runProduce :: Console -> Env -> ProduceOptions -> IO RunSummary
runProduce console env opts = do
  setup <- prepareWriters opts
  metrics <- newMetrics "produce" opts.writers
  stats <- replicateM opts.writers newWriterStats
  (errors, reportError) <- newErrorReporter
  let writer (ws, g) = writerLoop (pure False) (const 0) (storePush env) setup.payloadBytes setup.entries ws reportError g
  checkpointBeforeRun env
  start <- getMonotonicTime
  stateRef <- newIORef (initialTickState start)
  let doTick = tickOnce console metrics (fromIntegral opts.warmup) stats errors stateRef
  printLine console $
    formatRunHeader "produce" [("writers", showT opts.writers), ("targets", renderTargetSpec opts.targets), ("seed", showT setup.seed)]
  withAsync (mapConcurrently_ writer (zip stats setup.gens)) $ \writersA -> do
    link writersA
    withAsync (forever (threadDelay 1_000_000 >> void doTick)) $ \tickerA -> do
      link tickerA
      waitForStop opts.duration
  -- leaving 'withAsync' cancelled writers and ticker; the final tick and
  -- summary run on this thread only after the ticker is gone.
  report <- doTick
  let summary = produceSummary report
  traverse_ (printLine console) (renderSummary summary)
  pure summary

-- | Runs on the ticker thread only (plus once after it stopped): aggregates,
-- prints, publishes.
tickOnce :: Console -> Metrics -> Double -> [WriterStats] -> TBQueue Text -> IORef TickState -> IO TickReport
tickOnce console metrics warmup stats errors ref = do
  now <- getMonotonicTime
  report <- tickWriters warmup now stats ref
  printQueuedErrors console errors
  publish metrics report
  drawStatus console (formatStatus report)
  pure report

-- | Writer half of a tick: sums the writers' counters, advances the tick
-- state.
tickWriters :: Double -> Double -> [WriterStats] -> IORef TickState -> IO TickReport
tickWriters warmup now stats ref = do
  total <- sumSnapshots <$> traverse readSnapshot stats
  st <- readIORef ref
  let (report, st') = tick warmup now total st
  writeIORef ref st'
  pure report
