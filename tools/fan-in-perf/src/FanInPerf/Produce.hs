module FanInPerf.Produce
  ( PushOutcome (..),
    WriterSetup (..),
    writerStep,
    storePush,
    prepareWriters,
    writerLoop,
    tickWriters,
    runProduce,
  )
where

import Control.Concurrent.Async
import Control.Exception (evaluate)
import Data.Aeson qualified as A
import Data.Text qualified as T
import Data.Vector qualified as V
import FanInPerf.Metrics
import FanInPerf.Options (ProduceOptions (..))
import FanInPerf.Stats
import FanInPerf.Store
import FanInPerf.Targets
import FanInPerf.Terminal
import FanInPerf.Worker
import GHC.Clock (getMonotonicTime, getMonotonicTimeNSec)
import Imports
import System.Random
import UnliftIO.Exception (tryAny)
import Wire.FanInNotificationsStore

data PushOutcome = PushOk | PushFailed Text

-- | One push: generate targets, run, record. Synchronous exceptions are
-- counted as errors so a writer never dies; async ones (cancel) propagate.
writerStep ::
  (FanInPush -> IO PushOutcome) ->
  A.Object ->
  V.Vector Entry ->
  WriterStats ->
  (Text -> IO ()) ->
  StdGen ->
  IO StdGen
writerStep doPush payload entries stats reportError g = do
  let (push, g') = genPush payload entries g
  -- build the targets before timing so latency covers only the store call
  evaluate (forceTargets push.targets)
  t0 <- getMonotonicTimeNSec
  outcome <- either (PushFailed . T.pack . displayException) id <$> tryAny (doPush push)
  t1 <- getMonotonicTimeNSec
  recordOutcome push outcome t0 t1
  pure g'
  where
    -- 'Target' has strict fields, so forcing each element to WHNF is enough.
    forceTargets :: [Target] -> ()
    forceTargets = foldr seq ()

    recordOutcome push outcome t0 t1 = do
      let kind = pushKind push
      case outcome of
        PushOk -> recordSuccess stats kind (length push.targets) (t1 - t0)
        PushFailed msg -> do
          recordError stats kind
          reportError (kindName kind <> ": " <> msg)

storePush :: Env -> FanInPush -> IO PushOutcome
storePush env push =
  either (PushFailed . tshow) (const PushOk)
    <$> runStore env (pushViaFanIn push)

-- | Everything the writers of a run start from.
data WriterSetup = WriterSetup
  { seed :: Int,
    entries :: V.Vector Entry,
    payload :: A.Object,
    gens :: [StdGen],
    populationGen :: StdGen
  }

-- | Seed (random unless given), entries, payload and one generator per writer.
prepareWriters :: ProduceOptions -> IO WriterSetup
prepareWriters opts = do
  seed <- maybe randomIO pure opts.seed
  let (entries, gen') = mkEntries opts.clientsPerUser opts.targets (mkStdGen seed)
      (populationGen, gen'') = split gen'
      payload = mkPayload opts.payloadBytes
      gens = take opts.writers (unfoldr (Just . split) gen'')
  pure WriterSetup {..}

-- | Pushes until @stop@ reports true; checked between pushes, so no push is
-- cut off.
writerLoop ::
  IO Bool ->
  (FanInPush -> IO PushOutcome) ->
  A.Object ->
  V.Vector Entry ->
  WriterStats ->
  (Text -> IO ()) ->
  StdGen ->
  IO ()
writerLoop stop doPush payload entries ws reportError = loop
  where
    loop !g = do
      stopped <- stop
      unless stopped (writerStep doPush payload entries ws reportError g >>= loop)

runProduce :: Console -> Env -> ProduceOptions -> IO ()
runProduce console env opts = do
  setup <- prepareWriters opts
  metrics <- newMetrics "produce" opts.writers
  stats <- replicateM opts.writers newWriterStats
  (errors, reportError) <- newErrorReporter
  let writer (ws, g) = writerLoop (pure False) (storePush env) setup.payload setup.entries ws reportError g
  start <- getMonotonicTime
  stateRef <- newIORef (initialTickState start)
  let doTick = tickOnce console metrics (fromIntegral opts.warmup) stats errors stateRef
  printLine console $
    "produce: writers=" <> tshow opts.writers <> " targets=" <> renderTargetSpec opts.targets <> " seed=" <> tshow setup.seed
  withAsync (mapConcurrently_ writer (zip stats setup.gens)) $ \writersA -> do
    link writersA
    withAsync (forever (threadDelay 1_000_000 >> void doTick)) $ \tickerA -> do
      link tickerA
      waitForStop opts.duration
  -- leaving 'withAsync' cancelled writers and ticker; the final tick and
  -- summary run on this thread only after the ticker is gone.
  report <- doTick
  traverse_ (printLine console) (formatSummary report)

-- | Runs on the ticker thread only (plus once after it stopped): aggregates,
-- prints, publishes.
tickOnce :: Console -> Metrics -> Double -> [WriterStats] -> TBQueue Text -> IORef TickState -> IO TickReport
tickOnce console metrics warmup stats errors ref = do
  now <- getMonotonicTime
  (report, _) <- tickWriters warmup now stats ref
  printQueuedErrors console errors
  publish metrics report
  drawStatus console (formatStatus report)
  pure report

-- | Writer half of a tick: sums the writers' counters, advances the tick
-- state. Returns the report and the summed counters.
tickWriters :: Double -> Double -> [WriterStats] -> IORef TickState -> IO (TickReport, Snapshot)
tickWriters warmup now stats ref = do
  total <- sumSnapshots <$> traverse readSnapshot stats
  st <- readIORef ref
  let (report, st') = tick warmup now total st
  writeIORef ref st'
  pure (report, total)
