module FanInPerf.Produce
  ( PushOutcome (..),
    writerStep,
    runProduce,
  )
where

import Control.Concurrent.Async
import Control.Exception (AsyncException (UserInterrupt), evaluate, handleJust)
import Data.Aeson qualified as A
import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as T
import Data.Vector qualified as V
import FanInPerf.Metrics
import FanInPerf.Options (ProduceOptions (..))
import FanInPerf.Stats
import FanInPerf.Store
import FanInPerf.Targets
import FanInPerf.Terminal
import GHC.Clock (getMonotonicTime, getMonotonicTimeNSec)
import Imports
import System.Random
import UnliftIO.Exception (tryAny)
import Wire.FanInNotificationsStore

data PushOutcome = PushOk | PushFailed Text

-- REVIEW: Dependency injection of doPush and reportError is odd. This is what effects are for!

-- | One push: generate targets, run, record. Synchronous exceptions are
-- counted as errors so a writer never dies; async ones (cancel) propagate.
writerStep ::
  (NonEmpty Target -> IO PushOutcome) ->
  V.Vector Entry ->
  WriterStats ->
  (Text -> IO ()) ->
  StdGen ->
  IO StdGen
writerStep doPush entries stats reportError g = do
  -- REVIEW: Can kind not be deduced from targets?
  let ((kind, targets), g') = genTargets entries g
  -- build the targets before timing so latency covers only the store call
  evaluate (forceTargets targets)
  t0 <- getMonotonicTimeNSec
  outcome <- either (PushFailed . T.pack . displayException) id <$> tryAny (doPush targets)
  t1 <- getMonotonicTimeNSec
  case outcome of
    PushOk -> recordSuccess stats kind (length targets) (t1 - t0)
    PushFailed msg -> do
      recordError stats kind
      reportError (kindName kind <> ": " <> msg)
  pure g'

storePush :: Env -> A.Object -> NonEmpty Target -> IO PushOutcome
storePush env payload targets =
  either (PushFailed . T.pack . show) (const PushOk)
    <$> runStore env (pushViaFanIn (mkPush payload targets))

runProduce :: Console -> Env -> ProduceOptions -> IO ()
runProduce console env opts = do
  gen <- initStdGen
  let (entries, gen') = mkEntries opts.clientsPerUser opts.targets gen
      payload = mkPayload opts.payloadBytes
      writerGens = take opts.writers (unfoldr (Just . split) gen')
  metrics <- newMetrics "produce" opts.writers
  stats <- replicateM opts.writers newWriterStats
  errors <- newTBQueueIO 1000
  let -- error path only; drops messages when the ticker falls behind
      reportError msg = atomically $ do
        full <- isFullTBQueue errors
        unless full (writeTBQueue errors msg)
      writer (ws, g0) =
        let loop !g = writerStep (storePush env payload) entries ws reportError g >>= loop
         in loop g0
  start <- getMonotonicTime
  stateRef <- newIORef (initialTickState start)
  let doTick = tickOnce console metrics (fromIntegral opts.warmup) stats errors stateRef
  printLine console $
    "produce: writers=" <> T.pack (show opts.writers) <> " targets=" <> renderTargetSpec opts.targets
  withAsync (mapConcurrently_ writer (zip stats writerGens)) $ \writersA -> do
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
  total <- sumSnapshots <$> traverse readSnapshot stats
  st <- readIORef ref
  let (report, st') = tick warmup now total st
  writeIORef ref st'
  msgs <- atomically (flushTBQueue errors)
  traverse_ (printLine console) (take maxErrorLines msgs)
  when (length msgs > maxErrorLines) $
    printLine console ("... " <> T.pack (show (length msgs - maxErrorLines)) <> " more errors suppressed")
  publish metrics report
  drawStatus console (formatStatus report)
  pure report
  where
    maxErrorLines = 10

-- | Returns after @duration@ seconds or on Ctrl-C (GHC delivers SIGINT as
-- 'UserInterrupt' to the main thread).
waitForStop :: Maybe Int -> IO ()
waitForStop mDuration = handleJust isInterrupt pure sleepFor
  where
    sleepFor = maybe (forever (threadDelay 1_000_000)) (\d -> threadDelay (d * 1_000_000)) mDuration
    isInterrupt UserInterrupt = Just ()
    isInterrupt _ = Nothing
