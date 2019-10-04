-- | Like "Brig.Budget", but in-memory, per host (not per service), and on the upside more
-- exact.  Like https://hackage.haskell.org/package/token-bucket, but takes the entire
-- run-time of the actions into account, not just the number of executions.
-- http://hackage.haskell.org/package/rate-limit also looks related.
--
-- USE CASE: keep a lid of stalled native push notification threadsh.  if SNS is up, there
-- will be many, short-running executions of the action.  when SNS is down, the threads will
-- accumulate in memory and choke the gundeck instances.  so we want to stop spawning more
-- threads (and discard or queue native push notifications) before we run out of memory (which
-- could cause system outages).
module Gundeck.ThreadBudget
  ( ThreadBudgetState
  , mkThreadBudgetState
  , runWithBudget
  , watchThreadBudgetState
  ) where

import Imports

import Control.Exception.Safe (catchAny)
import Control.Lens
import Control.Monad.Catch (MonadCatch)
import Data.Metrics (Metrics)
import Data.Metrics.Middleware (gaugeSet, path)
import Data.SizedHashMap (SizedHashMap)
import Data.UUID (UUID, toText)
import Data.UUID.V4 (nextRandom)
import UnliftIO.Async
import UnliftIO.Exception (finally)

import qualified Data.Set as Set
import qualified Data.SizedHashMap as SHM
import qualified System.Logger.Class as LC


data ThreadBudgetState = ThreadBudgetState
  { _threadBudgetLimit   :: Int
  , _threadBudgetRunning :: IORef BudgetMap
  }

-- | We store not only the handle (for cleanup in 'watchThreadBudgetState'), but also the
-- number of spent resources at the time of allocating this one (so we can see if two actions
-- are running on the same budget token because they have been allocated concurrently).
type BudgetMap = SizedHashMap UUID (Int, Maybe (Async ()))

showDebugHandles :: BudgetMap -> String
showDebugHandles = show . fmap (_2 %~ fst) . SHM.toList

mkThreadBudgetState :: Int -> IO ThreadBudgetState
mkThreadBudgetState limit = ThreadBudgetState limit <$> newIORef SHM.empty

register
  :: IORef BudgetMap -> UUID -> Int -> Maybe (Async ()) -> MonadIO m => m ()
register ref key spent mhandle
  = atomicModifyIORef' ref (\hm -> (SHM.insert key (spent, mhandle) hm, ()))

unregister
  :: IORef BudgetMap -> UUID -> MonadIO m => m ()
unregister ref key
  = atomicModifyIORef' ref (\hm -> (SHM.delete key hm, ()))


-- | If there is budget available, make a synchronously to the action; otherwise, log a
-- warning and return immediately.  Make sure the budget state is updated accordingly both
-- when starting and ending the execution.
--
-- The limit in the 'ThreadBudgetState' argument is guaranteed to hold in the following sense:
-- if there are never more than N parallel calls to 'runWithBudget' within a few microseconds,
-- the limit will not be exceeded by more than N.
runWithBudget
  :: forall m. (MonadIO m, LC.MonadLogger m, MonadUnliftIO m)
  => ThreadBudgetState -> m () -> m ()
runWithBudget (ThreadBudgetState limit ref) action = do
  key <- liftIO nextRandom
  (`finally` unregister ref key) $ do
    spent <- SHM.size <$> readIORef ref
    register ref key spent Nothing
    if spent >= limit then nobudget else go key spent
  where
    go :: UUID -> Int -> m ()
    go key spent = do
      readIORef ref >>= \debugHandles -> LC.debug $
        "key"   LC..= (toText key) LC.~~
        "spent" LC..= spent LC.~~
        "map"   LC..= showDebugHandles debugHandles LC.~~
        LC.msg (LC.val "runWithBudget: go")

      handle <- async action
      register ref key spent (Just handle)
      wait handle

    nobudget :: m ()
    nobudget = do
      readIORef ref >>= \debugHandles -> LC.debug $
        "map" LC..= showDebugHandles debugHandles LC.~~
        LC.msg (LC.val "runWithBudget: nobudget")

      LC.warn $ LC.msg (LC.val "runWithBudget: out of budget.")


-- | Fork a thread that checks every 'watcherFreq' seconds if any async handles stored in the
-- state are stale (ie., have terminated with or without exception, but not been removed).  If
-- they persist in the state for more 1sec, call an error action (usually for logging a
-- warning).
--
-- 'runWithBudget' should keep track of the state itself; this function is just a safety
-- precaution to see if there aren't any corner cases we missed.
watchThreadBudgetState
  :: forall m. (MonadIO m, LC.MonadLogger m, MonadCatch m)
  => Metrics -> ThreadBudgetState -> Int -> m ()
watchThreadBudgetState metrics (ThreadBudgetState _limit ref) freq = safeForever $ do
  recordMetrics metrics ref
  removeStaleHandles ref
  threadDelay $ freq * 1000000

recordMetrics
  :: forall m. (MonadIO m, LC.MonadLogger m, MonadCatch m)
  => Metrics -> IORef BudgetMap -> m ()
recordMetrics metrics ref = do
  spent <- SHM.size <$> readIORef ref
  gaugeSet (fromIntegral spent) (path "net.sns.num_open_connections") metrics

staleToleranceMs :: Int
staleToleranceMs = 30  -- 1 is enough for normal hardware

-- | Get all handles for asyncs that have terminated, but not been removed from the state.  Do
-- that again after 'staleToleranceMs' millisecs to make sure that we don't catch any handles
-- that would have been removed during the 'runWithBudget' cleanup, but we were faster.  The
-- intersection between the two rounds constitutes the legitimately stale handles: warn about
-- them, and then remove them from the hashmap.
removeStaleHandles
  :: forall m. (MonadIO m, LC.MonadLogger m, MonadCatch m)
  => IORef BudgetMap -> m ()
removeStaleHandles ref = do
  round1 <- getStaleHandles
  threadDelay (staleToleranceMs * 1000)
  round2 <- getStaleHandles

  let staleHandles = Set.intersection round1 round2

  unless (null staleHandles) $ do
    warnStaleHandles (Set.size staleHandles) =<< readIORef ref
    forM_ staleHandles $ \key -> do
      mapM_ waitCatch . join . fmap snd =<< SHM.lookup key <$> readIORef ref
      unregister ref key

  where
    getStaleHandles :: m (Set UUID)
    getStaleHandles = Set.fromList . mconcat <$> do
      handles <- SHM.toList <$> readIORef ref
      forM handles $ \case
        (_, (_, Nothing)) -> do
          pure []
        (tid, (_, Just handle)) -> do
          status <- poll handle
          pure [tid | isJust status]

    warnStaleHandles :: Int -> BudgetMap -> m ()
    warnStaleHandles num handles = LC.warn $
      "BudgetMap" LC..= showDebugHandles handles
      LC.~~ LC.msg ("watchThreadBudgetState: removed " <> show num <> " stale handles.")

safeForever
  :: forall m. (MonadIO m, LC.MonadLogger m, MonadCatch m)
  => m () -> m ()
safeForever action = forever $ action `catchAny` \exc -> do
  LC.err $ "error" LC..= show exc LC.~~ LC.msg (LC.val "watchThreadBudgetState: crashed; retrying")
  threadDelay 60000000  -- pause to keep worst-case noise in logs manageable
