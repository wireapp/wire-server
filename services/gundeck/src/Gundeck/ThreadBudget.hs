-- | Like "Brig.Budget", but in-memory, per host (not per service), and with an strict/exact
-- upper bound.  Like https://hackage.haskell.org/package/token-bucket, but takes the entire
-- run-time of the actions into account, not just the number of executions.
-- http://hackage.haskell.org/package/rate-limit also looks related.
--
-- USE CASE: keep a lid of stalled native push notification threads.  if SNS is up, there
-- will be many short-running executions of the action.  when SNS is down, the threads will
-- accumulate in memory and choke the gundeck instances.  so we want to stop spawning more
-- threads (and discard or queue native push notifications) before we run out of memory (which
-- could cause system outages).
--
-- FUTUREWORK: http-client connection pools should handle this naturally and without doing
-- anything, but instead connection pools grow infinitely until system resources (file
-- handles, memory) are exhausted.  See
-- https://github.com/snoyberg/http-client/issues/307#issuecomment-343829351.  We tried to fix
-- this here: https://github.com/wireapp/wire-server/pull/609, but getting this right requires
-- quite some digging: https://github.com/snoyberg/http-client/issues/394.  So if you ever
-- want to figure this out properly, plan in some time for it.
module Gundeck.ThreadBudget
  ( ThreadBudgetState
  , mkThreadBudgetState
  , runWithBudget
  , runWithBudget'
  , watchThreadBudgetState

  -- * for testing
  , threadBudgetLimits
  , runningThreads
  , cancelAllThreads
  ) where

import Imports

import Control.Exception (ErrorCall(ErrorCall))
import Control.Exception.Safe (catchAny)
import Control.Lens
import Control.Monad.Catch (MonadCatch, throwM)
import Data.Metrics (Metrics)
import Data.Metrics.Middleware (gaugeSet, path)
import Data.SizedHashMap (SizedHashMap)
import Data.Time
import Data.UUID (UUID, toText)
import Data.UUID.V4 (nextRandom)
import Gundeck.Options
import UnliftIO.Async
import UnliftIO.Exception (finally)

import qualified Data.Set as Set
import qualified Data.SizedHashMap as SHM
import qualified System.Logger.Class as LC


data ThreadBudgetState = ThreadBudgetState
  { threadBudgetLimits  :: MaxConcurrentNativePushes
  , _threadBudgetRunning :: IORef BudgetMap
  } deriving (Generic)

-- | Store all handles for cleanup in 'watchThreadBudgetState'.
type BudgetMap = SizedHashMap UUID (Maybe (Async ()))

-- | Instead of taking the size of the SizedHashMap (O(1)), this counts all the threads that
-- are successfully running (dropping the ones that are just about to try to grab a token,
-- taking O(n)).
runningThreads :: ThreadBudgetState -> IO Int
runningThreads (ThreadBudgetState _ running)
  = length . filter isJust . SHM.elems <$> readIORef running

cancelAllThreads :: ThreadBudgetState -> IO ()
cancelAllThreads (ThreadBudgetState _ ref) = readIORef ref
  >>= mapM_ cancel . catMaybes . SHM.elems

mkThreadBudgetState :: HasCallStack => MaxConcurrentNativePushes -> IO ThreadBudgetState
mkThreadBudgetState limits = if limits ^. limitHard < limits ^. limitSoft
  then throwM . ErrorCall $
         "setMaxConcurrentNativePushes: hard limit < soft limit: " <> show limits
  else ThreadBudgetState limits <$> newIORef SHM.empty

register
  :: IORef BudgetMap -> UUID -> Maybe (Async ()) -> MonadIO m => m Int
register ref key mhandle
  = atomicModifyIORef' ref (\hm -> (SHM.insert key mhandle hm, SHM.size hm))

unregister
  :: IORef BudgetMap -> UUID -> MonadIO m => m ()
unregister ref key
  = atomicModifyIORef' ref (\hm -> (SHM.delete key hm, ()))


-- | If there is budget available, execute the action synchronously; otherwise, log a warning
-- and return immediately.  Make sure the budget state is updated accordingly both when
-- starting and ending the execution.
--
-- The hard limit in the 'ThreadBudgetState' argument is guaranteed to be an upper bound for
-- the number of concurrently running actions; surpassing the soft limit will trigger a
-- warning, but still execute the action.
--
-- The action is called in an 'Async', but 'runWithBudget' waits for it to finish so it can
-- update the budget.
runWithBudget
  :: forall m. (MonadIO m, LC.MonadLogger m, MonadUnliftIO m)
  => ThreadBudgetState -> m () -> m ()
runWithBudget tbs action = runWithBudget' tbs () action

-- | More flexible variant of 'runWithBudget' that allows the action to return a value.  With
-- a default in case of budget exhaustion.
runWithBudget'
  :: forall m a. (MonadIO m, LC.MonadLogger m, MonadUnliftIO m)
  => ThreadBudgetState -> a -> m a -> m a
runWithBudget' (ThreadBudgetState limits ref) fallback action = do
  key <- liftIO nextRandom
  (`finally` unregister ref key) $ do
    oldsize <- register ref key Nothing

    warnNoBudget (maybe False (oldsize >=) (limits ^. limitSoft))
                 (maybe False (oldsize >=) (limits ^. limitHard))
    if (maybe True (oldsize <) (limits ^. limitHard))
        then go key oldsize
        else pure fallback
  where
    go :: UUID -> Int -> m a
    go key oldsize = do
      readIORef ref >>= \debugHandles -> LC.debug $
        "key"   LC..= (toText key) LC.~~
        "spent" LC..= oldsize LC.~~
        "map"   LC..= show (SHM.size debugHandles) LC.~~
        LC.msg (LC.val "runWithBudget: go")

      handle <- async action
      _ <- register ref key (Just (const () <$> handle))
      wait handle

    -- iff soft and/or hard limit are breached, log a warning-level message.
    warnNoBudget :: Bool -> Bool -> m ()
    warnNoBudget False False = pure ()
    warnNoBudget soft hard = do
      debugHandles <- readIORef ref
      let limit = if hard then "hard" else "soft"
      LC.warn $
        "map" LC..= show (SHM.size debugHandles) LC.~~
        "soft-breach" LC..= soft LC.~~
        "hard-breach" LC..= hard LC.~~
        LC.msg (LC.val "runWithBudget: " <> limit <> " limit reached")


-- | Fork a thread that checks with the given frequency if any async handles stored in the
-- state are stale (ie., have terminated with or without exception, but not been removed).  If
-- that happens, log a warning.
--
-- 'runWithBudget' should keep track of the state itself; 'watchThreadBudgetState' is solely a
-- safety precaution to see if there aren't any corner cases we missed.
--
-- Also, issue some metrics.
watchThreadBudgetState
  :: forall m. (MonadIO m, LC.MonadLogger m, MonadCatch m)
  => Metrics -> ThreadBudgetState -> NominalDiffTime -> m ()
watchThreadBudgetState metrics (ThreadBudgetState limits ref) freq = safeForever $ do
  recordMetrics metrics limits ref
  removeStaleHandles ref
  threadDelayNominalDiffTime freq

recordMetrics
  :: forall m. (MonadIO m, LC.MonadLogger m, MonadCatch m)
  => Metrics -> MaxConcurrentNativePushes -> IORef BudgetMap -> m ()
recordMetrics metrics limits ref = do
  spent <- SHM.size <$> readIORef ref
  gaugeSet (fromIntegral spent) (path "net.nativepush.thread_budget_allocated") metrics
  forM_ (limits ^. limitHard) $ \lim ->
    gaugeSet (fromIntegral lim) (path "net.nativepush.thread_budget_hard_limit") metrics
  forM_ (limits ^. limitSoft) $ \lim ->
    gaugeSet (fromIntegral lim) (path "net.nativepush.thread_budget_soft_limit") metrics

threadDelayNominalDiffTime :: NominalDiffTime -> MonadIO m => m ()
threadDelayNominalDiffTime = threadDelay . round . (* 1000000) . toRational

staleTolerance :: NominalDiffTime
staleTolerance = 3

-- | Get all handles for asyncs that have terminated, but not been removed from the state.  Do
-- that again after 'staleTolerance' to make sure that we don't catch any handles that would
-- have been removed during the 'runWithBudget' cleanup, but we were faster.  The intersection
-- between the two rounds constitutes the legitimately stale handles: warn about them, and
-- then remove them from the budgetmap.
removeStaleHandles
  :: forall m. (MonadIO m, LC.MonadLogger m, MonadCatch m)
  => IORef BudgetMap -> m ()
removeStaleHandles ref = do
  round1 <- getStaleHandles
  threadDelayNominalDiffTime staleTolerance
  round2 <- getStaleHandles

  let staleHandles = Set.intersection round1 round2

  unless (null staleHandles) $ do
    warnStaleHandles (Set.size staleHandles) =<< readIORef ref
    forM_ staleHandles $ \key -> do
      mapM_ waitCatch . join =<< SHM.lookup key <$> readIORef ref
      unregister ref key

  where
    getStaleHandles :: m (Set UUID)
    getStaleHandles = Set.fromList . mconcat <$> do
      handles <- SHM.toList <$> readIORef ref
      forM handles $ \case
        (_, Nothing) -> do
          pure []
        (key, Just handle) -> do
          status <- poll handle
          pure [key | isJust status]

    warnStaleHandles :: Int -> BudgetMap -> m ()
    warnStaleHandles num handles = LC.warn $
      "BudgetMap" LC..= show (SHM.size handles)
      LC.~~ LC.msg ("watchThreadBudgetState: removed " <> show num <> " stale handles.")

safeForever
  :: forall m. (MonadIO m, LC.MonadLogger m, MonadCatch m)
  => m () -> m ()
safeForever action = forever $ action `catchAny` \exc -> do
  LC.err $ "error" LC..= show exc LC.~~ LC.msg (LC.val "watchThreadBudgetState: crashed; retrying")
  threadDelay 60000000  -- pause to keep worst-case noise in logs manageable
