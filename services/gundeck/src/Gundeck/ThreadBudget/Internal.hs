-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Gundeck.ThreadBudget.Internal where

import Control.Exception.Safe (catchAny)
import Control.Lens
import Control.Monad.Catch (MonadCatch)
import qualified Data.HashMap.Strict as HM
import Data.Metrics (Metrics, counterIncr)
import Data.Metrics.Middleware (gaugeSet, path)
import qualified Data.Set as Set
import Data.Time
import Data.UUID (UUID, toText)
import Data.UUID.V4 (nextRandom)
import Gundeck.Options
import Imports
import qualified System.Logger.Class as LC
import UnliftIO.Async
import UnliftIO.Exception (finally)

data ThreadBudgetState = ThreadBudgetState
  { threadBudgetLimits :: MaxConcurrentNativePushes,
    _threadBudgetRunning :: IORef BudgetMap
  }
  deriving (Generic)

-- | Store all handles for cleanup in 'watchThreadBudgetState'.
data BudgetMap = BudgetMap
  { bspent :: Int,
    bmap :: HashMap UUID (Int, Maybe (Async ()))
  }
  deriving (Eq, Generic)

-- | Instead of taking the pre-computed total budget spent of the 'BudgetMap' (O(1)), this
-- counts all the threads that are successfully running (dropping the ones that are just about
-- to try to grab a token).
--
-- WARNING: takes O(n)), use with care.  See 'bench_BudgetSpent''.
budgetSpent :: ThreadBudgetState -> IO Int
budgetSpent (ThreadBudgetState _ running) = budgetSpent' <$> readIORef running

budgetSpent' :: BudgetMap -> Int
budgetSpent' = sum . fmap fst . filter (isJust . snd) . HM.elems . bmap

cancelAllThreads :: ThreadBudgetState -> IO ()
cancelAllThreads (ThreadBudgetState _ ref) =
  readIORef ref
    >>= mapM_ cancel . catMaybes . fmap snd . HM.elems . bmap

mkThreadBudgetState :: HasCallStack => MaxConcurrentNativePushes -> IO ThreadBudgetState
mkThreadBudgetState limits = ThreadBudgetState limits <$> newIORef (BudgetMap 0 HM.empty)

-- | Allocate the resources for a new action to be called (but don't call the action yet).
allocate ::
  IORef BudgetMap -> UUID -> Int -> MonadIO m => m Int
allocate ref key newspent =
  atomicModifyIORef' ref $
    \(BudgetMap spent hm) ->
      ( BudgetMap (spent + newspent) (HM.insert key (newspent, Nothing) hm),
        spent
      )

-- | Register an already-allocated action with its 'Async'.
register ::
  IORef BudgetMap -> UUID -> Async () -> MonadIO m => m Int
register ref key handle =
  atomicModifyIORef' ref $
    \(BudgetMap spent hm) ->
      ( BudgetMap spent (HM.adjust (_2 .~ Just handle) key hm),
        spent
      )

-- | Remove an registered and/or allocated action from a 'BudgetMap'.
unregister ::
  IORef BudgetMap -> UUID -> MonadIO m => m ()
unregister ref key =
  atomicModifyIORef' ref $
    \bhm@(BudgetMap spent hm) ->
      case HM.lookup key hm of
        Just (newspent, _) -> (BudgetMap (spent - newspent) (HM.delete key hm), ())
        Nothing -> (bhm, ())

-- | If there is budget available, execute the action synchronously; otherwise, log a warning
-- and return immediately.  Make sure the budget state is updated accordingly both when
-- starting and ending the execution.
--
-- The hard limit in the 'ThreadBudgetState' argument is guaranteed to be an upper bound for
-- the number of concurrently spent budget tokens; surpassing the soft limit will trigger a
-- warning, but still execute the action.  One action can use up any integer number of budget
-- tokens, including 0 and negative.
--
-- The action is called in an 'Async', but 'runWithBudget' waits for it to finish so it can
-- update the budget.
runWithBudget ::
  forall m.
  (MonadIO m, LC.MonadLogger m, MonadUnliftIO m) =>
  Metrics ->
  ThreadBudgetState ->
  Int ->
  m () ->
  m ()
runWithBudget metrics tbs spent = runWithBudget' metrics tbs spent ()

-- | More flexible variant of 'runWithBudget' that allows the action to return a value.  With
-- a default in case of budget exhaustion.
runWithBudget' ::
  forall m a.
  (MonadIO m, LC.MonadLogger m, MonadUnliftIO m) =>
  Metrics ->
  ThreadBudgetState ->
  Int ->
  a ->
  m a ->
  m a
runWithBudget' metrics (ThreadBudgetState limits ref) spent fallback action = do
  key <- liftIO nextRandom
  (`finally` unregister ref key) $ do
    oldsize <- allocate ref key spent
    let softLimitBreached = maybe False (oldsize >=) (limits ^. limitSoft)
        hardLimitBreached = maybe False (oldsize >=) (limits ^. limitHard)
    warnNoBudget softLimitBreached hardLimitBreached oldsize
    if (maybe True (oldsize <) (limits ^. limitHard))
      then go key oldsize
      else pure fallback
  where
    go :: UUID -> Int -> m a
    go key oldsize = do
      LC.debug $
        "key" LC..= (toText key)
          LC.~~ "spent" LC..= oldsize
          LC.~~ LC.msg (LC.val "runWithBudget: go")
      handle <- async action
      _ <- register ref key (const () <$> handle)
      wait handle
    -- iff soft and/or hard limit are breached, log a warning-level message.
    warnNoBudget :: Bool -> Bool -> Int -> m ()
    warnNoBudget False False _ = pure ()
    warnNoBudget soft hard oldsize = do
      let limit = if hard then "hard" else "soft"
          metric = "net.nativepush." <> limit <> "_limit_breached"
      counterIncr (path metric) metrics
      LC.warn $
        "spent" LC..= show oldsize
          LC.~~ "soft-breach" LC..= soft
          LC.~~ "hard-breach" LC..= hard
          LC.~~ LC.msg ("runWithBudget: " <> limit <> " limit reached")

-- | Fork a thread that checks with the given frequency if any async handles stored in the
-- state are stale (ie., have terminated with or without exception, but not been removed).  If
-- that happens, log a warning.
--
-- 'runWithBudget' should keep track of the state itself; 'watchThreadBudgetState' is solely a
-- safety precaution to see if there aren't any corner cases we missed.
--
-- Also, issue some metrics.
watchThreadBudgetState ::
  forall m.
  (MonadIO m, LC.MonadLogger m, MonadCatch m) =>
  Metrics ->
  ThreadBudgetState ->
  NominalDiffTime ->
  m ()
watchThreadBudgetState metrics (ThreadBudgetState limits ref) freq = safeForever $ do
  recordMetrics metrics limits ref
  removeStaleHandles ref
  threadDelayNominalDiffTime freq

recordMetrics ::
  forall m.
  (MonadIO m, LC.MonadLogger m, MonadCatch m) =>
  Metrics ->
  MaxConcurrentNativePushes ->
  IORef BudgetMap ->
  m ()
recordMetrics metrics limits ref = do
  (BudgetMap spent _) <- readIORef ref
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
removeStaleHandles ::
  forall m.
  (MonadIO m, LC.MonadLogger m, MonadCatch m) =>
  IORef BudgetMap ->
  m ()
removeStaleHandles ref = do
  round1 <- getStaleHandles
  threadDelayNominalDiffTime staleTolerance
  round2 <- getStaleHandles
  let staleHandles = Set.intersection round1 round2
  unless (null staleHandles) $ do
    warnStaleHandles (Set.size staleHandles) =<< readIORef ref
    forM_ staleHandles $ \key -> do
      mapM_ waitCatch . join . fmap snd =<< HM.lookup key . bmap <$> readIORef ref
      unregister ref key
  isSanitary <- (\bm -> bspent bm == budgetSpent' bm) <$> readIORef ref
  unless isSanitary . LC.warn . LC.msg . LC.val $
    "watchThreadBudgetState: total overall thread budget diverged from async weights (repaired)."
  where
    getStaleHandles :: m (Set UUID)
    getStaleHandles =
      Set.fromList . mconcat <$> do
        handles <- HM.toList . bmap <$> readIORef ref
        forM handles $ \case
          (_, (_, Nothing)) -> do
            pure []
          (key, (_, Just handle)) -> do
            status <- poll handle
            pure [key | isJust status]
    warnStaleHandles :: Int -> BudgetMap -> m ()
    warnStaleHandles num (BudgetMap spent _) =
      LC.warn $
        "spent" LC..= show spent
          LC.~~ LC.msg ("watchThreadBudgetState: removed " <> show num <> " stale handles.")

safeForever ::
  forall m.
  (MonadIO m, LC.MonadLogger m, MonadCatch m) =>
  m () ->
  m ()
safeForever action =
  forever $
    action `catchAny` \exc -> do
      LC.err $ "error" LC..= show exc LC.~~ LC.msg (LC.val "watchThreadBudgetState: crashed; retrying")
      threadDelay 60000000 -- pause to keep worst-case noise in logs manageable
