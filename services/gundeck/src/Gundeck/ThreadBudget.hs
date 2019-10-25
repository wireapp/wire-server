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
  ) where

import Gundeck.ThreadBudget.Internal
