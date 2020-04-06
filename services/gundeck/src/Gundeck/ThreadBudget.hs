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

-- | Like "Brig.Budget", but in-memory, per host (not per service), and with an strict/exact
-- upper bound.  Like https://hackage.haskell.org/package/token-bucket, but takes the entire
-- run-time of the actions into account, not just the number of executions.
-- http://hackage.haskell.org/package/rate-limit also looks related.
-- https://github.com/juspay/fencer does what this module does, but as a networked service, so
-- in that way it works more like "Brig.Budget".
--
-- FUTUREWORK: https://github.com/layer-3-communications/lockpool seems like almost exactly
-- the same thing, but I only found this after ThreadBudget was done.  Before considering to
-- ThreadBudget a standalone package, take a closer look at lockpool!
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
  ( ThreadBudgetState,
    mkThreadBudgetState,
    runWithBudget,
    runWithBudget',
    watchThreadBudgetState,
  )
where

import Gundeck.ThreadBudget.Internal
