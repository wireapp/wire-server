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

{-# LANGUAGE OverloadedStrings #-}

module Data.Metrics.GC
  ( spawnGCMetricsCollector,
  )
where

import Control.Immortal
import GHC.Stats
import Imports
import Prometheus

secondInMicroseconds :: Int
secondInMicroseconds = 1000000

-- | Spawn an unkillable daemon thread which reports runtime memory statistics to Prometheus
spawnGCMetricsCollector :: IO ()
spawnGCMetricsCollector = whenM getRTSStatsEnabled . void $ do
  allocatedBytesGauge <-
    register $
      gauge (Info "gc_bytes_allocated_total" "Bytes allocated since the start of the server")
  bytesUsedMaxGauge <-
    register $
      gauge (Info "gc_bytes_used_max" "Maximum amount of memory living on the heap after the last major GC")
  bytesUsedCurrentGauge <-
    register $
      gauge (Info "gc_bytes_used_current" "Number of bytes in active use as of the last GC")
  gcSecondsCPUGauge <-
    register $
      gauge (Info "gc_seconds_cpu" "CPU time spent on last GC")
  gcSecondsWallGauge <-
    register $
      gauge (Info "gc_seconds_wall" "Wall clock time spent on last GC")
  -- Spawn a daemon thread that will continually report GC stats to prometheus
  -- It will restart if it ever crashes.
  create $ \_threadId -> forever $ do
    threadDelay $ 60 * secondInMicroseconds
    rts <- getRTSStats
    setGauge allocatedBytesGauge . fromIntegral $ allocated_bytes rts
    setGauge bytesUsedMaxGauge . fromIntegral $ max_live_bytes rts
    setGauge bytesUsedCurrentGauge . fromIntegral $ gcdetails_live_bytes (gc rts)
    setGauge gcSecondsCPUGauge . fromIntegral $ gcdetails_cpu_ns (gc rts)
    setGauge gcSecondsWallGauge . fromIntegral $ gcdetails_elapsed_ns (gc rts)
