{-# LANGUAGE OverloadedStrings #-}

module Data.Metrics.GC (toJson, spawnGCMetricsCollector) where

import Imports
import Control.Immortal
import Data.Aeson
import GHC.Stats
import Prometheus

secondInMicroseconds :: Int
secondInMicroseconds = 1000000

-- | Spawn an unkillable daemon thread which reports runtime memory statistics to Prometheus
spawnGCMetricsCollector :: IO ()
spawnGCMetricsCollector = whenM getRTSStatsEnabled . void $ do
    allocatedBytesGauge <- register
        $ gauge (Info "gc_bytes_allocated_total" "Bytes allocated since the start of the server")
    bytesUsedMaxGauge <- register
        $ gauge (Info "gc_bytes_used_max" "Maximum amount of memory living on the heap after the last major GC")
    bytesUsedCurrentGauge <- register
        $ gauge (Info "gc_bytes_used_current" "Number of bytes in active use as of the last GC")
    gcSecondsCPUGauge <- register
        $ gauge (Info "gc_seconds_cpu" "CPU time spent on last GC")
    gcSecondsWallGauge <- register
        $ gauge (Info "gc_seconds_wall" "Wall clock time spent on last GC")

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

toJson :: IO (Maybe Value)
toJson = do
    enabled <- getRTSStatsEnabled
    if enabled then Just <$> getStats else return Nothing
  where
    getStats = do
        rts <- getRTSStats
        return $ object
            [ "gc.bytes.allocated.total" .= allocated_bytes rts
            , "gc.bytes.used.max"        .= max_live_bytes rts
            , "gc.bytes.used.current"    .= gcdetails_live_bytes (gc rts)
            , "gc.seconds.cpu"           .= gcdetails_cpu_ns (gc rts)
            , "gc.seconds.wall"          .= gcdetails_elapsed_ns (gc rts)
            ]
