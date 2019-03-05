{-# LANGUAGE OverloadedStrings #-}

module Data.Metrics.GC (toJson) where

import Imports
import Data.Aeson
import GHC.Stats

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
