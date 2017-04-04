{-# LANGUAGE OverloadedStrings #-}

module Data.Metrics.GC (toJson) where

import Data.Aeson
import GHC.Stats

toJson :: IO (Maybe Value)
toJson = do
    enabled <- getGCStatsEnabled
    if enabled then Just <$> getStats else return Nothing
  where
    getStats = do
        gc <- getGCStats
        return $ object
            [ "gc.bytes.allocated.total" .= bytesAllocated gc
            , "gc.bytes.used.max"        .= maxBytesUsed gc
            , "gc.bytes.used.current"    .= currentBytesUsed gc
            , "gc.seconds.cpu"           .= gcCpuSeconds gc
            , "gc.seconds.wall"          .= gcWallSeconds gc
            ]
