module FanInPerf.Settle (waitUntilStable) where

import GHC.Clock (getMonotonicTime)
import Imports

-- | @waitUntilStable stableFor timeout pollInterval read@ polls @read@ (every
-- @pollInterval@ seconds) until it returned the same value for @stableFor@
-- seconds: @Just value@. 'Nothing' if that did not happen within @timeout@ seconds.
waitUntilStable :: (Eq a) => Double -> Double -> Double -> IO a -> IO (Maybe a)
waitUntilStable stableFor timeout pollInterval readValue = do
  started <- getMonotonicTime
  let go lastValue since = do
        value <- readValue
        now <- getMonotonicTime
        let since' = if Just value == lastValue then since else now
        if
          | now - since' >= stableFor -> pure (Just value)
          | now - started >= timeout -> pure Nothing
          | otherwise -> threadDelay (round (pollInterval * 1_000_000)) >> go (Just value) since'
  go Nothing started
