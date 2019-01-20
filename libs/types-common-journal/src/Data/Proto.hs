module Data.Proto where

import Imports
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX

now :: MonadIO m => m Int64
now = liftIO $ round . utcTimeToPOSIXSeconds <$> getCurrentTime
