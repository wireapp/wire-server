module Data.Proto where

import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX
import Imports

now :: MonadIO m => m Int64
now = liftIO $ round . utcTimeToPOSIXSeconds <$> getCurrentTime
