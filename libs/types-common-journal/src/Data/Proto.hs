module Data.Proto where

import Control.Monad.IO.Class
import Data.Int
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX

now :: MonadIO m => m Int64
now = liftIO $ round . utcTimeToPOSIXSeconds <$> getCurrentTime
