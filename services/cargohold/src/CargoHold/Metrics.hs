module CargoHold.Metrics where

import CargoHold.App (Env, metrics)
import Control.Lens (view)
import Data.Metrics.Middleware (counterAdd, counterIncr, path)
import Imports

s3UploadOk :: (MonadReader Env m, MonadIO m) => m ()
s3UploadOk =
  counterIncr (path "net.s3.upload_ok")
    =<< view metrics

s3UploadSize :: (MonadReader Env m, MonadIO m, Integral n) => n -> m ()
s3UploadSize n =
  counterAdd (fromIntegral n) (path "net.s3.upload_size")
    =<< view metrics
