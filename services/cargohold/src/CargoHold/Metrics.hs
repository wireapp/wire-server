-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module CargoHold.Metrics where

import Imports
import qualified Prometheus as Prom

s3UploadOk :: Prom.MonadMonitor m => m ()
s3UploadOk = Prom.incCounter netS3UploadOk

{-# NOINLINE netS3UploadOk #-}
netS3UploadOk :: Prom.Counter
netS3UploadOk =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "net.s3.upload_ok",
          Prom.metricHelp = "Number of successful S3 Uploads"
        }

s3UploadSize :: (Prom.MonadMonitor m, Integral n) => n -> m ()
s3UploadSize n =
  void $ Prom.addCounter netS3UploadSize (fromIntegral n)

{-# NOINLINE netS3UploadSize #-}
netS3UploadSize :: Prom.Counter
netS3UploadSize =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "net.s3.upload_size",
          Prom.metricHelp = "Number of bytes uploaded successfully uploaded to S3"
        }
