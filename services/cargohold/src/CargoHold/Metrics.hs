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
