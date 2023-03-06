{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Federator.Env where

import Bilge (RequestId)
import Control.Lens (makeLenses)
import Data.Metrics (Metrics)
import Federator.Options (RunSettings)
import Imports
import Network.DNS.Resolver (Resolver)
import qualified Network.HTTP.Client as HTTP
import OpenSSL.Session (SSLContext)
import qualified System.Logger.Class as LC
import Util.Options
import Wire.API.Federation.Component

data Env = Env
  { _metrics :: Metrics,
    _applog :: LC.Logger,
    _requestId :: RequestId,
    _dnsResolver :: Resolver,
    _runSettings :: RunSettings,
    _service :: Component -> Endpoint,
    _httpManager :: HTTP.Manager,
    _sslContext :: IORef SSLContext
  }

makeLenses ''Env
