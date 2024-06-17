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

import Control.Lens (makeLenses)
import Federator.Options (RunSettings)
import HTTP2.Client.Manager
import Imports
import Network.DNS.Resolver (Resolver)
import Network.HTTP.Client qualified as HTTP
import OpenSSL.Session (SSLContext)
import Prometheus
import System.Logger.Class qualified as LC
import Util.Options
import Wire.API.Federation.Component

data FederatorMetrics = FederatorMetrics
  { outgoingRequests :: Vector Text Counter,
    incomingRequests :: Vector Text Counter
  }

data Env = Env
  { _applog :: LC.Logger,
    _dnsResolver :: Resolver,
    _runSettings :: RunSettings,
    _service :: Component -> Endpoint,
    _externalPort :: Word16,
    _internalPort :: Word16,
    _httpManager :: HTTP.Manager,
    _http2Manager :: IORef Http2Manager,
    _federatorMetrics :: FederatorMetrics
  }

makeLenses ''Env

onNewSSLContext :: Env -> SSLContext -> IO ()
onNewSSLContext env ctx =
  atomicModifyIORef' (_http2Manager env) $ \mgr -> (setSSLContext ctx mgr, ())

mkHttp2Manager :: Int -> SSLContext -> IO Http2Manager
mkHttp2Manager tcpConnectionTimeout sslContext =
  setTCPConnectionTimeout tcpConnectionTimeout
    . setSSLRemoveTrailingDot True
    <$> http2ManagerWithSSLCtx sslContext
