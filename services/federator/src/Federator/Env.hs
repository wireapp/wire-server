{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

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

module Federator.Env where

import Bilge (RequestId)
import qualified Bilge as RPC
import Control.Lens (makeLenses)
import Data.Metrics (Metrics)
import Data.X509.CertificateStore
import Federator.Options (RunSettings)
import Network.DNS.Resolver (Resolver)
import qualified Network.HTTP.Client as HTTP
import qualified Network.TLS as TLS
import qualified System.Logger.Class as LC
import Wire.API.Federation.GRPC.Types

data TLSSettings = TLSSettings
  { _caStore :: CertificateStore,
    _creds :: TLS.Credential
  }

data Env = Env
  { _metrics :: Metrics,
    _applog :: LC.Logger,
    _requestId :: RequestId,
    _dnsResolver :: Resolver,
    _runSettings :: RunSettings,
    _service :: Component -> RPC.Request,
    _httpManager :: HTTP.Manager,
    _tls :: TLSSettings
  }

makeLenses ''TLSSettings
makeLenses ''Env
