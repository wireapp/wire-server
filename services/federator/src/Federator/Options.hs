{-# LANGUAGE StrictData #-}

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

module Federator.Options where

import Data.Aeson
import Imports
import System.Logger.Extended (Level, LogFormat)
import Util.Options

-- | Options that persist as runtime settings.
data RunSettings = RunSettings
  { -- | Would you like to federate with everyone or only with a select set of other wire-server installations?
    useSystemCAStore :: Bool,
    remoteCAStore :: Maybe FilePath,
    clientCertificate :: FilePath,
    clientPrivateKey :: FilePath,
    -- | Timeout for making TCP connections (for http2) with remote federators
    -- and local components. In microseconds.
    tcpConnectionTimeout :: Int,
    dnsHost :: Maybe String,
    dnsPort :: Maybe Word16
  }
  deriving (Eq, Show, Generic)

instance FromJSON RunSettings

data Opts = Opts
  { -- | Host and port for endpoint reachable only by other wire-server
    -- components in the same private network
    federatorInternal :: Endpoint,
    -- | Host and port for endpoint exposed to the open internet via nginx, to
    -- be contacted by other federators
    federatorExternal :: Endpoint,
    -- | Host and port of brig
    brig :: Endpoint,
    -- | Host and port of galley
    galley :: Endpoint,
    -- | Host and port of cargohold
    cargohold :: Endpoint,
    -- | Log level (Debug, Info, etc)
    logLevel :: Level,
    -- | Use netstrings encoding (see <http://cr.yp.to/proto/netstrings.txt>)
    logNetStrings :: Maybe (Last Bool),
    -- | Logformat to use
    logFormat :: !(Maybe (Last LogFormat)),
    -- | Runtime settings
    optSettings :: !RunSettings
  }
  deriving (Show, Generic)

instance FromJSON Opts
