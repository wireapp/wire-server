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

module Proxy.Options
  ( Opts,
    proxy,
    giphyEndpoint,
    secretsConfig,
    httpPoolSize,
    maxConns,
    logLevel,
    logNetStrings,
    logFormat,
    disabledAPIVersions,
    disableTlsForTest,
  )
where

import Cassandra.Options
import Control.Lens hiding (Level)
import Data.Aeson
import Data.Aeson.TH
import Imports
import System.Logger.Extended (Level, LogFormat)
import Wire.API.Routes.Version

data Opts = Opts
  { _proxy :: !Endpoint,
    _giphyEndpoint :: !Endpoint,
    -- | File containing upstream secrets
    _secretsConfig :: !FilePath,
    -- | Number of connections for the HTTP pool
    _httpPoolSize :: !Int,
    -- | Maximum number of incoming connections
    -- Logging
    _maxConns :: !Int,
    -- | Log level (Debug, Info, etc)
    _logLevel :: !Level,
    -- | Use netstrings encoding
    _logNetStrings :: !(Maybe (Last Bool)),
    -- | choose Encoding
    _logFormat :: !(Maybe (Last LogFormat)),
    _disabledAPIVersions :: !(Set VersionExp),
    _disableTlsForTest :: Maybe Bool
  }
  deriving (Show, Generic)

makeLenses ''Opts

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Opts
