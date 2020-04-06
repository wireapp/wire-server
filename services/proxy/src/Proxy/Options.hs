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

module Proxy.Options
  ( Opts,
    host,
    port,
    secretsConfig,
    httpPoolSize,
    maxConns,
    logLevel,
    logNetStrings,
    logFormat,
    mockOpts,
  )
where

import Control.Lens hiding (Level)
import Data.Aeson
import Data.Aeson.TH
import Imports
import System.Logger.Extended (Level (Debug), LogFormat)

data Opts
  = Opts
      { -- | Host to listen on
        _host :: !String,
        -- | Port to listen on
        _port :: !Word16,
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
        _logFormat :: !(Maybe (Last LogFormat))
      }
  deriving (Show, Generic)

makeLenses ''Opts

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Opts

-- | for testing.
mockOpts :: FilePath -> Opts
mockOpts secrets =
  Opts
    { _host = mempty,
      _port = 0,
      _secretsConfig = secrets,
      _httpPoolSize = 0,
      _maxConns = 0,
      _logLevel = Debug,
      _logNetStrings = pure $ pure $ True,
      _logFormat = mempty
    }
