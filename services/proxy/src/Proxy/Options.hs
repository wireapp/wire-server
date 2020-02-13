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
