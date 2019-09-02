module Proxy.Options
    ( Opts
    , host
    , port
    , secretsConfig
    , httpPoolSize
    , maxConns
    , logLevel
    , logNetStrings
    , logFormat
    , mockOpts
    ) where

import Imports
import Control.Lens hiding (Level)
import Data.Aeson
import Data.Aeson.TH
import System.Logger.Extended (Level(Debug), LogFormat)

data Opts = Opts
    { _host          :: !String     -- ^ Host to listen on
    , _port          :: !Word16     -- ^ Port to listen on
    , _secretsConfig :: !FilePath   -- ^ File containing upstream secrets
    , _httpPoolSize  :: !Int        -- ^ Number of connections for the HTTP pool
    , _maxConns      :: !Int        -- ^ Maximum number of incoming connections
    -- Logging
    , _logLevel      :: !Level       -- ^ Log level (Debug, Info, etc)
    , _logNetStrings :: !(Maybe (Last Bool)) -- ^ Use netstrings encoding
    , _logFormat     :: !(Maybe (Last LogFormat))-- ^ choose Encoding
    } deriving (Show, Generic)

makeLenses ''Opts

deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Opts

-- | for testing.
mockOpts :: FilePath -> Opts
mockOpts secrets = Opts
    { _host          = mempty
    , _port          = 0
    , _secretsConfig = secrets
    , _httpPoolSize  = 0
    , _maxConns      = 0
    , _logLevel      = Debug
    , _logNetStrings = pure $ pure $ True
    , _logFormat     = mempty
    }
