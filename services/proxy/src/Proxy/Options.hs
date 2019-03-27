module Proxy.Options
    ( Opts
    , host
    , port
    , secretsConfig
    , httpPoolSize
    , maxConns
    , logLevel
    , logNetStrings
    ) where

import Imports
import Control.Lens hiding (Level)
import Data.Aeson
import Data.Aeson.TH
import System.Logger (Level)

data Opts = Opts
    { _host          :: !String     -- ^ Host to listen on
    , _port          :: !Word16     -- ^ Port to listen on
    , _secretsConfig :: !FilePath   -- ^ File containing upstream secrets
    , _httpPoolSize  :: !Int        -- ^ Number of connections for the HTTP pool
    , _maxConns      :: !Int        -- ^ Maximum number of incoming connections
    -- Logging
    , _logLevel      :: !Level       -- ^ Log level (Debug, Info, etc)
    , _logNetStrings :: !Bool        -- ^ Use netstrings encoding (see
                                     --   <http://cr.yp.to/proto/netstrings.txt>)
    } deriving (Show, Generic)

makeLenses ''Opts

deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Opts
