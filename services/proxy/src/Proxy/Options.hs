{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Proxy.Options
    ( Opts
    , host
    , port
    , secretsConfig
    , httpPoolSize
    , maxConns
    ) where

import Imports
import Control.Lens
import Data.Aeson
import Data.Aeson.TH

data Opts = Opts
    { _host          :: !String        -- ^ Host to listen on
    , _port          :: !Word16        -- ^ Post to listen on
    , _secretsConfig :: !FilePath      -- ^ File containing upstream secrets
    , _httpPoolSize  :: !Int           -- ^ Number of connections for the HTTP pool
    , _maxConns      :: !Int           -- ^ Maximum number of incoming connections
    } deriving (Show, Generic)

makeLenses ''Opts

deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Opts
