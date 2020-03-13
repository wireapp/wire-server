{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Federator.Options where

import Data.Aeson
import Imports
import System.Logger.Extended
import Util.Options

data Opts
  = Opts
      { -- | Host and port
        federator :: Endpoint,
        -- | Log level (Debug, Info, etc)
        logLevel :: Level,
        -- | Use netstrings encoding (see <http://cr.yp.to/proto/netstrings.txt>)
        logNetStrings :: Maybe (Last Bool),
        -- | Logformat to use
        logFormat :: !(Maybe (Last LogFormat))
      }
  deriving (Show, Generic)

instance FromJSON Opts
