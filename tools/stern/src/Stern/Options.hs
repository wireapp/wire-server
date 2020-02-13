{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Stern.Options where

import Data.Yaml (FromJSON (..))
import GHC.Generics
import Imports
import System.Logger.Extended (Level, LogFormat)
import Util.Options

-- | Options that are consumed on startup
data Opts
  = Opts
      { stern :: !Endpoint,
        brig :: !Endpoint,
        galley :: !Endpoint,
        gundeck :: !Endpoint,
        -- TODO: Both ibis and galeb should be made optional
        --       for installations where these services are not available
        ibis :: !Endpoint,
        galeb :: !Endpoint,
        -- Logging
        logLevel :: !Level,
        logNetStrings :: !(Maybe (Last Bool)),
        logFormat :: !(Maybe (Last LogFormat))
      }
  deriving (Show, Generic)

instance FromJSON Opts
