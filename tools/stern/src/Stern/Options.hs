{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Stern.Options where

import Data.Yaml (FromJSON(..))
import GHC.Generics
import Imports
import Util.Options
import System.Logger (Level)

-- | Options that are consumed on startup
data Opts = Opts
    { stern   :: !Endpoint
    , brig    :: !Endpoint
    , galley  :: !Endpoint
    , gundeck :: !Endpoint
    -- TODO: Both ibis and galeb should be made optional
    --       for installations where these services are not available
    , ibis    :: !Endpoint
    , galeb   :: !Endpoint
    -- Logging
    , logLevel      :: !Level
    , logNetStrings :: !Bool
    } deriving (Show, Generic)

instance FromJSON Opts
