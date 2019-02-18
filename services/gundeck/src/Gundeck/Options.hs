{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gundeck.Options where

import Imports
import Control.Lens hiding (Level)
import Data.Aeson.TH
import Data.Yaml (FromJSON)
import Gundeck.Aws.Arn
import System.Logger (Level)
import Util.Options
import Util.Options.Common

newtype NotificationTTL = NotificationTTL
    { notificationTTLSeconds :: Word32 }
    deriving (Eq, Ord, Show, Generic, FromJSON)

data AWSOpts = AWSOpts
    { _awsAccount     :: !Account
    , _awsRegion      :: !Region
    , _awsArnEnv      :: !ArnEnv
    , _awsQueueName   :: !Text
    , _awsSqsEndpoint :: !AWSEndpoint
    , _awsSnsEndpoint :: !AWSEndpoint
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''AWSOpts
makeLenses ''AWSOpts

data Settings = Settings
    { _setHttpPoolSize    :: !Int
    , _setNotificationTTL :: !NotificationTTL
    , _setBulkPush        :: !Bool
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Settings
makeLenses ''Settings

data Opts = Opts
    { _optGundeck   :: !Endpoint
    , _optCassandra :: !CassandraOpts
    , _optRedis     :: !Endpoint
    , _optAws       :: !AWSOpts
    , _optDiscoUrl  :: !(Maybe Text)
    , _optSettings  :: !Settings
    -- Logging
    , _optLogLevel      :: !Level       -- ^ Log level (Debug, Info, etc)
    , _optLogNetStrings :: !Bool        -- ^ Use netstrings encoding:
                                        --   <http://cr.yp.to/proto/netstrings.txt>
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Opts
makeLenses ''Opts
