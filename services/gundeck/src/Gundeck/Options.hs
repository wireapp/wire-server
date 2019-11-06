{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gundeck.Options where

import Imports
import Control.Lens hiding (Level)
import Data.Aeson.TH
import Data.Yaml (FromJSON)
import Gundeck.Aws.Arn
import System.Logger.Extended (Level, LogFormat)
import Util.Options
import Util.Options.Common

newtype NotificationTTL = NotificationTTL
    { notificationTTLSeconds :: Word32 }
    deriving (Eq, Ord, Show, Generic, FromJSON)

data AWSOpts = AWSOpts
    { _awsAccount     :: !Account       -- ^ AWS account
    , _awsRegion      :: !Region        -- ^ AWS region name
    , _awsArnEnv      :: !ArnEnv        -- ^ Environment name to scope ARNs to
    , _awsQueueName   :: !Text          -- ^ SQS queue name
    , _awsSqsEndpoint :: !AWSEndpoint
    , _awsSnsEndpoint :: !AWSEndpoint
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''AWSOpts
makeLenses ''AWSOpts

data Settings = Settings
    {
    -- | Number of connections to keep open in the http-client pool
      _setHttpPoolSize    :: !Int
    -- | TTL (seconds) of stored notifications
    , _setNotificationTTL :: !NotificationTTL
    -- | Use this option to group push notifications and send them in bulk to Cannon, instead
    -- of in individual requests
    , _setBulkPush        :: !Bool
    -- | Maximum number of concurrent threads calling SNS.
    , _setMaxConcurrentNativePushes :: !(Maybe MaxConcurrentNativePushes)
    -- | Maximum number of parallel requests to SNS and cassandra
    -- during native push processing (per incoming push request)
    -- defaults to unbounded, if unset.
    , _setPerNativePushConcurrency  :: !(Maybe Int)
    } deriving (Show, Generic)

data MaxConcurrentNativePushes = MaxConcurrentNativePushes
    { _limitHard :: !(Maybe Int)  -- ^ more than this number of threads will not be allowed
    , _limitSoft :: !(Maybe Int)  -- ^ more than this number of threads will be warned about
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Settings
makeLenses ''Settings

deriveFromJSON toOptionFieldName ''MaxConcurrentNativePushes
makeLenses ''MaxConcurrentNativePushes

data Opts = Opts
    { _optGundeck   :: !Endpoint       -- ^ Hostname and port to bind to
    , _optCassandra :: !CassandraOpts
    , _optRedis     :: !Endpoint
    , _optAws       :: !AWSOpts
    , _optDiscoUrl  :: !(Maybe Text)
    , _optSettings  :: !Settings
    -- Logging
    , _optLogLevel      :: !Level       -- ^ Log level (Debug, Info, etc)
    , _optLogNetStrings :: !(Maybe (Last Bool))        -- ^ Use netstrings encoding:
                                        --   <http://cr.yp.to/proto/netstrings.txt>
    , _optLogFormat :: !(Maybe (Last LogFormat))
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Opts
makeLenses ''Opts
