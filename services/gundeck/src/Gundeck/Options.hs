{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Gundeck.Options where

import Imports
import Control.Lens
import Data.Aeson.TH
import Data.Yaml (FromJSON)
import Gundeck.Aws.Arn
import Util.Options
import Util.Options.Common

newtype NotificationTTL = NotificationTTL
    { notificationTTLSeconds :: Word32 }
    deriving (Eq, Ord, Show, Generic, FromJSON)

-- | AWS options
data AWSOpts = AWSOpts
    { _awsAccount     :: !Account      -- ^ Account
    , _awsRegion      :: !Region       -- ^ Region
    , _awsArnEnv      :: !ArnEnv       -- ^ Environment name to scope ARNs to
    , _awsQueueName   :: !Text         -- ^ SQS event queue name
    , _awsSqsEndpoint :: !AWSEndpoint  -- ^ SQS endpoint
    , _awsSnsEndpoint :: !AWSEndpoint  -- ^ SNS endpoint
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''AWSOpts
makeLenses ''AWSOpts

data FallbackOpts = FallbackOpts
    { _fbSkipFallbacks :: !Bool    -- ^ Never send delayed fallback notifications
    , _fbPreferNotice  :: !Bool    -- ^ Send notifications of type @notice@
    , _fbQueueDelay    :: !Word64  -- ^ Delay of notifications before sending a
                                   --   fallback, in seconds. MUST be 30s or higher
    , _fbQueueLimit    :: !Int     -- ^ Max size of the notification fallback queue
    , _fbQueueBurst    :: !Word16  -- ^ Max number of delayed notifications
                                   --   to fire in a row, per second
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''FallbackOpts
makeLenses ''FallbackOpts

data Settings = Settings
    { _setHttpPoolSize    :: !Int              -- ^ Number of connections
                                               --   for the HTTP client pool
    , _setNotificationTTL :: !NotificationTTL  -- ^ TTL for stored notifications,
                                               --   in seconds
    , _setBulkPush        :: !Bool             -- ^ Group push notifications and
                                               --   send them to Cannon in bulk,
                                               --   instead of doing individual
                                               --   requests
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Settings
makeLenses ''Settings

data Opts = Opts
    { _optGundeck   :: !Endpoint        -- ^ Hostname and port to listen on
    , _optCassandra :: !CassandraOpts   -- ^ Cassandra settings
    , _optRedis     :: !Endpoint        -- ^ Redis endpoint
    , _optAws       :: !AWSOpts         -- ^ AWS settings
    , _optDiscoUrl  :: !(Maybe Text)    -- ^ Disco URL (klabautermann)
    , _optFallback  :: !FallbackOpts    -- ^ Settings for fallback notifications
    , _optSettings  :: !Settings        -- ^ Other settings
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Opts
makeLenses ''Opts
