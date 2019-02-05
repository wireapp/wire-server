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
import Options.Applicative
import Options.Applicative.Types
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
    , _awsConnectionLimit :: !(Maybe Int)
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
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Opts
makeLenses ''Opts

parseOptions :: IO Opts
parseOptions = execParser (info (helper <*> optsParser) desc)
  where
    desc = header "Gundeck - Push Notifications" <> fullDesc

optsParser :: Parser Opts
optsParser = Opts <$>
    (Endpoint <$>
        (textOption $
            long "host"
            <> value "*4"
            <> showDefault
            <> metavar "HOSTNAME"
            <> help "Hostname or address to bind to")
        <*>
        (option auto $
            long "port"
            <> short 'p'
            <> metavar "PORT"
            <> help "Port to listen on"))
  <*> cassandraParser
  <*> redisParser
  <*> awsParser
  <*> optional discoUrlParser
  <*> settingsParser
  where
    redisParser :: Parser Endpoint
    redisParser = Endpoint <$>
        (textOption $
            long "redis-host"
            <> metavar "HOSTNAME"
            <> help "Redis hostname")
        <*>
        (option auto $
            long "redis-port"
            <> metavar "PORT"
            <> help "Redis port")

    awsParser :: Parser AWSOpts
    awsParser = AWSOpts <$>
        (fmap Account . textOption $
            long "aws-account"
            <> metavar "STRING"
            <> help "aws account")
        <*>
        (option parseRegion $
            long "aws-region"
            <> metavar "STRING"
            <> help "aws region name")
        <*>
        (fmap ArnEnv . textOption $
            long "aws-arn-env"
            <> metavar "STRING"
            <> help "environment name to scope ARNs to")
        <*>
        (textOption $
            long "event-queue-name"
            <> metavar "STRING"
            <> help "sqs queue name")
        <*>
        (option parseAWSEndpoint $
            long "aws-sqs-endpoint"
            <> metavar "STRING"
            <> value (AWSEndpoint "sqs.eu-west-1.amazonaws.com" True 443)
            <> showDefault
            <> help "aws SQS endpoint")
        <*>
        (option parseAWSEndpoint $
            long "aws-sns-endpoint"
            <> metavar "STRING"
            <> value (AWSEndpoint "sns.eu-west-1.amazonaws.com" True 443)
            <> showDefault
            <> help "aws SNS endpoint")
        <*>
        (optional $ option auto $
            long "aws-connection-limit"
            <> metavar "SIZE"
            <> help "maximum number of simultaneous connections to AWS")

    settingsParser :: Parser Settings
    settingsParser = Settings <$>
        (option auto $
            long "http-pool-size"
            <> metavar "SIZE"
            <> showDefault
            <> help "number of connections for the http client pool"
            <> value 128)
        <*>
        (fmap NotificationTTL . option auto $
            long "notification-ttl"
            <> metavar "SIZE"
            <> showDefault
            <> help "TTL (seconds) of stored notifications"
            <> value 86400)
        <*>
        (switch $
            long "bulk-push"
            <> help ("Use this option to group push notifications and send them " <>
                     "in bulk to Cannon, instead of in individual requests."))

    parseRegion = readerAsk >>= either readerError return . fromText . fromString
