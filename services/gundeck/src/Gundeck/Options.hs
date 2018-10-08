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
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''AWSOpts
makeLenses ''AWSOpts

data FallbackOpts = FallbackOpts
    { _fbSkipFallbacks :: !Bool
    , _fbPreferNotice  :: !Bool
    , _fbQueueDelay    :: !Word64
    , _fbQueueLimit    :: !Int
    , _fbQueueBurst    :: !Word16
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''FallbackOpts
makeLenses ''FallbackOpts

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
    , _optFallback  :: !FallbackOpts
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
  <*> fallbackParser
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

    fallbackParser :: Parser FallbackOpts
    fallbackParser = FallbackOpts <$>
        -- NOTE: If set, notifications are still queued to be sent, etc. but never actually
        -- end up getting sent out. This allows us to still keep track of how successful
        -- we are with cancelling the fallback notifications and thus get a feeling of
        --  where we stand today.
        (switch $
            long "skip-fallbacks"
            <> help "Use this option if you wish to never send delayed fallback notifications.")

        <*> (switch $
                long "prefer-notice"
                <> help "Use this option if you always wish to send notifications of type notice.")

        <*> (delayOption $
                long "fallback-queue-delay"
                <> metavar "SIZE"
                <> showDefault
                <> help "Delay (seconds) of notifications before sending a fallback. \
                   \MUST be higher than 30 seconds."
                <> value 300)

        <*> (option auto $
                long "fallback-queue-limit"
                <> metavar "SIZE"
                <> showDefault
                <> help "Max. size of the notification fallback queue."
                <> value 30000)

        <*> (option auto $
                long "fallback-queue-burst"
                <> metavar "SIZE"
                <> showDefault
                <> help "Max. number of delayed notifications to fire in a row (i.e. per second)."
                <> value 100)

    delayOption = fmap check . option auto
      where
        check x = if x < 30 then error "Delay must > 30" else x

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
