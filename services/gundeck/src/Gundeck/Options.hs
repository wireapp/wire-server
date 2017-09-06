{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gundeck.Options
    ( Opts
    , parseOptions
    , hostname
    , serverPort
    , discoUrl

      -- * Storage
    , cassHost
    , cassPort
    , keyspace
    , redisHost
    , redisPort

      -- * AWS
    , queueName
    , awsRegion
    , awsAccount
    , awsArnEnv

      -- * RPC
    , httpPoolSize

      -- * Notifications
    , NotificationTTL (..)
    , notificationTTL

      -- * Fallback Notification Queue
    , fbNoQueue
    , fbQueueLimit
    , fbQueueDelay
    , fbQueueBurst
    ) where

import Cassandra hiding (Error)
import Control.Lens hiding ((.=))
import Data.Text (Text, pack)
import Data.Word
import Data.Misc
import Data.Monoid
import Data.String
import Gundeck.Aws.Arn
import Options.Applicative
import Options.Applicative.Types

newtype NotificationTTL = NotificationTTL
    { notificationTTLSeconds :: Word32 }
    deriving (Eq, Ord, Show)

data Opts = Opts
    { _hostname        :: !String
    , _serverPort      :: !Port
    , _cassHost        :: !String
    , _cassPort        :: !Word16
    , _keyspace        :: !Keyspace
    , _redisHost       :: !String
    , _redisPort       :: !Word16
    , _discoUrl        :: !(Maybe String)
    , _httpPoolSize    :: !Int
    , _queueName       :: !Text
    , _awsRegion       :: !Region
    , _awsAccount      :: !Account
    , _awsArnEnv       :: !ArnEnv
    , _notificationTTL :: !NotificationTTL
    , _fbNoQueue       :: !Bool
    , _fbQueueDelay    :: !Word64
    , _fbQueueLimit    :: !Int
    , _fbQueueBurst    :: !Word16
    }

makeLenses ''Opts

parseOptions :: IO Opts
parseOptions = execParser (info (helper <*> optsParser) desc)
  where
    desc = header "Gundeck - Push Notifications" <> fullDesc

    optsParser :: Parser Opts
    optsParser = Opts
        <$> (strOption $
                long "host"
                <> value "*4"
                <> showDefault
                <> metavar "HOSTNAME"
                <> help "host to listen on")

        <*> (option auto $
                long "port"
                <> short 'p'
                <> metavar "PORT"
                <> help "listen port")

        <*> (strOption $
                long "cassandra-host"
                <> metavar "HOSTNAME"
                <> help "cassandra hostname")

        <*> (option auto $
                long "cassandra-port"
                <> metavar "PORT"
                <> help "cassandra port")

        <*> (fmap Keyspace . textOption $
                long "cassandra-keyspace"
                <> metavar "STRING"
                <> help "database keyspace to use")

        <*> (strOption $
                long "redis-host"
                <> metavar "HOSTNAME"
                <> help "redis hostname")

        <*> (option auto $
                long "redis-port"
                <> metavar "PORT"
                <> help "redis port")

        <*> (optional $ strOption $
                long "disco-url"
                <> metavar "URL"
                <> help "klabautermann url")

        <*> (option auto $
                long "http-pool-size"
                <> metavar "SIZE"
                <> showDefault
                <> help "number of connections for the http client pool"
                <> value 128)

        <*> (textOption $
                long "event-queue-name"
                <> metavar "STRING"
                <> help "sqs queue name")

        <*> (option region $
                long "aws-region"
                <> metavar "STRING"
                <> help "aws region name")

        <*> (fmap Account . textOption $
                long "aws-account"
                <> metavar "STRING"
                <> help "aws account")

        <*> (fmap ArnEnv . textOption $
                long "aws-arn-env"
                <> metavar "STRING"
                <> help "environment name to scope ARNs to")

        <*> (fmap NotificationTTL . option auto $
                long "notification-ttl"
                <> metavar "SIZE"
                <> showDefault
                <> help "TTL (seconds) of stored notifications"
                <> value 86400)

        <*> (switch $
                long "no-fallback-queue"
                <> help "Use this option if you wish to never send delayed fallback notifications. \
                   \If set, the following fallback-queue options are irrelevant.")

        <*> (option auto $
                long "fallback-queue-delay"
                <> metavar "SIZE"
                <> showDefault
                <> help "Delay (seconds) of notifications in the fallback queue. \
                   \Should be higher than the values defined in the AWS module."
                <> value 60)

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

    region = readerAsk >>= either readerError return . fromText . fromString

    textOption :: Mod OptionFields String -> Parser Text
    textOption = fmap pack . strOption
