{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Galley.Options where

import Control.Lens hiding ((.=))
import Data.Aeson.TH (deriveFromJSON)
import Data.Text (Text)
import Data.Monoid
import Data.Word (Word16)
import GHC.Generics
import Options.Applicative
import Util.Options
import Data.ByteString.Conversion
import Data.Maybe
import Util.Options.Common
import Data.Misc

data Settings = Settings
    { _setHttpPoolSize          :: !Int
    , _setMaxConvAndTeamSize    :: !Word16  -- NOTE: This must be in sync with brig
    , _setIntraListing          :: !Bool    -- call Brig for device listing
    , _setConversationCodeURI   :: !HttpsUrl
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Settings
makeLenses ''Settings

-- [Note: journaling]
-- Journaling can be disabled simply by not passing the JournalOpts when starting the service

data JournalOpts = JournalOpts
    { _awsQueueName :: !Text
    , _awsEndpoint  :: !AWSEndpoint
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''JournalOpts
makeLenses ''JournalOpts

data Opts = Opts
    { _optGalley    :: !Endpoint
    , _optCassandra :: !CassandraOpts
    , _optBrig      :: !Endpoint
    , _optGundeck   :: !Endpoint
    , _optDiscoUrl  :: !(Maybe Text)
    , _optSettings  :: !Settings
    , _optJournal   :: !(Maybe JournalOpts)
    }

deriveFromJSON toOptionFieldName ''Opts
makeLenses ''Opts

parseOptions :: IO Opts
parseOptions = execParser (info (helper <*> optsParser) desc)
  where
    desc = header "Galley - Conversation Service" <> fullDesc

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
  <*> brigParser
  <*> gundeckParser
  <*> optional discoUrlParser
  <*> settingsParser
  <*> optional journalOptsParser
  where
    brigParser :: Parser Endpoint
    brigParser = Endpoint
        <$> (textOption $
                long "brig-host"
                <> metavar "HOSTNAME"
                <> help "Brig hostname")
        <*>  (option auto $
                long "brig-port"
                <> metavar "PORT"
                <> help "Brig port")

    gundeckParser :: Parser Endpoint
    gundeckParser = Endpoint
        <$> (textOption $
                long "gundeck-host"
                <> metavar "HOSTNAME"
                <> help "Gundeck hostname")
        <*> (option auto $
                long "gundeck-port"
                <> metavar "PORT"
                <> help "Gundeck port")

    settingsParser :: Parser Settings
    settingsParser = Settings
        <$> (option auto $
                long "http-pool-size"
                <> metavar "SIZE"
                <> showDefault
                <> help "number of connections for the http client pool"
                <> value 128)
        <*>
            (option auto $
                long "conv-team-max-size"
                <> metavar "INT"
                <> showDefault
                <> help "Max. # of members in a conversation/team."
                <> value 128)
        <*>
            (switch $
                long "intra-device-listing"
                <> help "Use this option if you want to fetch the device list from brig instead.")
        <*>
            (httpsUrlOption $
                long "conversation-code-prefix"
                <> metavar "STRING"
                <> help "URI prefix for conversations with access mode 'code'")

journalOptsParser :: Parser JournalOpts
journalOptsParser = JournalOpts
    <$> (textOption $
            long "team-events-queue-name"
            <> metavar "STRING"
            <> help "sqs queue name to send team events")
    <*> (option parseAWSEndpoint $
            long "aws-sqs-endpoint"
            <> value (AWSEndpoint "sqs.eu-west-1.amazonaws.com" True 443)
            <> metavar "STRING"
            <> showDefault
            <> help "aws endpoint")

httpsUrlOption :: Mod OptionFields String -> Parser HttpsUrl
httpsUrlOption = fmap (fromMaybe (error "Invalid HTTPS URL") . fromByteString) . bytesOption
