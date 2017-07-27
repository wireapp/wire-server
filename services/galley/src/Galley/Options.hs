{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Galley.Options
    ( Opts
    , brigHost
    , brigPort
    , cassHost
    , cassPort
    , discoUrl
    , gundeckHost
    , gundeckPort
    , hostname
    , keyspace
    , serverPort
    , httpPoolSz
    , JournalOpts (..)
    , journalOpts
    , queueName
    , awsRegion
    , parseOptions
    , journalOptsParser
    ) where

import Cassandra hiding (Error)
import Control.Lens hiding ((.=))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Text (Text)
import Data.Word
import Data.Misc
import Data.Monoid
import Network.AWS (Region (..))
import Network.AWS.Data
import Data.String
import Options.Applicative
import Options.Applicative.Types

import qualified Data.Text as Text

data Opts = Opts
    { _hostname    :: !String
    , _serverPort  :: !Port
    , _cassHost    :: !String
    , _cassPort    :: !Word16
    , _keyspace    :: !Keyspace
    , _brigHost    :: !ByteString
    , _brigPort    :: !Port
    , _gundeckHost :: !ByteString
    , _gundeckPort :: !Port
    , _discoUrl    :: !(Maybe String)
    , _httpPoolSz  :: !Int
    , _journalOpts :: !(Maybe JournalOpts)
    }

-- [Note: journaling]
-- Journaling can be disabled simply by not passing the JournalOpts when starting the service

data JournalOpts = JournalOpts
    { _queueName :: !Text
    , _awsRegion :: !Region
    }

makeLenses ''JournalOpts
makeLenses ''Opts

parseOptions :: IO Opts
parseOptions = execParser (info (helper <*> optsParser) desc)
  where
    desc = header "Galley - Conversation Service" <> fullDesc

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
                long "keyspace"
                <> metavar "STRING"
                <> help "database keyspace to use")

        <*> (bytesOption $
                long "brig-host"
                <> metavar "HOSTNAME"
                <> help "brig hostname")

        <*> (option auto $
                long "brig-port"
                <> metavar "PORT"
                <> help "brig port")

        <*> (bytesOption $
                long "gundeck-host"
                <> metavar "HOSTNAME"
                <> help "gundeck hostname")

        <*> (option auto $
                long "gundeck-port"
                <> metavar "PORT"
                <> help "gundeck port")

        <*> (optional $ strOption $
                long "disco-url"
                <> metavar "URL"
                <> help "klabautermann url")

        <*> (option auto $
                long "http-pool-size"
                <> metavar "SIZE"
                <> showDefault
                <> help "number of connections for the http pool"
                <> value 128)

        <*> optional journalOptsParser

journalOptsParser :: Parser JournalOpts
journalOptsParser = JournalOpts
    <$> (textOption $
            long "team-events-queue-name"
            <> metavar "STRING"
            <> help "sqs queue name to send team events")

    <*> (option region $
            long "aws-region"
            <> metavar "STRING"
            <> value Ireland
            <> showDefault
            <> help "aws region name")

bytesOption :: Mod OptionFields String -> Parser ByteString
bytesOption = fmap pack . strOption

textOption :: Mod OptionFields String -> Parser Text
textOption = fmap Text.pack . strOption

region :: ReadM Region
region = readerAsk >>= either readerError return . fromText . fromString
