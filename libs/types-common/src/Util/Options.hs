{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Util.Options where

import Control.Lens
import Data.Aeson (FromJSON)
import Data.Aeson.TH
import Data.Json.Util (toFieldName)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
import Data.Word (Word16)
import Data.Yaml hiding (Parser)
import GHC.Generics
import Options.Applicative
import System.Directory
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Util.Options.Common

data Endpoint = Endpoint
    { _epHost :: !Text
    , _epPort :: !Word16
    } deriving (Show, Generic)

deriveFromJSON toFieldName ''Endpoint
makeLenses ''Endpoint

data CassandraOpts = CassandraOpts
    { _casEndpoint :: !Endpoint
    , _casKeyspace :: !Text
    } deriving (Show, Generic)

deriveFromJSON toFieldName ''CassandraOpts
makeLenses ''CassandraOpts

getOptions :: (FromJSON a) => String -> Parser a -> FilePath -> IO a
getOptions desc parser defaultPath = do
    path <- parseConfigPath defaultPath mkDesc
    file <- doesFileExist path
    if file
        then do
            configFile <- decodeFileEither path
            case configFile of
                Left e  -> fail $ show e
                Right o -> return o
        else do
            hPutStrLn stderr $
                "Config file at " ++
                path ++
                " does not exist, falling back to command-line arguments. \n"
            execParser (info (helper <*> parser) mkDesc)
  where
    mkDesc = header desc <> fullDesc

parseConfigPath :: FilePath -> InfoMod String -> IO String
parseConfigPath defaultPath desc = do
    args <- getArgs
    let result =
            getParseResult $
            execParserPure defaultPrefs (info (helper <*> pathParser) desc) args
    pure $ fromMaybe defaultPath result
  where
    pathParser :: Parser String
    pathParser =
        strOption $
        long "config-file" <> short 'c' <> help "Config file to load" <>
        showDefault <>
        value defaultPath

cassandraParser :: Parser CassandraOpts
cassandraParser = CassandraOpts <$>
    (Endpoint <$>
        (textOption $
            long "cassandra-host"
            <> metavar "HOSTNAME" 
            <> help "Cassandra hostname or address")
      <*>
        (option auto $
            long "cassandra-port"
            <> metavar "PORT"
            <> help "Cassandra port"))
  <*>
    (textOption $
        long "cassandra-keyspace"
        <> metavar "STRING"
        <> help "Cassandra keyspace")

discoUrlParser :: Parser Text
discoUrlParser = textOption
    $ long "disco-url" 
    <> metavar "URL"
    <> help "klabautermann url"
