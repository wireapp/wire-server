{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Util.Options where

import Control.Lens
import Data.Aeson (FromJSON)
import Data.Aeson.TH
import Data.ByteString (ByteString)
import Data.ByteString.Conversion
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.String
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml hiding (Parser)
import GHC.Generics
import GHC.Word
import Options.Applicative
import Options.Applicative.Types
import System.Directory
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import URI.ByteString
import Util.Options.Common

import qualified Data.ByteString.Char8 as BS

data AWSEndpoint = AWSEndpoint
    { _awsHost   :: !ByteString
    , _awsSecure :: !Bool
    , _awsPort   :: !Int
    , _awsScope  :: !ByteString
    } deriving (Eq, Show)

instance FromByteString AWSEndpoint where
    parser = do
        url    <- uriParser strictURIParserOptions
        secure <- case url^.uriSchemeL.schemeBSL of
                        "https" -> return True
                        "http"  -> return False
                        x       -> fail ("Unsupported scheme: " ++ show x)
        host   <- case (url^.authorityL <&> view (authorityHostL.hostBSL)) of
                        Just h  -> return h
                        Nothing -> fail ("No host in: " ++ show url)
        port   <- case urlPort url of
                        Just p  -> return p
                        Nothing -> return $ if secure then 443
                                                      else 80
        scope  <- case BS.split '.' host of
                        (_:s:_) -> return s
                        _       -> return ""
        return $ AWSEndpoint host secure port scope

instance FromJSON AWSEndpoint where
    parseJSON = withText "AWSEndpoint" $
        either fail return . runParser parser . encodeUtf8

urlPort :: URIRef Absolute -> Maybe Int
urlPort u = do
    a <- u^.authorityL
    p <- a^.authorityPortL
    return (fromIntegral (p^.portNumberL))

makeLenses ''AWSEndpoint

data Endpoint = Endpoint
    { _epHost :: !Text
    , _epPort :: !Word16
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Endpoint
makeLenses ''Endpoint

data CassandraOpts = CassandraOpts
    { _casEndpoint :: !Endpoint
    , _casKeyspace :: !Text
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''CassandraOpts
makeLenses ''CassandraOpts

newtype FilePathSecrets = FilePathSecrets FilePath
    deriving (Eq, Show, Read, FromJSON)

loadSecret :: FromJSON a => FilePathSecrets -> IO (Maybe a)
loadSecret (FilePathSecrets p) = do
    path   <- canonicalizePath p
    exists <- doesFileExist path
    if exists
        then return . decode =<< BS.readFile path
        else return Nothing

getOptions :: (FromJSON a) => String -> Parser a -> FilePath -> IO a
getOptions desc pars defaultPath = do
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
            execParser (info (helper <*> pars) mkDesc)
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

parseAWSEndpoint :: ReadM AWSEndpoint
parseAWSEndpoint = readerAsk >>= maybe (error "Could not parse AWS endpoint") return . fromByteString . fromString

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
