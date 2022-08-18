{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Util.Options where

import Control.Lens
import Data.Aeson.TH
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Conversion
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml hiding (Parser)
import Imports
import Options.Applicative
import Options.Applicative.Types
import System.Exit (die)
import System.IO (hPutStrLn)
import URI.ByteString
import Util.Options.Common

data AWSEndpoint = AWSEndpoint
  { _awsHost :: !ByteString,
    _awsSecure :: !Bool,
    _awsPort :: !Int
  }
  deriving (Eq, Show)

instance FromByteString AWSEndpoint where
  parser = do
    url <- uriParser strictURIParserOptions
    secure <- case url ^. uriSchemeL . schemeBSL of
      "https" -> pure True
      "http" -> pure False
      x -> fail ("Unsupported scheme: " ++ show x)
    host <- case url ^. authorityL <&> view (authorityHostL . hostBSL) of
      Just h -> pure h
      Nothing -> fail ("No host in: " ++ show url)
    port <- case urlPort url of
      Just p -> pure p
      Nothing ->
        pure $
          if secure
            then 443
            else 80
    pure $ AWSEndpoint host secure port

instance FromJSON AWSEndpoint where
  parseJSON =
    withText "AWSEndpoint" $
      either fail pure . runParser parser . encodeUtf8

urlPort :: URIRef Absolute -> Maybe Int
urlPort u = do
  a <- u ^. authorityL
  p <- a ^. authorityPortL
  pure (fromIntegral (p ^. portNumberL))

makeLenses ''AWSEndpoint

data Endpoint = Endpoint
  { _epHost :: !Text,
    _epPort :: !Word16
  }
  deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Endpoint

makeLenses ''Endpoint

data CassandraOpts = CassandraOpts
  { _casEndpoint :: !Endpoint,
    _casKeyspace :: !Text,
    -- | If this option is unset, use all available nodes.
    -- If this option is set, use only cassandra nodes in the given datacentre
    --
    -- This option is most likely only necessary during a cassandra DC migration
    -- FUTUREWORK: remove this option again, or support a datacentre migration feature
    _casFilterNodesByDatacentre :: !(Maybe Text)
  }
  deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''CassandraOpts

makeLenses ''CassandraOpts

newtype FilePathSecrets = FilePathSecrets FilePath
  deriving (Eq, Show, FromJSON)

loadSecret :: FromJSON a => FilePathSecrets -> IO (Either String a)
loadSecret (FilePathSecrets p) = do
  path <- canonicalizePath p
  exists <- doesFileExist path
  if exists
    then over _Left show . decodeEither' <$> BS.readFile path
    else pure (Left "File doesn't exist")

getOptions ::
  FromJSON a =>
  -- | Program description
  String ->
  -- | CLI parser for the options (if there is no config)
  Maybe (Parser a) ->
  -- | Default config path, can be overridden with @--config-file@
  FilePath ->
  IO a
getOptions desc pars defaultPath = do
  path <- parseConfigPath defaultPath mkDesc
  file <- doesFileExist path
  case (file, pars) of
    -- Config exists, we can just take options from there
    (True, _) -> do
      configFile <- decodeFileEither path
      case configFile of
        Left e ->
          fail $
            show e
              <> " while attempting to decode "
              <> show path
        Right o -> pure o
    -- Config doesn't exist but at least we have a CLI options parser
    (False, Just p) -> do
      hPutStrLn stderr $
        "Config file at " ++ path
          ++ " does not exist, falling back to command-line arguments. \n"
      execParser (info (helper <*> p) mkDesc)
    -- No config, no parser :(
    (False, Nothing) -> do
      die $ "Config file at " ++ path ++ " does not exist. \n"
  where
    mkDesc :: InfoMod b
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
        long "config-file" <> short 'c' <> help "Config file to load"
          <> showDefault
          <> value defaultPath

parseAWSEndpoint :: ReadM AWSEndpoint
parseAWSEndpoint = readerAsk >>= maybe (error "Could not parse AWS endpoint") pure . fromByteString . fromString

discoUrlParser :: Parser Text
discoUrlParser =
  textOption $
    long "disco-url"
      <> metavar "URL"
      <> help "klabautermann url"
