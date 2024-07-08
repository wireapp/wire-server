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

module Util.Options
  ( module Util.Options,
    -- TODO: Switch denpendees to the original module?
    module Cassandra.Options,
  )
where

import Cassandra.Options
import Control.Lens
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Conversion
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml hiding (Parser)
import Imports
import Options.Applicative
import Options.Applicative.Types
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
    awsHost <- case url ^. authorityL <&> view (authorityHostL . hostBSL) of
      Just h -> pure h
      Nothing -> fail ("No host in: " ++ show url)
    awsPort <- case urlPort url of
      Just p -> pure p
      Nothing ->
        pure $
          if secure
            then 443
            else 80
    pure $ AWSEndpoint awsHost secure awsPort

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

newtype FilePathSecrets = FilePathSecrets FilePath
  deriving (Eq, Show, FromJSON, IsString)

initCredentials :: (MonadIO m, FromJSON a) => FilePathSecrets -> m a
initCredentials secretFile = do
  dat <- loadSecret secretFile
  pure $ either (\e -> error $ "Could not load secrets from " ++ show secretFile ++ ": " ++ e) id dat

loadSecret :: (MonadIO m, FromJSON a) => FilePathSecrets -> m (Either String a)
loadSecret (FilePathSecrets p) = do
  path <- canonicalizePath p
  exists <- doesFileExist path
  if exists
    then liftIO $ over _Left show . decodeEither' <$> BS.readFile path
    else pure (Left "File doesn't exist")

-- | Get configuration options from the command line or configuration file.
--
-- This uses the provided optparse-applicative parser, if given. In all cases,
-- it prepends a `config-file` option to the parser that accepts a file name.
-- When that option is found, the config file is used to get the options,
-- instead of the command line.
getOptions ::
  forall a.
  (FromJSON a) =>
  -- | Program description
  String ->
  -- | CLI parser for the options (if there is no config)
  Maybe (Parser a) ->
  -- | Default config path, can be overridden with @--config-file@
  FilePath ->
  IO a
getOptions desc mp defaultPath = do
  (path, mOpts) <-
    execParser $
      info
        (optsOrConfigFile <**> helper)
        (header desc <> fullDesc)
  exists <- doesFileExist path
  case (exists, mOpts) of
    -- config file exists, take options from there
    (True, _) -> do
      decodeFileEither path >>= \case
        Left e ->
          fail $
            show e
              <> " while attempting to decode "
              <> path
        Right o -> pure o
    -- config doesn't exist, take options from command line
    (False, Just opts) -> pure opts
    -- no config, no parser, just fail
    (False, Nothing) ->
      fail $ "Config file at " <> path <> " does not exist."
  where
    optsOrConfigFile :: Parser (FilePath, Maybe a)
    optsOrConfigFile =
      (,)
        <$> strOption
          ( long "config-file"
              <> short 'c'
              <> help "Config file to load"
              <> showDefault
              <> value defaultPath
          )
        <*> sequenceA mp

parseAWSEndpoint :: ReadM AWSEndpoint
parseAWSEndpoint = readerAsk >>= maybe (error "Could not parse AWS endpoint") pure . fromByteString . fromString

discoUrlParser :: Parser Text
discoUrlParser =
  textOption $
    long "disco-url"
      <> metavar "URL"
      <> help "klabautermann url"
