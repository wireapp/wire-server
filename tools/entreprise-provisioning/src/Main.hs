-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Main (main) where

import API
import CLI
import Data.Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Id
import Data.Map.Strict qualified as Map
import Data.Range
import Data.Text qualified as Text
import Env qualified
import Env.Internal.Help qualified as Env
import Imports
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Options.Applicative
import System.Exit (die)
import System.IO (hPutStrLn)
import Types

main :: IO ()
main = do
  env <- Env.parse (Env.header "entreprise-provisioning") envParser
  cmd <- customExecParser (prefs showHelpOnEmpty) (info (commandParser env <**> helper) fullDesc)

  case cmd of
    EnvInfo -> do
      putStrLn $ Env.helpInfo (Env.header "entreprise-provisioning" Env.defaultInfo) envParser mempty
    ProvisionUserGroupChannels opts -> do
      when opts.verbose $ do
        hPutStrLn stderr $ "Galley URL: " <> show opts.services.galleyUrl
        hPutStrLn stderr $ "Brig URL: " <> show opts.services.brigUrl
        hPutStrLn stderr $ "Loading input from: " <> inputFile opts

      inputData <- LBS.readFile opts.inputFile
      UserGroupChannelsProvisionningSpec input <- case decode inputData of
        Nothing -> die "Failed to parse input JSON"
        Just x -> pure x

      when opts.verbose $
        hPutStrLn stderr $
          "Processing " <> show (Map.size input) <> " user groups"

      manager <- newManager tlsManagerSettings

      userGroupResults <- Map.traverseWithKey (processUserGroup manager opts opts.authToken) input

      LBS.putStr $ encode $ UserGroupChannelsProvisionningResult userGroupResults

-- | Process a single user group
processUserGroup ::
  Manager ->
  ProvisionUserGroupChannelsOptions ->
  Token ->
  UserGroupId ->
  [ChannelName] ->
  IO UserGroupResult
processUserGroup manager opts token groupId channelNames = do
  when opts.verbose $
    hPutStrLn stderr $
      "Processing user group: "
        <> show groupId
        <> " with "
        <> show (length channelNames)
        <> " channels"

  -- Create channels
  channelResults <- mapM (createChannelWithLogging manager opts token) channelNames

  -- Extract successful channel IDs
  let successfulIds = [cid | ChannelSuccess _ cid <- channelResults]

  when opts.verbose $
    hPutStrLn stderr $
      "Successfully created "
        <> show (length successfulIds)
        <> " out of "
        <> show (length channelNames)
        <> " channels"

  -- Associate channels to group
  assocResult <-
    if null successfulIds
      then do
        when opts.verbose $
          hPutStrLn stderr "Skipping association: no channels created successfully"
        pure $
          Left $
            ErrorDetail 0 $
              object ["error" .= ("no channels created" :: Text)]
      else do
        when opts.verbose $
          hPutStrLn stderr "Associating channels to user group..."
        associateChannelsToGroup manager opts.services token opts.userId groupId successfulIds

  pure $
    UserGroupResult
      { channel = channelResults,
        association = case assocResult of
          Right _ -> AssociationSuccess
          Left err -> AssociationFailureResult err
      }

-- | Create a channel with logging
createChannelWithLogging ::
  Manager ->
  ProvisionUserGroupChannelsOptions ->
  Token ->
  ChannelName ->
  IO ChannelResult
createChannelWithLogging manager opts token channelName' = do
  when opts.verbose $
    hPutStrLn stderr $
      "Creating channel: " <> Text.unpack (fromRange $ fromChannelName channelName')

  result <- createChannel manager opts.services token opts.userId opts.teamId channelName'

  case result of
    Right convId -> do
      when opts.verbose $
        hPutStrLn stderr $
          "✓ Channel created: " <> Text.unpack (fromRange $ fromChannelName channelName')
      pure $ ChannelSuccess channelName' convId
    Left err -> do
      when opts.verbose $
        hPutStrLn stderr $
          "✗ Channel creation failed: " <> Text.unpack (fromRange $ fromChannelName channelName')
      pure $ ChannelFailure channelName' err
