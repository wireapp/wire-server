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

module CLI
  ( Command (..),
    ProvisionUserGroupChannelsOptions (..),
    commandParser,
    envParser,
  )
where

import Data.Id
import Data.UUID qualified as UUID
import Env qualified
import Imports
import Options.Applicative
import Types

data Env = Env
  { envGalleyUrl :: Maybe ApiUrl,
    envBrigUrl :: Maybe ApiUrl,
    envAuthToken :: Maybe Token
  }

data ProvisionUserGroupChannelsOptions = ProvisionUserGroupChannelsOptions
  { teamId :: TeamId,
    userId :: UserId,
    services :: Services,
    inputFile :: FilePath,
    authToken :: Token,
    verbose :: Bool
  }

data Command
  = ProvisionUserGroupChannels ProvisionUserGroupChannelsOptions
  | EnvInfo

envParser :: Env.Parser Env.Error Env
envParser =
  Env
    <$> Env.optional (ApiUrl <$> Env.var Env.str "WIRE_GALLEY_URL" (Env.help "Galley service URL"))
    <*> Env.optional (ApiUrl <$> Env.var Env.str "WIRE_BRIG_URL" (Env.help "Brig service URL"))
    <*> Env.optional (Token <$> Env.var Env.str "WIRE_AUTH_TOKEN" (Env.help "Authentication token"))

commandParser :: Env -> Parser Command
commandParser env =
  hsubparser $
    mconcat
      [ command
          "user-groups"
          ( info
              (userGroupsParser env)
              (progDesc "Provision user groups with channels")
          ),
        command
          "env"
          ( info
              envInfoParser
              (progDesc "Environment variable information")
          )
      ]

userGroupsParser :: Env -> Parser Command
userGroupsParser env =
  hsubparser $
    command
      "channels"
      ( info
          (channelsParser env)
          (progDesc "Create channels and associate them with user groups")
      )
  where
    channelsParser :: Env -> Parser Command
    channelsParser e =
      ProvisionUserGroupChannels <$> provisionParser e

    provisionParser :: Env -> Parser ProvisionUserGroupChannelsOptions
    provisionParser e =
      ProvisionUserGroupChannelsOptions
        <$> option
          (maybeReader $ fmap Id . UUID.fromString)
          ( short 't'
              <> long "team-id"
              <> metavar "TEAM_ID"
              <> help "Team ID (UUID)"
          )
        <*> option
          (maybeReader $ fmap Id . UUID.fromString)
          ( short 'u'
              <> long "user-id"
              <> metavar "USER_ID"
              <> help "User ID (UUID)"
          )
        <*> ( Services
                <$> ( ApiUrl
                        <$> strOption
                          ( long "galley-url"
                              <> metavar "GALLEY_URL"
                              <> help "Galley service URL"
                              <> foldMap (\url -> value url.fromApiUrl <> showDefault) e.envGalleyUrl
                          )
                    )
                <*> ( ApiUrl
                        <$> strOption
                          ( long "brig-url"
                              <> metavar "BRIG_URL"
                              <> help "Brig service URL"
                              <> foldMap (\url -> value url.fromApiUrl <> showDefault) e.envBrigUrl
                          )
                    )
            )
        <*> strOption
          ( short 'f'
              <> long "file"
              <> metavar "FILENAME"
              <> help "Input JSON file"
          )
        <*> case envAuthToken e of
          Just token ->
            option
              (Token <$> str)
              ( long "auth-token"
                  <> metavar "TOKEN"
                  <> help "Authentication token"
                  <> value token
              )
          Nothing ->
            Token
              <$> strOption
                ( long "auth-token"
                    <> metavar "TOKEN"
                    <> help "Authentication token"
                )
        <*> switch
          ( short 'v'
              <> long "verbose"
              <> help "Enable verbose output to stderr"
          )

envInfoParser :: Parser Command
envInfoParser =
  hsubparser $
    command
      "info"
      ( info
          (pure EnvInfo)
          (progDesc "Display environment variable parsing help")
      )
