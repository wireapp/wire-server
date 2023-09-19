-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module RabbitMQConsumer.Lib where

import Imports
import Options.Applicative

data Opts = Opts
  { host :: String,
    port :: Int,
    username :: String,
    password :: String,
    vhost :: String
  }
  deriving (Show)

optsParser :: Parser Opts
optsParser =
  Opts
    <$> strOption
      ( long "host"
          <> short 's'
          <> metavar "HOST"
          <> help "RabbitMQ host"
          <> value "localhost"
          <> showDefault
      )
    <*> option
      auto
      ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> help "RabbitMQ Port"
          <> value 15762
          <> showDefault
      )
    <*> strOption
      ( long "username"
          <> short 'u'
          <> metavar "USERNAME"
          <> help "RabbitMQ Username"
          <> value "guest"
          <> showDefault
      )
    <*> strOption
      ( long "password"
          <> short 'w'
          <> metavar "PASSWORD"
          <> help "RabbitMQ Password"
          <> value "guest"
          <> showDefault
      )
    <*> strOption
      ( long "vhost"
          <> short 'v'
          <> metavar "VHOST"
          <> help "RabbitMQ VHost"
          <> value "/"
          <> showDefault
      )

main :: IO ()
main = do
  opts <- execParser (info (helper <*> optsParser) desc)
  putStrLn $ "Connecting to RabbitMQ at " <> show opts
  where
    desc = header "rabbitmq-consumer" <> progDesc "CLI tool to consume messages from a RabbitMQ queue" <> fullDesc
