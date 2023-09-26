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
{-# LANGUAGE OverloadedStrings #-}

module RabbitMQConsumer.Lib where

import Data.ByteString.Lazy.Char8 qualified as BL
import Imports
import Network.AMQP
import Network.AMQP.Types (FieldTable (..))
import Network.Socket
import Options.Applicative

main :: IO ()
main = do
  opts <- execParser (info (helper <*> optsParser) desc)
  conn <- openConnection' opts.host opts.port opts.vhost opts.username opts.password
  chan <- openChannel conn
  recoverMsgs chan True

  let abort = cancelConsumer chan

  _tag <- consumeMsgs' chan opts.queue Ack (myCallback abort) abort (FieldTable mempty)
  putStrLn "waiting for messages..."

  threadDelay $ 10 * 1000 * 1000 -- 10 seconds
  closeConnection conn
  putStrLn "connection closed"
  where
    desc = header "rabbitmq-consumer" <> progDesc "CLI tool to consume messages from a RabbitMQ queue" <> fullDesc

myCallback :: (ConsumerTag -> IO ()) -> (Message, Envelope) -> IO ()
myCallback abort (msg, _env) = do
  putStrLn $ "received message:"
  putStrLn $ BL.unpack (msgBody msg)

data Opts = Opts
  { host :: String,
    port :: PortNumber,
    username :: Text,
    password :: Text,
    vhost :: Text,
    queue :: Text
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
          <> value 5672
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
          <> value "alpaca-grapefruit"
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
    <*> strOption
      ( long "queue"
          <> short 'q'
          <> metavar "QUEUE"
          <> help "RabbitMQ Queue"
          <> value "test"
          <> showDefault
      )
