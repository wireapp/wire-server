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
import Network.Socket
import Options.Applicative

main :: IO ()
main = do
  opts <- execParser (info (helper <*> optsParser) desc)
  done <- newEmptyMVar
  conn <- openConnection' opts.host opts.port opts.vhost opts.username opts.password
  chan <- openChannel conn
  recoverMsgs chan True
  void $ consumeMsgs chan opts.queue Ack (myCallback opts done)
  takeMVar done
  closeConnection conn
  putStrLn "connection closed"
  where
    desc = header "rabbitmq-consumer" <> progDesc "CLI tool to consume messages from a RabbitMQ queue" <> fullDesc

myCallback :: Opts -> MVar () -> (Message, Envelope) -> IO ()
myCallback opts done (msg, env) = do
  putStrLn $ "received message (vhost=" <> cs opts.vhost <> ") (queue=" <> cs opts.queue <> "):\n"
  putStrLn $ BL.unpack (msgBody msg)
  putStrLn $ "\ntype 'drop' to drop the message and terminate, or press enter to terminate without dropping the message"
  input <- getLine
  if input == "drop"
    then do
      ackEnv env
      putStrLn "message dropped"
    else putStrLn "message not dropped"
  putMVar done ()
  -- block and stop processing any more messages
  forever $ threadDelay maxBound

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
