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

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Domain (Domain)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Imports
import Network.AMQP
import Network.Socket
import Options.Applicative
import Wire.API.Component (Component)
import Wire.API.Federation.BackendNotifications (BackendNotification (..))

main :: IO ()
main = do
  opts <- execParser (info (helper <*> optsParser) desc)
  conn <- openConnection' opts.host opts.port opts.vhost opts.username opts.password
  chan <- openChannel conn
  qos chan 0 1 False
  done <- newEmptyMVar
  case opts.cmd of
    Interactive -> void $ consumeMsgs chan opts.queue Ack (interactive done opts)
    Head -> do
      runTimerAsync done opts.timeoutSec
      void $ consumeMsgs chan opts.queue Ack (printHead done opts)
    DropHead dhOpts -> do
      runTimerAsync done opts.timeoutSec
      void $ consumeMsgs chan opts.queue Ack (dropHead done opts dhOpts)
  takeMVar done
  closeConnection conn
  putStrLn "connection closed"
  where
    desc = header "rabbitmq-consumer" <> progDesc "CLI tool to consume messages from a RabbitMQ queue" <> fullDesc

    printHead :: MVar () -> Opts -> (Message, Envelope) -> IO ()
    printHead done opts (msg, _env) = do
      putStrLn $ displayMessage opts msg
      void $ tryPutMVar done ()

    dropHead :: MVar () -> Opts -> DropHeadOpts -> (Message, Envelope) -> IO ()
    dropHead done opts dhOpts (msg, env) = do
      putStrLn $ displayMessage opts msg
      case decode @BackendNotification msg.msgBody of
        Nothing -> putStrLn "failed to decode message body"
        Just bn -> do
          if bn.path == dhOpts.path
            then do
              putStrLn "dropping message"
              nackEnv env
            else do
              putStrLn "path does not match. keeping message"
      void $ tryPutMVar done ()

    interactive :: MVar () -> Opts -> (Message, Envelope) -> IO ()
    interactive done opts (msg, env) = do
      putStrLn $ displayMessage opts msg
      putStrLn $ "type 'drop' to drop the message and terminate, or press enter to terminate without dropping the message"
      input <- getLine
      if input == "drop"
        then do
          ackEnv env
          putStrLn "message dropped"
        else putStrLn "message not dropped"
      void $ tryPutMVar done ()

    displayMessage :: Opts -> Message -> String
    displayMessage opts msg =
      intercalate
        "\n"
        [ "vhost: " <> T.unpack opts.vhost,
          "queue: " <> T.unpack opts.queue,
          "timestamp: " <> show msg.msgTimestamp,
          "received message: \n" <> BL.unpack (maybe msg.msgBody encodePretty (decode @BackendNotification' msg.msgBody))
        ]

    runTimerAsync :: MVar () -> Int -> IO ()
    runTimerAsync done sec = void $ forkIO $ do
      threadDelay (sec * 1000000)
      putStrLn $ "timeout after " <> show sec <> " seconds"
      void $ tryPutMVar done ()

data Opts = Opts
  { host :: String,
    port :: PortNumber,
    username :: Text,
    password :: Text,
    vhost :: Text,
    queue :: Text,
    timeoutSec :: Int,
    cmd :: Command
  }

data DropHeadOpts = DropHeadOpts
  { path :: Text
  }

data Command = Head | DropHead DropHeadOpts | Interactive

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
    <*> option
      auto
      ( long "timeout"
          <> short 't'
          <> metavar "TIMEOUT"
          <> help
            "Timeout in seconds. The command will timeout if no messages are received within this time. \
            \This can happen when the queue is empty, \
            \or when we lose the single active consumer race."
          <> value 10
          <> showDefault
      )
    <*> hsubparser (headCommand <> dropHeadCommand <> interactiveCommand)

headCommand :: Mod CommandFields Command
headCommand =
  (command "head" (info (pure Head) (progDesc "Print the first message in the queue")))

dropHeadCommand :: Mod CommandFields Command
dropHeadCommand =
  (command "drop-head" (info p (progDesc "Drop the first message in the queue")))
  where
    p :: Parser Command
    p =
      DropHead
        . DropHeadOpts
        <$> strOption
          ( long "path"
              <> short 'a'
              <> metavar "PATH"
              <> help "only drop the first message if the path matches"
          )

interactiveCommand :: Mod CommandFields Command
interactiveCommand =
  (command "interactive" (info (pure Interactive) (progDesc "Interactively drop the first message from the queue")))

newtype Body = Body {unBody :: Value}
  deriving (Show, Eq, Generic)

instance ToJSON Body where
  toJSON (Body v) = v

instance FromJSON Body where
  parseJSON v =
    Body . bodyToValue . TL.encodeUtf8 <$> parseJSON v
    where
      bodyToValue :: BL.ByteString -> Value
      bodyToValue bs =
        fromMaybe (String . TL.toStrict . TL.decodeUtf8 $ bs) $
          decode @Value bs

-- | A variant of 'BackendNotification' with a FromJSON instance for the body field
-- that converts its BL.ByteString content to a JSON value so that it can be pretty printed
data BackendNotification' = BackendNotification'
  { ownDomain :: Domain,
    targetComponent :: Component,
    path :: Text,
    body :: Body
  }
  deriving (Show, Eq, Generic)

instance ToJSON BackendNotification'

instance FromJSON BackendNotification'
