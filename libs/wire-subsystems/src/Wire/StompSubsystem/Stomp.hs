{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

-- | Working with STOMP queues (targeting ActiveMQ specifically).
module Wire.StompSubsystem.Stomp
  ( Env (..),
    Stomp (..),
    mkEnv,
    Broker (..),
    Credentials (..),
    StompOpts (..),
    enqueueInternal,
    listenInternal,
    runStompSubsystem,
  )
where

import Codec.MIME.Type qualified as MIME
import Control.Lens hiding ((.=))
import Control.Monad.Catch (Handler (..), MonadCatch, MonadMask, try)
import Control.Monad.Trans.Resource
import Control.Retry hiding (retryPolicy)
import Data.Aeson as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Char qualified as Char
import Data.Conduit.Network.TLS
import Data.Text
import Data.Text.Encoding
import Imports
import Network.Mom.Stompl.Client.Queue hiding (try)
import Polysemy
import Polysemy.Final
import System.Logger qualified as Logger
import System.Logger.Class as Log hiding (settings)
import UnliftIO (throwIO, timeout)
import Wire.StompSubsystem

data Broker = Broker
  { -- | Broker URL
    host :: Text,
    -- | Port
    port :: Int,
    -- | Username and password
    auth :: Maybe Credentials,
    -- | Whether to use TLS
    tls :: Bool
  }
  deriving (Show)

data Credentials = Credentials
  { user :: Text,
    pass :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Credentials

data StompOpts = StompOpts
  { host :: !Text,
    port :: !Int,
    tls :: !Bool
  }
  deriving (Show, Generic)

instance FromJSON StompOpts where
  parseJSON = genericParseJSON customOptions
    where
      customOptions =
        defaultOptions
          { fieldLabelModifier = \a -> "stom" <> capitalise a
          }
      capitalise :: String -> String
      capitalise [] = []
      capitalise (x : xs) = Char.toUpper x : xs

data Env = Env
  { _logger :: !Logger.Logger,
    _broker :: !Broker
  }

makeLenses ''Env

newtype Stomp a = Stomp
  { unStomp :: ReaderT Env (ResourceT IO) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadReader Env,
      MonadResource,
      MonadUnliftIO
    )

instance MonadLogger Stomp where
  log l m = view logger >>= \g -> Logger.log g l m

-- | Construct an 'Env' with some default settings.
mkEnv ::
  -- | Logger
  Logger.Logger ->
  -- | Options that can be customized
  StompOpts ->
  -- | Credentials
  Credentials ->
  Env
mkEnv lgr o cred =
  Env
    { _logger = Logger.clone (Just "stomp") lgr,
      _broker =
        Broker
          { host = o.host,
            port = o.port,
            auth = Just cred,
            tls = o.tls
          }
    }

-- | Send a message to a STOMP queue.
--
-- In case of failure will try five more times. The timeout for each attempt
-- is 500ms.
enqueueInternal :: (ToJSON a) => Text -> a -> Stomp ()
enqueueInternal q m = do
  b <- view broker
  retrying (retryPolicy b) (retryPredicate b) (const $ enqueueAction b) >>= either throwIO pure
  where
    retryPredicate _ _ res = pure (isLeft res)
    retryPolicy _ = limitRetries 5 <> exponentialBackoff 50000
    enqueueAction broker' =
      liftIO $
        (try :: IO () -> IO (Either StomplException ())) $
          stompTimeout "enqueue" 500000 $
            withConnection' broker' $
              \conn ->
                withWriter
                  conn
                  (unpack q)
                  (unpack q)
                  [OWithReceipt, OWaitReceipt]
                  []
                  oconv
                  $ \w ->
                    writeQ w jsonType [("persistent", "true")] m

-- Note [receipts]
-- ~~~
-- When we acknowledge a message in 'listen', we don't need to wait for
-- a receipt because nothing bad will happen if our ACK doesn't go
-- through; handlers of events coming via queues are supposed to be
-- idempotent.
--
-- However, when we *send* a message, we definitely want a receipt (a
-- confirmation that the broker received the message). This doesn't
-- eliminate failure modes entirely – if we don't get a receipt we might
-- think that a message has not been enqueued while it in fact has – but
-- it's better than the opposite.

-- | Forever listen to messages from a STOMP queue and execute a callback
-- for each incoming message.
--
-- In case of connection failure or an exception, will retry indefinitely.
--
-- When 'listenInternal' catches any kind of exception, it will reestablish the
-- connection and get a new message to process. Assuming that the broker is
-- configured properly, after failing on the same message several times the
-- message will go into the Dead Letter Queue where it can be analyzed
-- manually.
listenInternal ::
  (FromJSON a) =>
  Text ->
  (a -> IO ()) ->
  Stomp ()
listenInternal q callback = do
  b <- view broker
  recovering retryPolicy (handlers b) (const $ listenAction b)
  where
    retryPolicy = constantDelay 1000000
    listenAction broker' =
      withRunInIO $ \_ ->
        withConnection' broker' $ \conn ->
          Network.Mom.Stompl.Client.Queue.withReader
            conn
            (unpack q)
            (unpack q)
            [OMode ClientIndi]
            []
            (iconv q)
            $ \r ->
              forever $ do
                -- NB: 'readQ' can't timeout because it's just reading from
                -- a chan (no network queries are being made)
                m <- readQ r
                callback (msgContent m)
                stompTimeout "listen/ack" 1000000 $ ack conn m
    handlers _ = skipAsyncExceptions ++ [logError]
    logError = const . Handler $ \(e :: SomeException) -> do
      Log.err $
        msg (val "Exception when listening to a STOMP queue")
          ~~ field "queue" (show q)
          ~~ field "error" (show e)
      pure True

-- Note [exception handling]
-- ~~~
-- The callback might throw an exception, which will be caught by
-- 'recovering'. This will kill and restart the connection, while we could
-- in theory do better (just throw away the exception without killing
-- the connection). However, this is supposed to be a very rare case
-- and it would complicate the code so we don't care.

-------------------------------------------------------------------------------
-- Utilities

iconv :: (FromJSON a) => Text -> InBound a
iconv queue _ _ _ bs =
  case Aeson.eitherDecode (BL.fromStrict bs) of
    Right x -> pure x
    Left e ->
      convertError $
        "Error when parsing message from STOMP queue " <> unpack queue <> ": " <> e

oconv :: (ToJSON a) => OutBound a
oconv = pure . BL.toStrict . Aeson.encode

jsonType :: MIME.Type
jsonType = MIME.Type (MIME.Application "json") []

-- | Set up a STOMP connection.
withConnection' :: Broker -> (Con -> IO a) -> IO a
withConnection' b =
  withConnection (unpack b.host) b.port config []
  where
    config =
      [OAuth (unpack cred.user) (unpack cred.pass) | Just cred <- [b.auth]]
        ++ [OTLS (tlsClientConfig b.port (encodeUtf8 b.host)) | b.tls]
        ++ [OTmo 1000]

-- | Like 'timeout', but throws an 'AppException' instead of returning a
-- 'Maybe'. Not very composable, but kinda convenient here.
stompTimeout :: String -> Int -> IO a -> IO a
stompTimeout location t act =
  timeout t act >>= \case
    Just x -> pure x
    Nothing ->
      throwIO $
        AppException $
          location <> ": STOMP request took more than " <> show t <> "mcs and has timed out"

-------------------------------------------------------------------------------
-- Polysemy Interpreter

runStompSubsystem ::
  (Member (Final IO) r) =>
  Env ->
  Sem (StompSubsystem : r) a ->
  Sem r a
runStompSubsystem env = interpretFinal $ \case
  Enqueue _host queue message -> liftS @IO $ runResourceT $ runReaderT (enqueueInternal queue message).unStomp env
  Listen _host queue callback -> do
    callbackS <- bindS callback
    s <- getInitialStateS
    liftS @IO $ runResourceT $ runReaderT ((listenInternal queue $ \message -> void $ callbackS (s $> message)).unStomp) env
