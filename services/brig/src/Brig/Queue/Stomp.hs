-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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
module Brig.Queue.Stomp
  ( Env (..),
    mkEnv,
    Broker (..),
    Credentials (..),
    enqueue,
    listen,
  )
where

import BasePrelude hiding (Handler, throwIO)
import qualified Brig.Options as Opts
import qualified Codec.MIME.Type as MIME
import Control.Monad.Catch (Handler (..), MonadMask)
import Control.Retry hiding (retryPolicy)
import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Conduit.Network.TLS
import Data.Text
import Data.Text.Encoding
import Network.Mom.Stompl.Client.Queue hiding (try)
import System.Logger.Class as Log
import UnliftIO (MonadUnliftIO, throwIO, withRunInIO)

data Env = Env
  { -- | STOMP broker that we're using
    broker :: Broker
  }

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

-- | Construct an 'Env' with some default settings.
mkEnv ::
  -- | Options that can be customized
  Opts.StompOpts ->
  -- | Credentials
  Credentials ->
  Env
mkEnv o cred =
  Env
    { broker =
        Broker
          { host = Opts.stompHost o,
            port = Opts.stompPort o,
            auth = Just cred,
            tls = Opts.stompTls o
          }
    }

-- | Send a message to a STOMP queue.
--
-- In case of failure will try five more times. The timeout for each attempt
-- is 500ms.
enqueue :: (ToJSON a, MonadIO m) => Broker -> Text -> a -> m ()
enqueue b q m =
  retrying retryPolicy retryPredicate (const enqueueAction) >>= either throwIO pure
  where
    retryPredicate _ res = pure (isLeft res)
    retryPolicy = limitRetries 5 <> exponentialBackoff 50000
    enqueueAction =
      liftIO $
        try @StomplException $
          stompTimeout "enqueue" 500000 $
            withConnection' b $
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
-- When 'listen' catches any kind of exception, it will reestablish the
-- connection and get a new message to process. Assuming that the broker is
-- configured properly, after failing on the same message several times the
-- message will go into the Dead Letter Queue where it can be analyzed
-- manually.
listen ::
  (FromJSON a, MonadLogger m, MonadMask m, MonadUnliftIO m) =>
  Broker ->
  Text ->
  (a -> m ()) ->
  m ()
listen b q callback =
  recovering retryPolicy handlers (const listenAction)
  where
    retryPolicy = constantDelay 1000000
    listenAction =
      withRunInIO $ \runInIO ->
        withConnection' b $ \conn ->
          withReader
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
                runInIO $ callback (msgContent m)
                stompTimeout "listen/ack" 1000000 $ ack conn m
    handlers = skipAsyncExceptions ++ [logError]
    logError = const . Handler $ \(e :: SomeException) -> do
      Log.err $
        msg (val "Exception when listening to a STOMP queue")
          ~~ field "queue" (show q)
          ~~ field "error" (show e)
      return True

-- Note [exception handling]
-- ~~~
-- The callback might throw an exception, which will be caught by
-- 'recovering'. This will kill and restart the connection, while we could
-- in theory do better (just throw away the exception without killing
-- the connection). However, this is supposed to be a very rare case
-- and it would complicate the code so we don't care.

-------------------------------------------------------------------------------
-- Utilities

iconv :: FromJSON a => Text -> InBound a
iconv queue _ _ _ bs =
  case Aeson.eitherDecode (BL.fromStrict bs) of
    Right x -> pure x
    Left e ->
      convertError $
        "Error when parsing message from STOMP queue " <> unpack queue <> ": " <> e

oconv :: ToJSON a => OutBound a
oconv = pure . BL.toStrict . Aeson.encode

jsonType :: MIME.Type
jsonType = MIME.Type (MIME.Application "json") []

-- | Set up a STOMP connection.
withConnection' :: Broker -> (Con -> IO a) -> IO a
withConnection' b =
  withConnection (unpack (host b)) (port b) config []
  where
    config =
      [OAuth (unpack (user cred)) (unpack (pass cred)) | Just cred <- [auth b]]
        ++ [OTLS (tlsClientConfig (port b) (encodeUtf8 (host b))) | tls b]
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
