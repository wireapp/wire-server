{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Working with STOMP queues.
module Brig.Stomp
    ( Env(..)
    , enqueue
    , listen
    ) where

import BasePrelude hiding (throwIO, try)

import Control.Lens
import Control.Monad.Catch (MonadMask)
import Control.Retry hiding (retryPolicy)
import Data.Aeson                                     as Aeson
import Data.Text
import Network.Mom.Stompl.Client.Queue hiding (try)
import System.Logger.Class
import UnliftIO

import qualified Codec.MIME.Type                      as MIME
import qualified Data.ByteString.Lazy                 as BL

data Env = Env
    {
    -- | STOMP broker URL
      _serverUrl  :: Text
    -- | Port (usually 61613 or 61614)
    , _serverPort :: Int
    -- | Username and password
    , _auth       :: Maybe (Text, Text)
    -- | Internal queue name (used only for debugging)
    , _queueName  :: Text
    -- | Queue name as used by the broker
    , _queuePath  :: Text
    }

makeLenses ''Env

-- mkEnv :: Logger -> Opt.AWSOpts -> Manager -> IO Env
-- mkEnv lgr opts mgr = do

-- InternalEventQueue
-- /queue/internal-event

-- | Send a message to a STOMP queue.
--
-- In case of failure will try five times (with exponential backoff) and
-- then throw an exception.
enqueue :: (ToJSON a, MonadIO m) => Env -> a -> m ()
enqueue e m =
    retrying retryPolicy retryPredicate (const enqueueAction) >>= either throwIO pure
  where
    -- TODO: we should have some timeout here but I'm not sure what that
    -- timeout should be (if we try 5 times then it can't be too high
    -- because the client might have a 10s timeout or smth -- maybe we
    -- should just try three times instead of five?)
    retryPredicate _ res = pure (isLeft res)
    retryPolicy = limitRetries 5 <> exponentialBackoff 100000
    enqueueAction =
        liftIO $ try @_ @StomplException $
        withConnection' e $ \conn ->
        withWriter conn (unpack (e^.queueName))
                        (unpack (e^.queuePath)) [] [] oconv $ \q ->
            writeQ q jsonType [] m

-- | Forever listen to messages from a STOMP queue and execute a callback
-- for each incoming message.
--
-- In case of connection failure, will retry indefinitely.
listen :: (FromJSON a, MonadLogger m, MonadMask m, MonadUnliftIO m)
       => Env -> (a -> m ()) -> m ()
listen e callback =
    recoverAll retryPolicy (const listenAction)  
  where
    -- Note [exception handling]
    -- ~~~
    -- The callback might throw an exception, which will be caught by
    -- 'recoverAll'. This will kill and restart the connection, while we could
    -- in theory do better (just throw away the exception without killing
    -- the connection). However, this is supposed to be a very rare case
    -- and it would complicate the code a bit so we don't care.
    --
    -- If the message can't be parsed, this will throw an exception as well
    -- and the message won't be ACK-ed. Thus, invalid JSON will be stuck in
    -- the queue forever. Presumably this is what we want (because then we
    -- might want to handle such messages manually, and ditto for all other
    -- unprocessable messages).
    retryPolicy = constantDelay 3000000
    listenAction =
        withRunInIO $ \runInIO ->
        withConnection' e $ \conn ->
        withReader conn (unpack (e^.queueName))
                        (unpack (e^.queuePath)) [] [] (iconv (e^.queueName)) $ \q ->
            forever $ do
                m <- readQ q
                runInIO $ callback (msgContent m)
                ack conn m

    -- TODO: setup ACK settings and think about it
    -- TODO: check if TLS is automatic or not
    

-------------------------------------------------------------------------------
-- Utilities

iconv :: FromJSON a => Text -> InBound a
iconv queue _ _ _ bs =
    case Aeson.eitherDecode (BL.fromStrict bs) of
        Right x -> pure x
        Left e  -> convertError $
            "Error when parsing message from STOMP queue " <> unpack queue <> ": " <> e

oconv :: ToJSON a => OutBound a
oconv = pure . BL.toStrict . Aeson.encode

jsonType :: MIME.Type
jsonType = MIME.Type (MIME.Application "json") []

-- | Set up a STOMP connection.
withConnection' :: Env -> (Con -> IO a) -> IO a
withConnection' e =
    withConnection (unpack (e^.serverUrl)) (e^.serverPort) config []
  where
    config =
        [ OAuth (unpack user) (unpack pass) | Just (user, pass) <- [e^.auth] ]
