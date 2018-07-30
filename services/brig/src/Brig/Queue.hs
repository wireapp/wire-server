{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Working with remote queues (like Amazon SQS).
module Brig.Queue
    ( module Brig.Queue.Types
    , enqueue
    , listen
    ) where

import Brig.App
import Brig.Queue.Types
import Control.Lens (view, (^.))
import Control.Exception (ErrorCall (..))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader (ask)
import Data.Aeson
import Data.Text (Text)
import Network.AWS.SQS (smrsMD5OfMessageBody)
import OpenSSL.EVP.Digest (Digest, digestLBS)
import System.Logger.Class as Log

import qualified Brig.AWS               as AWS
import qualified Brig.Queue.Stomp       as Stomp
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Text.Encoding     as T

-- | Enqueue a message.
--
-- Throws an error in case of failure.
enqueue :: ToJSON a => Queue -> a -> AppIO ()
enqueue (StompQueue queue) message =
    view stompEnv >>= \case
        Just env -> Stomp.enqueue (Stomp.broker env) queue message
        Nothing  -> do
            Log.err $
                msg (val "Tried to publish a message but STOMP is not configured") .
                field "StompQueue" (show queue)
            throwM (ErrorCall "The server couldn't access a queue")
enqueue (SqsQueue queue) message =
    view awsEnv >>= \env -> do
        let body = encode message
        bodyMD5 <- digest <$> view digestMD5 <*> pure body
        resp    <- AWS.execute env (AWS.enqueueStandard queue body)
        unless (resp^.smrsMD5OfMessageBody == Just bodyMD5) $ do
            Log.err $
                msg (val "Returned hash (MD5) doesn't match message hash") .
                field "SqsQueue"      (show queue) .
                field "returned_hash" (show (resp^.smrsMD5OfMessageBody)) .
                field "message_hash"  (show (Just bodyMD5))
            throwM (ErrorCall "The server couldn't access a queue")
  where
    digest :: Digest -> BL.ByteString -> Text
    digest d = T.decodeLatin1 . B16.encode . digestLBS d

-- | Forever listen to messages coming from a queue and execute a callback
-- for each incoming message.
--
-- See documentation of underlying functions (e.g. 'Stomp.listen') for
-- extra details.
listen :: (Show a, FromJSON a) => Queue -> (a -> AppIO ()) -> AppIO ()
listen (StompQueue queue) callback =
    view stompEnv >>= \case
        Just env -> Stomp.listen (Stomp.broker env) queue callback
        Nothing  -> do
            Log.err $
                msg (val "Can't listen on a queue because STOMP is not configured") .
                field "StompQueue" (show queue)
            throwM (ErrorCall "The server couldn't access a queue")
listen (SqsQueue queue) callback = do
    env <- ask
    AWS.execute (env^.awsEnv) $ AWS.listen queue (runAppT env . callback)
