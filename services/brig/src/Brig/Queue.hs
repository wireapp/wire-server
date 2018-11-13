{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Working with remote queues (like Amazon SQS).
module Brig.Queue
    ( module Brig.Queue.Types
    , enqueue
    , listen
    ) where

import Imports
import Brig.App
import Brig.Queue.Types
import Control.Lens (view, (^.))
import Control.Exception (ErrorCall (..))
import Control.Monad.Catch
import Data.Aeson
import Network.AWS.SQS (smrsMD5OfMessageBody)
import OpenSSL.EVP.Digest (Digest, digestLBS)
import System.Logger.Class as Log

import qualified Brig.AWS               as AWS
import qualified Brig.Queue.Stomp       as Stomp
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Text.Encoding     as T

-- Note [queue refactoring]
-- ~~~~~~~~~~~~~~~~
--
-- The way we deal with queues is not the best. There are two pieces of
-- technical debt here:
--
--   1. 'Queue' is currently used only for the internal events queue, even
--      though we have queues in other places (and not only in Brig). We
--      should move 'Brig.Queue' out of Brig and use it elsewhere too.
--
--   2. If the 'Queue' is an SqsQueue, it has to be "resolved" before it can
--      be used (we do that in 'newEnv', for instance). Ideally the 'Queue'
--      should be a self-contained reference to a queue, with no 'Broker' or
--      'Stomp.Env' needed to use it; we can still have 'Stomp.Env' in our
--      configs, but it should disappear after the config is read.
--      Similarly, for SqsQueues we should store the queue URL in the
--      config, and not queue name, because AWS documentation suggests that
--      the URL should actually be considered the canonical queue
--      identifier.

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
