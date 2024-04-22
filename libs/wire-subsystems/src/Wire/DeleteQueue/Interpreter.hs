module Wire.DeleteQueue.Interpreter (runDeleteQueue) where

import Control.Exception (ErrorCall (..))
import Control.Monad.Catch
import Data.Aeson
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as BL
import Data.Text as T
import Data.Text.Encoding qualified as T
import Imports
import OpenSSL.EVP.Digest
import Polysemy
import System.Logger.Class
import System.Logger.Class as Log hiding (settings)
import Wire.DeleteQueue
import Wire.InternalEvent
import Wire.Queue
import Wire.Queue.AWS qualified as AWS
import Wire.Queue.Stomp qualified as Stomp

runDeleteQueue :: InterpreterFor DeleteQueue r
runDeleteQueue = interpret $ \case
  EnqueueUserDeletion userId -> enqueueUserDeletionImp userId
  EnqueueClientDeletion clientId userId mConnId -> enqueueClientDeletionImp clientId userId mConnId
  EnqueueServiceDeletion providerId serviceId -> enqueueServiceDeletionImp providerId serviceId

enqueueUserDeletionImp :: a
enqueueUserDeletionImp uid = do
  queue <- asks internalEventsQueue
  embed @IO $ enqueue queue (DeleteUser uid)

enqueueClientDeletionImp :: a
enqueueClientDeletionImp = undefined

enqueueServiceDeletionImp :: a
enqueueServiceDeletionImp = undefined

-- Note [queue refactoring] -- 2018-08-16
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

data Env = Env
  { broker :: Maybe Stomp.Broker,
    awsEnv :: Maybe AWS.Env
  }

-- | Enqueue a message.
--
-- Throws an error in case of failure.
enqueue ::
  ( MonadReader Env m,
    ToJSON a,
    MonadIO m,
    MonadLogger m,
    MonadThrow m
  ) =>
  Queue ->
  a ->
  m ()
enqueue (StompQueue queue) message =
  asks broker >>= \case
    Just broker -> Stomp.enqueue broker queue message
    Nothing -> do
      Log.err $
        msg (val "Tried to publish a message but STOMP is not configured")
          . field "StompQueue" (show queue)
      throwM (ErrorCall "The server couldn't access a queue")
enqueue (SqsQueue queue) message =
  asks awsEnv >>= \case
    Nothing -> undefined -- throw an error, copy over from prev implementation
    Just env -> do
      let body = encode message
      md5 <- liftIO $ getDigestByName "MD5"
      let bodyMD5 = fmap (flip digest body) md5
      resp <- AWS.execute env (AWS.enqueueStandard queue body)
      unless (resp.sendMessageResponse_mD5OfMessageBody == bodyMD5) $ do
        Log.err $
          msg (val "Returned hash (MD5) doesn't match message hash")
            . field "SqsQueue" (show queue)
            . field "returned_hash" (show (resp.sendMessageResponse_mD5OfMessageBody))
            . field "message_hash" (show (Just bodyMD5))
        throwM (ErrorCall "The server couldn't access a queue")
  where
    digest :: Digest -> BL.ByteString -> Text
    digest d = T.decodeLatin1 . B16.encode . digestLBS d
