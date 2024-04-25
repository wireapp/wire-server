module Wire.DeleteQueue.Interpreter
  ( runDeleteQueue,
    mkEnv,
    Env,
  )
where

import Amazonka.SQS.Lens
import Control.Exception (ErrorCall (..))
import Control.Lens
import Data.Aeson
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as BL
import Data.Id
import Data.Text as T
import Data.Text.Encoding qualified as T
import Imports
import OpenSSL.EVP.Digest hiding (digest)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import System.Logger.Class qualified as Log
import Wire.DeleteQueue
import Wire.InternalEvent
import Wire.Queue
import Wire.Queue.AWS qualified as AWS
import Wire.Queue.Stomp qualified as Stomp
import Wire.Sem.Logger

runDeleteQueue ::
  Member (Input Queue) r =>
  Member (Embed IO) r =>
  Member (Input Env) r =>
  Member (Logger (Log.Msg -> Log.Msg)) r =>
  Member (Error ErrorCall) r =>
  InterpreterFor DeleteQueue r
runDeleteQueue = interpret $ \case
  EnqueueUserDeletion userId -> enqueueUserDeletionImpl userId
  EnqueueClientDeletion clientId userId mConnId -> enqueueClientDeletionImpl clientId userId mConnId
  EnqueueServiceDeletion providerId serviceId -> enqueueServiceDeletionImpl providerId serviceId

enqueueUserDeletionImpl ::
  Member (Input Queue) r =>
  Member (Input Env) r =>
  Member (Embed IO) r =>
  Member (Logger (Log.Msg -> Log.Msg)) r =>
  Member (Error ErrorCall) r =>
  UserId ->
  Sem r ()
enqueueUserDeletionImpl uid = do
  queue <- input
  env <- input
  enqueue env queue (DeleteUser uid)

enqueueClientDeletionImpl ::
  Member (Input Queue) r =>
  Member (Input Env) r =>
  Member (Embed IO) r =>
  Member (Logger (Log.Msg -> Log.Msg)) r =>
  Member (Error ErrorCall) r =>
  ClientId ->
  UserId ->
  Maybe ConnId ->
  Sem r ()
enqueueClientDeletionImpl cid uid mConnId = do
  queue <- input
  env <- input
  enqueue env queue (DeleteClient cid uid mConnId)

enqueueServiceDeletionImpl ::
  Member (Input Queue) r =>
  Member (Input Env) r =>
  Member (Embed IO) r =>
  Member (Logger (Log.Msg -> Log.Msg)) r =>
  Member (Error ErrorCall) r =>
  ProviderId ->
  ServiceId ->
  Sem r ()
enqueueServiceDeletionImpl pid sid = do
  queue <- input
  env <- input
  enqueue env queue (DeleteService pid sid)

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

mkEnv :: Maybe Stomp.Broker -> Maybe AWS.Env -> Env
mkEnv = Env

-- | Enqueue a message.
--
-- Throws an error in case of failure.
enqueue ::
  Member (Embed IO) r =>
  Member (Logger (Log.Msg -> Log.Msg)) r =>
  Member (Error ErrorCall) r =>
  ToJSON a =>
  Env ->
  Queue ->
  a ->
  Sem r ()
enqueue env (StompQueue queue) message =
  case env.broker of
    Just broker -> embed @IO $ Stomp.enqueue broker queue message
    Nothing -> do
      err $
        Log.msg (Log.val "Tried to publish a message but STOMP is not configured")
          . Log.field "StompQueue" (show queue)
      throw (ErrorCall "The server couldn't access a queue")
enqueue env (SqsQueue queue) message =
  case env.awsEnv of
    Nothing -> undefined -- throw an error, copy over from prev implementation
    Just awsEnv -> do
      let body = encode message
      md5 <- embed @IO $ getDigestByName "MD5"
      let bodyMD5 = fmap (flip digest body) md5
      resp <- embed @IO $ AWS.execute awsEnv (AWS.enqueueStandard queue body)
      unless (resp ^. sendMessageResponse_mD5OfMessageBody == bodyMD5) $ do
        err $
          Log.msg (Log.val "Returned hash (MD5) doesn't match message hash")
            . Log.field "SqsQueue" (show queue)
            . Log.field "returned_hash" (show (resp ^. sendMessageResponse_mD5OfMessageBody))
            . Log.field "message_hash" (show (Just bodyMD5))
        throw (ErrorCall "The server couldn't access a queue")
  where
    digest :: Digest -> BL.ByteString -> Text
    digest d = T.decodeLatin1 . B16.encode . digestLBS d
