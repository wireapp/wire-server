module Brig.DeleteQueue.Interpreter
  ( runDeleteQueue,
    QueueEnv (..),
  )
where

import Amazonka.SQS.Lens
import Brig.Queue.Stomp qualified as Stomp
import Control.Exception (ErrorCall (..))
import Control.Lens
import Data.Aeson
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as BL
import Data.Text as T
import Data.Text.Encoding qualified as T
import Imports
import OpenSSL.EVP.Digest hiding (digest)
import Polysemy
import Polysemy.Error
import System.Logger.Class qualified as Log
import Wire.API.AWS qualified as AWS
import Wire.DeleteQueue
import Wire.InternalEvent
import Wire.Sem.Logger

-- | The queue environment constructed from `QueueOpts`.
data QueueEnv
  = StompQueueEnv Stomp.Broker Text
  | SqsQueueEnv AWS.Env Int Text

runDeleteQueue ::
  ( Member (Embed IO) r,
    Member (Logger (Log.Msg -> Log.Msg)) r,
    Member (Error ErrorCall) r
  ) =>
  QueueEnv ->
  InterpreterFor DeleteQueue r
runDeleteQueue queueEnv =
  interpret $ \case
    EnqueueUserDeletion userId -> enqueue queueEnv (DeleteUser userId)
    EnqueueClientDeletion clientId userId mConnId -> enqueue queueEnv (DeleteClient clientId userId mConnId)
    EnqueueServiceDeletion providerId serviceId -> enqueue queueEnv (DeleteService providerId serviceId)

-- | Enqueue a message.
--
-- Throws an error in case of failure.
enqueue ::
  ( Member (Embed IO) r,
    Member (Logger (Log.Msg -> Log.Msg)) r,
    Member (Error ErrorCall) r
  ) =>
  (ToJSON a) =>
  QueueEnv ->
  a ->
  Sem r ()
enqueue (StompQueueEnv broker queue) message =
  embed @IO $ Stomp.enqueue broker queue message
enqueue (SqsQueueEnv awsEnv _ queue) message = do
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
