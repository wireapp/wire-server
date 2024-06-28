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

-- | Working with remote queues (like Amazon SQS).
module Brig.Queue
  ( module Brig.Queue.Types,
    enqueue,
    listen,
  )
where

import Amazonka.SQS.Lens (sendMessageResponse_mD5OfMessageBody)
import Brig.App
import Brig.DeleteQueue.Interpreter (QueueEnv (..))
import Brig.Queue.Stomp qualified as Stomp
import Brig.Queue.Types
import Control.Exception (ErrorCall (..))
import Control.Lens (view, (^.))
import Control.Monad.Catch
import Data.Aeson
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as BL
import Data.Text.Encoding qualified as T
import Imports
import OpenSSL.EVP.Digest (Digest, digestLBS)
import System.Logger.Class as Log hiding (settings)
import Wire.API.AWS qualified as AWS

-- Note [queue refactoring]
-- ~~~~~~~~~~~~~~~~
--
-- The way we deal with queues is not the best. There is at least one piece of
-- technical debt here:
--
--   1. 'Queue' is currently used only for the internal events queue, even
--      though we have queues in other places (and not only in Brig). We
--      should move 'Brig.Queue' out of Brig and use it elsewhere too.

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
  QueueEnv ->
  a ->
  m ()
enqueue (StompQueueEnv env queue) message =
  Stomp.enqueue env queue message
enqueue (SqsQueueEnv env _ queue) message = do
  let body = encode message
  bodyMD5 <- digest <$> view digestMD5 <*> pure body
  resp <- AWS.execute env (AWS.enqueueStandard queue body)
  unless (resp ^. sendMessageResponse_mD5OfMessageBody == Just bodyMD5) $ do
    Log.err $
      msg (val "Returned hash (MD5) doesn't match message hash")
        . field "SqsQueue" (show queue)
        . field "returned_hash" (show (resp ^. sendMessageResponse_mD5OfMessageBody))
        . field "message_hash" (show (Just bodyMD5))
    throwM (ErrorCall "The server couldn't access a queue")
  where
    digest :: Digest -> BL.ByteString -> Text
    digest d = T.decodeLatin1 . B16.encode . digestLBS d

-- | Forever listen to messages coming from a queue and execute a callback
-- for each incoming message.
--
-- See documentation of underlying functions (e.g. 'Stomp.listen') for
-- extra details.
listen ::
  ( Show a,
    FromJSON a,
    MonadLogger m,
    MonadMask m,
    MonadUnliftIO m
  ) =>
  QueueEnv ->
  (a -> m ()) ->
  m ()
listen (StompQueueEnv env queue) callback =
  Stomp.listen env queue callback
listen (SqsQueueEnv env throttleMillis queue) callback = do
  withRunInIO $ \lower -> AWS.execute env $ AWS.listen throttleMillis queue $ lower . callback
