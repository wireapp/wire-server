module Brig.Queue where

import Brig.App
import Brig.Options
import Control.Lens (view)
import Control.Monad.Catch
import Data.Aeson
import Imports
import System.Logger.Class as Log hiding (settings)
import Wire.DeleteQueue.Interpreter
import Wire.Queue.AWS qualified as AWSQueue
import Wire.Queue.Stomp qualified as Stomp

-- | Forever listen to messages coming from a queue and execute a callback
-- for each incoming message.
--
-- See documentation of underlying functions (e.g. 'Stomp.listen') for
-- extra details.
listen ::
  ( Show a,
    FromJSON a,
    MonadLogger m,
    MonadReader Env m,
    MonadMask m,
    MonadUnliftIO m
  ) =>
  QueueEnv ->
  (a -> m ()) ->
  m ()
listen (StompQueueEnv broker queue) callback =
  Stomp.listen broker queue callback
listen (SqsQueueEnv aws queue) callback = do
  throttleMillis <- fromMaybe defSqsThrottleMillis <$> view (settings . sqsThrottleMillis)
  withRunInIO $ \lower -> AWSQueue.execute aws $ AWSQueue.listen throttleMillis queue $ lower . callback
