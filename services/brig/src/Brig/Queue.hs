module Brig.Queue where

import Brig.AWS qualified as AWS
import Brig.App
import Brig.Options
import Control.Exception (ErrorCall (..))
import Control.Lens (view, (^.))
import Control.Monad.Catch
import Data.Aeson
import Imports
import System.Logger.Class as Log hiding (settings)
import Wire.Queue
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
  Queue ->
  (a -> m ()) ->
  m ()
listen (StompQueue queue) callback =
  view stompEnv >>= \case
    Just env -> Stomp.listen (Stomp.broker env) queue callback
    Nothing -> do
      Log.err $
        msg (val "Can't listen on a queue because STOMP is not configured")
          . field "StompQueue" (show queue)
      throwM (ErrorCall "The server couldn't access a queue")
listen (SqsQueue queue) callback = do
  env <- ask
  throttleMillis <- fromMaybe defSqsThrottleMillis <$> view (settings . sqsThrottleMillis)
  withRunInIO $ \lower -> AWS.execute (env ^. awsEnv) $ AWS.listen throttleMillis queue $ lower . callback
