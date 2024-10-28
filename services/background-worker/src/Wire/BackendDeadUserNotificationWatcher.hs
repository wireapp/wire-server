{-# LANGUAGE BlockArguments #-}

module Wire.BackendDeadUserNotificationWatcher where

import Cassandra
import Control.Error.Util (hush)
import Control.Monad.Trans.Maybe
import Data.ByteString.Conversion
import Data.Domain
import Data.Id
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Imports
import Network.AMQP qualified as Q
import Network.AMQP.Extended
import Network.AMQP.Lifted qualified as QL
import UnliftIO
import Wire.BackgroundWorker.Env

type DomainConsumerMap = IORef (Map Domain (Q.ConsumerTag, MVar ()))

startWatcher :: DomainConsumerMap -> Q.Channel -> AppT IO ()
startWatcher _consumersRef chan = do
  markAsWorking BackendDeadUserNoticationWatcher

  cassandra <- asks (.cassandra)

  -- This ensures that we receive notifications 1 by 1 which ensures they are
  -- delivered in order.
  lift $ Q.qos chan 0 1 False

  -- TODO: replace bare string with the constant from the rabbitmq PR.
  let queueName = "dead-user-notifications"

  void . lift $ Q.declareQueue chan Q.newQueue {Q.queueName = queueName}
  void $ QL.consumeMsgs chan queueName Q.Ack $ \(_msg, envelope) ->
    let vals = Text.splitOn "." envelope.envRoutingKey
     in case vals of
          [uidText, cidText] -> do
            m <- runMaybeT $ do
              uid <- hoistMaybe $ hush $ parseIdFromText uidText
              cid <- hoistMaybe $ fromByteString $ Text.encodeUtf8 cidText
              pure (uid, cid)
            (uid, cid) <- maybe handleParseErrors pure m
            markAsNeedsFullSync cassandra uid cid

          -- TODO:
          _ -> error "invalid routing key, don't throw an error here, just log things"
  where
    -- TODO: use the logger instead of error
    handleParseErrors = error "Log: could not parse a user and client id from routing key."

markAsNeedsFullSync :: ClientState -> UserId -> ClientId -> AppT IO ()
markAsNeedsFullSync cassandra uid cid = do
  runClient cassandra do
    retry x1 $ write missedNotifications (params LocalQuorum (uid, cid))
  where
    missedNotifications :: PrepQuery W (UserId, ClientId) ()
    missedNotifications =
      [sql|
          INSERT INTO missed_notifications (user_id, client_id)
          VALUES (?, ?)
      |]

startWorker ::
  AmqpEndpoint ->
  AppT IO (IORef (Maybe Q.Channel), DomainConsumerMap)
startWorker rabbitmqOpts = do
  env <- ask
  -- These are used in the POSIX signal handlers, so we need to make
  -- cross thread references that we can use to cancel consumers and
  -- wait for current processing steps to finish.
  chanRef <- newIORef Nothing
  consumersRef <- newIORef mempty
  let -- cleanup the refs when channels die
      -- This is so we aren't trying to close consumers
      -- that don't exist when the service is shutdown.
      clearRefs = do
        atomicWriteIORef chanRef Nothing
        atomicWriteIORef consumersRef mempty
  void $
    async $
      liftIO $
        openConnectionWithRetries env.logger rabbitmqOpts $
          RabbitMqHooks
            { -- The exception handling in `openConnectionWithRetries` won't open a new
              -- connection on an explicit close call.
              onNewChannel = \chan -> do
                atomicWriteIORef chanRef $ pure chan
                runAppT env $ startWatcher consumersRef chan,
              onChannelException = \_ -> do
                clearRefs
                runAppT env $ markAsNotWorking BackendDeadUserNoticationWatcher,
              onConnectionClose = do
                clearRefs
                runAppT env $ markAsNotWorking BackendDeadUserNoticationWatcher
            }
  pure (chanRef, consumersRef)
