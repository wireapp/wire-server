{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Wire.API.UserGroup.BackgroundJobs where

import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson qualified as A
import Data.Id
import Data.Map qualified as Map
import Data.Schema
import Imports
import Network.AMQP qualified as Q
import Network.AMQP.Types qualified as Q

-- | NOTE: Stored in RabbitMQ, any changes to serialization of this object could cause
-- notifications to get lost.
data SyncUserGroupAndChannel = SyncUserGroupAndChannel
  { userGroupId :: UserGroupId,
    convId :: ConvId,
    actor :: UserId
  }
  deriving (Show, Eq)
  deriving (A.ToJSON, A.FromJSON) via (Schema SyncUserGroupAndChannel)

instance ToSchema SyncUserGroupAndChannel where
  schema =
    object "SyncUserGroupAndChannel" $
      SyncUserGroupAndChannel
        <$> (.userGroupId) .= field "userGroupId" schema
        <*> (.convId) .= field "convId" schema
        <*> (.actor) .= field "actor" schema

-- | NOTE: Stored in RabbitMQ, any changes to serialization of this object could cause
-- notifications to get lost.
data SyncUserGroup = SyncUserGroup
  { userGroupId :: UserGroupId,
    actor :: UserId
  }
  deriving (Show, Eq)
  deriving (A.ToJSON, A.FromJSON) via (Schema SyncUserGroup)

instance ToSchema SyncUserGroup where
  schema =
    object "SyncUserGroup" $
      SyncUserGroup
        <$> (.userGroupId) .= field "userGroupId" schema
        <*> (.actor) .= field "actor" schema

sendSyncUserGroupAndChannel ::
  (MonadIO m) =>
  BackgroundJobsEnv ->
  UserGroupId ->
  ConvId ->
  UserId ->
  m ()
sendSyncUserGroupAndChannel env userGroupId convId actor = liftIO $ do
  let msg =
        Q.newMsg
          { Q.msgBody = A.encode SyncUserGroupAndChannel {..},
            Q.msgDeliveryMode = Just env.deliveryMode,
            Q.msgContentType = Just "application/json"
          }
      -- Empty string means default exchange
      exchange = ""
  ensureQueue env.channel routingKeySyncUserGroupAndChannel
  void $ Q.publishMsg env.channel exchange routingKeySyncUserGroupAndChannel msg

routingKeySyncUserGroupAndChannel :: Text
routingKeySyncUserGroupAndChannel = "background-job.sync-user-group-and-channel"

sendSyncUserGroup ::
  (MonadIO m) =>
  BackgroundJobsEnv ->
  UserGroupId ->
  UserId ->
  m ()
sendSyncUserGroup env userGroupId actor = liftIO $ do
  let msg =
        Q.newMsg
          { Q.msgBody = A.encode SyncUserGroup {..},
            Q.msgDeliveryMode = Just env.deliveryMode,
            Q.msgContentType = Just "application/json"
          }
      -- Empty string means default exchange
      exchange = ""
  ensureQueue env.channel routingKeySyncUserGroup
  void $ Q.publishMsg env.channel exchange routingKeySyncUserGroup msg

routingKeySyncUserGroup :: Text
routingKeySyncUserGroup = "background-job.sync-user-group"

-- | If you ever change this function and modify
-- queue parameters, know that it will start failing in the
-- next release! So be prepared to write migrations.
ensureQueue :: Q.Channel -> Text -> IO ()
ensureQueue chan queue = do
  let opts =
        Q.QueueOpts
          { Q.queueName = queue,
            Q.queuePassive = False,
            Q.queueDurable = True,
            Q.queueExclusive = False,
            Q.queueAutoDelete = False,
            Q.queueHeaders =
              Q.FieldTable $
                Map.fromList
                  -- single-active-consumer is used because it is order
                  -- preserving, especially into databases and to remote servers,
                  -- exactly what we are doing here!
                  -- Without single active consumer, messages will be delivered
                  -- round-robbin to all consumers, but then we lose effect-ordering
                  -- due to processing and network times.
                  [ ("x-single-active-consumer", Q.FVBool True),
                    ("x-queue-type", Q.FVString "quorum")
                  ]
          }
  void $ Q.declareQueue chan opts

-- * Internal machinery

data BackgroundJobsEnv = BackgroundJobsEnv
  { channel :: Q.Channel,
    deliveryMode :: Q.DeliveryMode
  }

data EnqueueError = EnqueueError String
  deriving (Show)

instance Exception EnqueueError
