{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.BackgroundJobs where

import Control.Arrow ((&&&))
import Control.Lens (makePrisms)
import Data.Aeson qualified as Aeson
import Data.Id
import Data.Map.Strict qualified as Map
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import Network.AMQP qualified as Q
import Network.AMQP.Types qualified as QT
import Wire.Arbitrary (Arbitrary (..), GenericUniform (..))

data BackgroundJobPayload
  = BackgroundJobSyncUserGroupAndChannel SyncUserGroupAndChannel
  | BackgroundJobSyncUserGroup SyncUserGroup
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform BackgroundJobPayload

backgroundJobPayloadLabel :: BackgroundJobPayload -> Text
backgroundJobPayloadLabel p = case backgroundJobPayloadTag p of
  BackgroundJobSyncUserGroupAndChannelTag -> "sync-user-group-and-channel"
  BackgroundJobSyncUserGroupTag -> "sync-user-group"

data BackgroundJobPayloadTag
  = BackgroundJobSyncUserGroupAndChannelTag
  | BackgroundJobSyncUserGroupTag
  deriving stock (Eq, Ord, Bounded, Enum, Show, Generic)
  deriving (Arbitrary) via GenericUniform BackgroundJobPayloadTag

instance ToSchema BackgroundJobPayloadTag where
  schema =
    enum @Text $
      mconcat
        [ element "sync-user-group-and-channel" BackgroundJobSyncUserGroupAndChannelTag,
          element "sync-user-group" BackgroundJobSyncUserGroupTag
        ]

backgroundJobPayloadTag :: BackgroundJobPayload -> BackgroundJobPayloadTag
backgroundJobPayloadTag =
  \case
    BackgroundJobSyncUserGroupAndChannel {} -> BackgroundJobSyncUserGroupAndChannelTag
    BackgroundJobSyncUserGroup {} -> BackgroundJobSyncUserGroupTag

backgroundJobPayloadTagSchema :: ObjectSchema SwaggerDoc BackgroundJobPayloadTag
backgroundJobPayloadTagSchema = field "type" schema

data SyncUserGroupAndChannel = SyncUserGroupAndChannel
  { teamId :: TeamId,
    userGroupId :: UserGroupId,
    convId :: ConvId,
    actor :: Maybe UserId
  }
  deriving (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema SyncUserGroupAndChannel)
  deriving (Arbitrary) via GenericUniform SyncUserGroupAndChannel

instance ToSchema SyncUserGroupAndChannel where
  schema =
    object $
      SyncUserGroupAndChannel
        <$> (.teamId) .= field "team_id" schema
        <*> (.userGroupId) .= field "user_group_id" schema
        <*> (.convId) .= field "conv_id" schema
        <*> (.actor) .= maybe_ (optField "actor" schema)

data SyncUserGroup = SyncUserGroup
  { teamId :: TeamId,
    userGroupId :: UserGroupId,
    actor :: Maybe UserId
  }
  deriving (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema SyncUserGroup)
  deriving (Arbitrary) via GenericUniform SyncUserGroup

instance ToSchema SyncUserGroup where
  schema =
    object $
      SyncUserGroup
        <$> (.teamId) .= field "team_id" schema
        <*> (.userGroupId) .= field "user_group_id" schema
        <*> (.actor) .= maybe_ (optField "actor" schema)

makePrisms ''BackgroundJobPayload

backgroundJobPayloadObjectSchema :: ObjectSchema SwaggerDoc BackgroundJobPayload
backgroundJobPayloadObjectSchema =
  snd
    <$> (backgroundJobPayloadTag &&& id)
      .= bind
        (fst .= backgroundJobPayloadTagSchema)
        (snd .= dispatch backgroundJobPayloadDataSchema)
  where
    backgroundJobPayloadDataSchema :: BackgroundJobPayloadTag -> ObjectSchema SwaggerDoc BackgroundJobPayload
    backgroundJobPayloadDataSchema = \case
      BackgroundJobSyncUserGroupAndChannelTag -> tag _BackgroundJobSyncUserGroupAndChannel (field "payload" schema)
      BackgroundJobSyncUserGroupTag -> tag _BackgroundJobSyncUserGroup (field "payload" schema)

instance ToSchema BackgroundJobPayload where
  schema = object backgroundJobPayloadObjectSchema

deriving via (Schema BackgroundJobPayload) instance Aeson.FromJSON BackgroundJobPayload

deriving via (Schema BackgroundJobPayload) instance Aeson.ToJSON BackgroundJobPayload

deriving via (Schema BackgroundJobPayload) instance S.ToSchema BackgroundJobPayload

-- | Background job envelope. Payload is a free-form JSON object.
data BackgroundJob = BackgroundJob
  { jobId :: JobId,
    requestId :: RequestId,
    payload :: BackgroundJobPayload
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform BackgroundJob
  deriving (Aeson.ToJSON, Aeson.FromJSON, S.ToSchema) via Schema BackgroundJob

instance ToSchema BackgroundJob where
  schema =
    object $
      BackgroundJob
        <$> jobId .= field "id" schema
        <*> requestId .= field "requestId" schema
        <*> payload .= field "payload" schema

backgroundJobsRoutingKey :: Text
backgroundJobsRoutingKey = backgroundJobsQueueName

backgroundJobsQueueName :: Text
backgroundJobsQueueName = "background-jobs"

ensureBackgroundJobsQueue :: Q.Channel -> IO ()
ensureBackgroundJobsQueue chan = do
  let headers =
        QT.FieldTable
          ( Map.fromList
              [ ("x-queue-type", QT.FVString "quorum")
              ]
          )
      q =
        Q.newQueue
          { Q.queueName = backgroundJobsQueueName,
            Q.queueDurable = True,
            Q.queueAutoDelete = False,
            Q.queueExclusive = False,
            Q.queueHeaders = headers
          }
  void $ Q.declareQueue chan q
