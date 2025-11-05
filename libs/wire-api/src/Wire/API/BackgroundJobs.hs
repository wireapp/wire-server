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

data JobPayload
  = JobSyncUserGroupAndChannel SyncUserGroupAndChannel
  | JobSyncUserGroup SyncUserGroup
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform JobPayload

jobPayloadLabel :: JobPayload -> Text
jobPayloadLabel p = case jobPayloadTag p of
  JobSyncUserGroupAndChannelTag -> "sync-user-group-and-channel"
  JobSyncUserGroupTag -> "sync-user-group"

data JobPayloadTag
  = JobSyncUserGroupAndChannelTag
  | JobSyncUserGroupTag
  deriving stock (Eq, Ord, Bounded, Enum, Show, Generic)
  deriving (Arbitrary) via GenericUniform JobPayloadTag

instance ToSchema JobPayloadTag where
  schema =
    enum @Text "JobPayloadTag" $
      mconcat
        [ element "sync-user-group-and-channel" JobSyncUserGroupAndChannelTag,
          element "sync-user-group" JobSyncUserGroupTag
        ]

jobPayloadTag :: JobPayload -> JobPayloadTag
jobPayloadTag =
  \case
    JobSyncUserGroupAndChannel {} -> JobSyncUserGroupAndChannelTag
    JobSyncUserGroup {} -> JobSyncUserGroupTag

jobPayloadTagSchema :: ObjectSchema SwaggerDoc JobPayloadTag
jobPayloadTagSchema = field "type" schema

data SyncUserGroupAndChannel = SyncUserGroupAndChannel
  { teamId :: TeamId,
    userGroupId :: UserGroupId,
    convId :: ConvId,
    actor :: UserId
  }
  deriving (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema SyncUserGroupAndChannel)
  deriving (Arbitrary) via GenericUniform SyncUserGroupAndChannel

instance ToSchema SyncUserGroupAndChannel where
  schema =
    object "SyncUserGroupAndChannel" $
      SyncUserGroupAndChannel
        <$> (.teamId) .= field "team_id" schema
        <*> (.userGroupId) .= field "user_group_id" schema
        <*> (.convId) .= field "conv_id" schema
        <*> (.actor) .= field "actor" schema

data SyncUserGroup = SyncUserGroup
  { teamId :: TeamId,
    userGroupId :: UserGroupId,
    actor :: UserId
  }
  deriving (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema SyncUserGroup)
  deriving (Arbitrary) via GenericUniform SyncUserGroup

instance ToSchema SyncUserGroup where
  schema =
    object "SyncUserGroup" $
      SyncUserGroup
        <$> (.teamId) .= field "team_id" schema
        <*> (.userGroupId) .= field "user_group_id" schema
        <*> (.actor) .= field "actor" schema

makePrisms ''JobPayload

jobPayloadObjectSchema :: ObjectSchema SwaggerDoc JobPayload
jobPayloadObjectSchema =
  snd
    <$> (jobPayloadTag &&& id)
      .= bind
        (fst .= jobPayloadTagSchema)
        (snd .= dispatch jobPayloadDataSchema)
  where
    jobPayloadDataSchema :: JobPayloadTag -> ObjectSchema SwaggerDoc JobPayload
    jobPayloadDataSchema = \case
      JobSyncUserGroupAndChannelTag -> tag _JobSyncUserGroupAndChannel (field "payload" schema)
      JobSyncUserGroupTag -> tag _JobSyncUserGroup (field "payload" schema)

instance ToSchema JobPayload where
  schema = object "JobPayload" jobPayloadObjectSchema

deriving via (Schema JobPayload) instance Aeson.FromJSON JobPayload

deriving via (Schema JobPayload) instance Aeson.ToJSON JobPayload

deriving via (Schema JobPayload) instance S.ToSchema JobPayload

-- | Background job envelope. Payload is a free-form JSON object.
data Job = Job
  { jobId :: JobId,
    requestId :: RequestId,
    payload :: JobPayload
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform Job
  deriving (Aeson.ToJSON, Aeson.FromJSON, S.ToSchema) via Schema Job

instance ToSchema Job where
  schema =
    object "Job" $
      Job
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
