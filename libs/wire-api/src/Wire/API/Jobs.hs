{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
-- for more details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.API.Jobs where

import Arbiter.Core.Job.Types (JobRead)
import Control.Arrow ((&&&))
import Control.Lens (makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Id
import Data.Json.Util
import Data.OpenApi qualified as S
import Data.Proxy
import Data.Schema
import Data.Text as Text
import GHC.TypeLits
import Imports
import Test.QuickCheck (oneof)
import Wire.Arbitrary (Arbitrary (..), GenericUniform (..))

-- | All scheduled jobs share one physical Arbiter queue. The payload tag keeps
-- the logical job type explicit without requiring one queue per job type.
type ScheduledJobsQueueName = "scheduled_jobs"

scheduledJobsQueueName :: Text
scheduledJobsQueueName = Text.pack $ symbolVal (Proxy @ScheduledJobsQueueName)

-- | Empty payload because the schedule itself carries all execution context.
data MeetingsCleanupJob = MeetingsCleanupJob
  deriving stock (Eq, Generic, Show)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema MeetingsCleanupJob)

instance ToSchema MeetingsCleanupJob where
  schema = object $ pure MeetingsCleanupJob

instance Arbitrary MeetingsCleanupJob where
  arbitrary = pure MeetingsCleanupJob

-- | Payload for adminless deletions.
-- Arbiter persists these payloads and workers decode them later, so changes to
-- field names or shapes require a coordinated rollout. The origin user is
-- optional for jobs created by system reconciliation; the request ID is always
-- captured when a job is scheduled.
data AdminlessDeletionJob = AdminlessDeletionJob
  { adminlessDeletionJobTeamId :: TeamId,
    adminlessDeletionJobConversationId :: ConvId,
    adminlessDeletionJobOrigUserId :: Maybe UserId,
    adminlessDeletionJobRequestId :: RequestId
  }
  deriving stock (Eq, Generic, Show)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema AdminlessDeletionJob)

instance Arbitrary AdminlessDeletionJob where
  arbitrary = AdminlessDeletionJob <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance ToSchema AdminlessDeletionJob where
  schema =
    object $
      AdminlessDeletionJob
        <$> (.adminlessDeletionJobTeamId) .= field "team_id" schema
        <*> (.adminlessDeletionJobConversationId) .= field "conversation_id" schema
        <*> (.adminlessDeletionJobOrigUserId) .= maybe_ (optField "orig_user_id" schema)
        <*> (.adminlessDeletionJobRequestId) .= field "request_id" schema

-- | Payload for adminless reminders.
-- Arbiter persists these payloads and workers decode them later, so changes to
-- field names or shapes require a coordinated rollout. The origin user is
-- optional for jobs created by system reconciliation; the request ID is always
-- captured when a job is scheduled.
data AdminlessReminderJob = AdminlessReminderJob
  { adminlessReminderJobTeamId :: TeamId,
    adminlessReminderJobConversationId :: ConvId,
    adminlessReminderJobOrigUserId :: Maybe UserId,
    adminlessReminderJobDeletionScheduledFor :: UTCTimeMillis,
    adminlessReminderJobRequestId :: RequestId
  }
  deriving stock (Eq, Generic, Show)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema AdminlessReminderJob)

instance Arbitrary AdminlessReminderJob where
  arbitrary = AdminlessReminderJob <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance ToSchema AdminlessReminderJob where
  schema =
    object $
      AdminlessReminderJob
        <$> (.adminlessReminderJobTeamId) .= field "team_id" schema
        <*> (.adminlessReminderJobConversationId) .= field "conversation_id" schema
        <*> (.adminlessReminderJobOrigUserId) .= maybe_ (optField "orig_user_id" schema)
        <*> (.adminlessReminderJobDeletionScheduledFor) .= field "deletion_scheduled_for" schema
        <*> (.adminlessReminderJobRequestId) .= field "request_id" schema

-- | Tagged payload persisted in Arbiter's JSONB queue. Keep the type tags and
-- nested data shape stable when changing scheduled jobs.
data ScheduledJobPayload
  = MeetingsCleanup MeetingsCleanupJob
  | AdminlessDeletion AdminlessDeletionJob
  | AdminlessReminder AdminlessReminderJob
  deriving stock (Eq, Generic, Show)

data ScheduledJobPayloadTag
  = MeetingsCleanupTag
  | AdminlessReminderTag
  | AdminlessDeletionTag
  deriving stock (Eq, Ord, Bounded, Enum, Show, Generic)
  deriving (Arbitrary) via GenericUniform ScheduledJobPayloadTag

instance ToSchema ScheduledJobPayloadTag where
  schema =
    enum @Text $
      mconcat
        [ element "meetings_cleanup" MeetingsCleanupTag,
          element "adminless_deletion" AdminlessDeletionTag,
          element "adminless_reminder" AdminlessReminderTag
        ]

makePrisms ''ScheduledJobPayload

scheduledJobPayloadObjectSchema :: ObjectSchema SwaggerDoc ScheduledJobPayload
scheduledJobPayloadObjectSchema =
  snd <$> (toTag &&& id) .= bind (fst .= tagObjectSchema) (snd .= dispatch toSchema)
  where
    tagObjectSchema :: ObjectSchema SwaggerDoc ScheduledJobPayloadTag
    tagObjectSchema = field "type" schema

    toTag :: ScheduledJobPayload -> ScheduledJobPayloadTag
    toTag =
      \case
        MeetingsCleanup {} -> MeetingsCleanupTag
        AdminlessDeletion {} -> AdminlessDeletionTag
        AdminlessReminder {} -> AdminlessReminderTag

    toSchema :: ScheduledJobPayloadTag -> ObjectSchema SwaggerDoc ScheduledJobPayload
    toSchema = \case
      MeetingsCleanupTag -> tag _MeetingsCleanup (field "data" schema)
      AdminlessDeletionTag -> tag _AdminlessDeletion (field "data" schema)
      AdminlessReminderTag -> tag _AdminlessReminder (field "data" schema)

instance ToSchema ScheduledJobPayload where
  schema = object scheduledJobPayloadObjectSchema

deriving via (Schema ScheduledJobPayload) instance FromJSON ScheduledJobPayload

deriving via (Schema ScheduledJobPayload) instance ToJSON ScheduledJobPayload

deriving via (Schema ScheduledJobPayload) instance S.ToSchema ScheduledJobPayload

instance Arbitrary ScheduledJobPayload where
  arbitrary = oneof [MeetingsCleanup <$> arbitrary, AdminlessDeletion <$> arbitrary, AdminlessReminder <$> arbitrary]

-- | Registry for the scheduled jobs we expose via Arbiter.
type ScheduledJobsRegistry =
  '[ '(ScheduledJobsQueueName, ScheduledJobPayload)
   ]

data JobWorkerHandlers = JobWorkerHandlers
  { scheduledJobsRunMeetingsCleanup :: MeetingsCleanupJob -> IO (),
    scheduledJobsRunAdminlessDeletion :: JobRead AdminlessDeletionJob -> IO (),
    scheduledJobsRunAdminlessReminder :: JobRead AdminlessReminderJob -> IO ()
  }
