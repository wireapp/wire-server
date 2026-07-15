{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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

import Data.Aeson (FromJSON, ToJSON, Value (Null), parseJSON, toJSON)
import Data.Id
import Data.Json.Util
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import Test.QuickCheck (Arbitrary (..))

-- | Shared queue name for the scheduled meetings cleanup job.
meetingsCleanupQueueName :: Text
meetingsCleanupQueueName = "meetings_cleanup_jobs"

-- | Shared queue name for the adminless deletion job.
adminlessDeletionQueueName :: Text
adminlessDeletionQueueName = "adminless_deletion_jobs"

-- | Shared queue name for the adminless reminder job.
adminlessReminderQueueName :: Text
adminlessReminderQueueName = "adminless_reminder_jobs"

-- | Empty payload because the schedule itself carries all execution context.
data MeetingsCleanupJob = MeetingsCleanupJob
  deriving stock (Eq, Generic, Show)

instance Arbitrary MeetingsCleanupJob where
  arbitrary = pure MeetingsCleanupJob

instance ToJSON MeetingsCleanupJob where
  toJSON MeetingsCleanupJob = Null

instance FromJSON MeetingsCleanupJob where
  parseJSON Null = pure MeetingsCleanupJob
  parseJSON _ = fail "MeetingsCleanupJob expects null"

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

-- | Registry for the scheduled jobs we expose via Arbiter.
type ScheduledJobsRegistry =
  '[ '("meetings_cleanup_jobs", MeetingsCleanupJob),
     '("adminless_deletion_jobs", AdminlessDeletionJob),
     '("adminless_reminder_jobs", AdminlessReminderJob)
   ]
