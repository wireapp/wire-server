{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
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
import Data.Int qualified as Int
import Data.Json.Util
import Data.OpenApi qualified as S
import Data.Schema
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Imports
import Test.QuickCheck (Arbitrary (..), elements)
import Wire.API.PostgresMarshall

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
-- Keep the JSON encoding backwards compatible. The jobs API reads these payloads
-- back from Arbiter, so changing field names or shapes without a migration will
-- break job listing and management.
data AdminlessDeletionJob = AdminlessDeletionJob
  { adminlessDeletionJobTeamId :: TeamId,
    adminlessDeletionJobConversationId :: ConvId,
    adminlessDeletionJobOrigUserId :: UserId
  }
  deriving stock (Eq, Generic, Show)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema AdminlessDeletionJob)

instance Arbitrary AdminlessDeletionJob where
  arbitrary = AdminlessDeletionJob <$> arbitrary <*> arbitrary <*> arbitrary

instance ToSchema AdminlessDeletionJob where
  schema =
    object $
      AdminlessDeletionJob
        <$> (.adminlessDeletionJobTeamId) .= field "team_id" schema
        <*> (.adminlessDeletionJobConversationId) .= field "conversation_id" schema
        <*> (.adminlessDeletionJobOrigUserId) .= field "orig_user_id" schema

-- | Payload for adminless reminders.
-- Keep the JSON encoding backwards compatible. The jobs API reads these payloads
-- back from Arbiter, so changing field names or shapes without a migration will
-- break job listing and management.
data AdminlessReminderJob = AdminlessReminderJob
  { adminlessReminderJobTeamId :: TeamId,
    adminlessReminderJobConversationId :: ConvId,
    adminlessReminderJobOrigUserId :: UserId,
    adminlessReminderJobDeletionScheduledFor :: UTCTimeMillis
  }
  deriving stock (Eq, Generic, Show)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema AdminlessReminderJob)

instance Arbitrary AdminlessReminderJob where
  arbitrary = AdminlessReminderJob <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance ToSchema AdminlessReminderJob where
  schema =
    object $
      AdminlessReminderJob
        <$> (.adminlessReminderJobTeamId) .= field "team_id" schema
        <*> (.adminlessReminderJobConversationId) .= field "conversation_id" schema
        <*> (.adminlessReminderJobOrigUserId) .= field "orig_user_id" schema
        <*> (.adminlessReminderJobDeletionScheduledFor) .= field "deletion_scheduled_for" schema

-- | The generic scheduled-job families we currently need to persist.
data ScheduledJobKind
  = AdminlessReminder
  | AdminlessDeletion
  | AdminlessSetup
  | AdminlessTeardown
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema ScheduledJobKind)

instance Arbitrary ScheduledJobKind where
  arbitrary = elements [minBound .. maxBound]

instance ToSchema ScheduledJobKind where
  schema =
    enum @Text $
      mconcat
        [ element "adminless-reminder" AdminlessReminder,
          element "adminless-deletion" AdminlessDeletion,
          element "adminless-setup" AdminlessSetup,
          element "adminless-teardown" AdminlessTeardown
        ]

scheduledJobKindToInt :: ScheduledJobKind -> Int
scheduledJobKindToInt = fromEnum

scheduledJobKindFromInt :: Int -> Maybe ScheduledJobKind
scheduledJobKindFromInt n
  | n < fromEnum (minBound :: ScheduledJobKind) = Nothing
  | n > fromEnum (maxBound :: ScheduledJobKind) = Nothing
  | otherwise = Just (toEnum n)

-- | App-level metadata stored alongside Arbiter's runtime state.
data ScheduledJob = ScheduledJob
  { scheduledJobId :: ScheduledJobId,
    scheduledJobKind :: ScheduledJobKind,
    scheduledJobTeamId :: TeamId,
    scheduledJobConversationId :: Maybe ConvId,
    scheduledJobScheduledFor :: UTCTime
  }
  deriving stock (Eq, Generic, Show)

instance PostgresMarshall Int.Int32 ScheduledJobKind where
  postgresMarshall = fromIntegral . scheduledJobKindToInt

instance PostgresUnmarshall Int.Int32 ScheduledJobKind where
  postgresUnmarshall n =
    maybe (Left "invalid scheduled job kind") Right $
      scheduledJobKindFromInt (fromIntegral n)

instance PostgresMarshall (ScheduledJobId, Int.Int32, TeamId, Maybe ConvId, UTCTime) ScheduledJob where
  postgresMarshall ScheduledJob {..} =
    ( scheduledJobId,
      postgresMarshall scheduledJobKind,
      scheduledJobTeamId,
      scheduledJobConversationId,
      scheduledJobScheduledFor
    )

instance PostgresUnmarshall (ScheduledJobId, Int.Int32, TeamId, Maybe ConvId, UTCTime) ScheduledJob where
  postgresUnmarshall (jobId, jobKind, teamId, conversationId, scheduledFor) =
    ScheduledJob
      <$> postgresUnmarshall jobId
      <*> postgresUnmarshall jobKind
      <*> postgresUnmarshall teamId
      <*> postgresUnmarshall conversationId
      <*> postgresUnmarshall scheduledFor

instance PostgresMarshall (UUID, Int.Int32, UUID, Maybe UUID, UTCTime) ScheduledJob where
  postgresMarshall ScheduledJob {..} =
    ( toUUID scheduledJobId,
      postgresMarshall scheduledJobKind,
      toUUID scheduledJobTeamId,
      toUUID <$> scheduledJobConversationId,
      scheduledJobScheduledFor
    )

instance PostgresUnmarshall (UUID, Int.Int32, UUID, Maybe UUID, UTCTime) ScheduledJob where
  postgresUnmarshall (jobId, jobKind, teamId, conversationId, scheduledFor) =
    ScheduledJob
      <$> postgresUnmarshall jobId
      <*> postgresUnmarshall jobKind
      <*> postgresUnmarshall teamId
      <*> postgresUnmarshall conversationId
      <*> pure scheduledFor

-- | Registry for the scheduled jobs we expose via Arbiter.
type ScheduledJobsRegistry =
  '[ '("meetings_cleanup_jobs", MeetingsCleanupJob),
     '("adminless_deletion_jobs", AdminlessDeletionJob),
     '("adminless_reminder_jobs", AdminlessReminderJob)
   ]
