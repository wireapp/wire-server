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

module Wire.API.Jobs
  ( ScheduledJobsRegistry,
    ScheduledJob (..),
    ScheduledJobKind (..),
    MeetingsCleanupJob (..),
    meetingsCleanupQueueName,
    scheduledJobKindFromInt,
    scheduledJobKindToInt,
  )
where

import Data.Aeson (FromJSON, ToJSON, Value (Null), parseJSON, toJSON)
import Data.Int qualified as Int
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Imports
import Wire.API.PostgresMarshall

-- | Shared queue name for the scheduled meetings cleanup job.
meetingsCleanupQueueName :: Text
meetingsCleanupQueueName = "meetings_cleanup_jobs"

-- | Empty payload because the schedule itself carries all execution context.
data MeetingsCleanupJob = MeetingsCleanupJob
  deriving stock (Eq, Generic, Show)

instance ToJSON MeetingsCleanupJob where
  toJSON MeetingsCleanupJob = Null

instance FromJSON MeetingsCleanupJob where
  parseJSON Null = pure MeetingsCleanupJob
  parseJSON _ = fail "MeetingsCleanupJob expects null"

-- | The generic scheduled-job families we currently need to persist.
data ScheduledJobKind
  = AdminlessReminder
  | AdminlessDeletion
  | AdminlessSetup
  | AdminlessTeardown
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)

scheduledJobKindToInt :: ScheduledJobKind -> Int
scheduledJobKindToInt = fromEnum

scheduledJobKindFromInt :: Int -> Maybe ScheduledJobKind
scheduledJobKindFromInt n
  | n < fromEnum (minBound :: ScheduledJobKind) = Nothing
  | n > fromEnum (maxBound :: ScheduledJobKind) = Nothing
  | otherwise = Just (toEnum n)

-- | App-level metadata stored alongside Arbiter's runtime state.
data ScheduledJob = ScheduledJob
  { scheduledJobId :: UUID,
    scheduledJobKind :: ScheduledJobKind,
    scheduledJobTeamId :: UUID,
    scheduledJobConversationId :: Maybe UUID,
    scheduledJobScheduledFor :: UTCTime
  }
  deriving stock (Eq, Generic, Show)

instance PostgresMarshall Int.Int32 ScheduledJobKind where
  postgresMarshall = fromIntegral . scheduledJobKindToInt

instance PostgresUnmarshall Int.Int32 ScheduledJobKind where
  postgresUnmarshall n =
    maybe (Left "invalid scheduled job kind") Right $
      scheduledJobKindFromInt (fromIntegral n)

instance PostgresMarshall (UUID, Int.Int32, UUID, Maybe UUID, UTCTime) ScheduledJob where
  postgresMarshall ScheduledJob{..} =
    ( scheduledJobId,
      postgresMarshall scheduledJobKind,
      scheduledJobTeamId,
      scheduledJobConversationId,
      scheduledJobScheduledFor
    )

instance PostgresUnmarshall (UUID, Int.Int32, UUID, Maybe UUID, UTCTime) ScheduledJob where
  postgresUnmarshall (jobId, jobKind, teamId, conversationId, scheduledFor) =
    ScheduledJob
      <$> postgresUnmarshall jobId
      <*> postgresUnmarshall jobKind
      <*> postgresUnmarshall teamId
      <*> postgresUnmarshall conversationId
      <*> postgresUnmarshall scheduledFor

-- | Registry for the scheduled jobs we expose via Arbiter.
type ScheduledJobsRegistry =
  '[ '("meetings_cleanup_jobs", MeetingsCleanupJob)
   ]
