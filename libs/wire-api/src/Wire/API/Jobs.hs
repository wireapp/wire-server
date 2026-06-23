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

module Wire.API.Jobs
  ( ScheduledJobsRegistry,
    MeetingsCleanupJob (..),
    meetingsCleanupQueueName,
  )
where

import Data.Aeson (FromJSON, ToJSON, Value (Null), parseJSON, toJSON)
import Imports

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

-- | Registry for the scheduled jobs we expose via Arbiter.
type ScheduledJobsRegistry =
  '[ '("meetings_cleanup_jobs", MeetingsCleanupJob)
   ]
