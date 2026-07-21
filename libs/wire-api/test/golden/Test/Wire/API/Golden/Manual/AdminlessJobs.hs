-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Golden.Manual.AdminlessJobs where

import Data.Id
import Data.Json.Util (UTCTimeMillis, readUTCTimeMillis)
import Data.UUID qualified as UUID
import Imports
import Wire.API.Jobs

teamId :: TeamId
teamId = Id . fromJust $ UUID.fromString "00000000-0000-0000-0000-000000000001"

conversationId :: ConvId
conversationId = Id . fromJust $ UUID.fromString "00000000-0000-0000-0000-000000000002"

originUserId :: UserId
originUserId = Id . fromJust $ UUID.fromString "00000000-0000-0000-0000-000000000003"

requestId :: RequestId
requestId = RequestId "golden-adminless-job"

deletionScheduledFor :: UTCTimeMillis
deletionScheduledFor = fromJust $ readUTCTimeMillis "2026-07-14T12:00:00.000Z"

testObject_AdminlessDeletionJob_1 :: AdminlessDeletionJob
testObject_AdminlessDeletionJob_1 = AdminlessDeletionJob teamId conversationId Nothing requestId

testObject_AdminlessDeletionJob_2 :: AdminlessDeletionJob
testObject_AdminlessDeletionJob_2 = AdminlessDeletionJob teamId conversationId (Just originUserId) requestId

testObject_AdminlessReminderJob_1 :: AdminlessReminderJob
testObject_AdminlessReminderJob_1 = AdminlessReminderJob teamId conversationId Nothing deletionScheduledFor requestId

testObject_AdminlessReminderJob_2 :: AdminlessReminderJob
testObject_AdminlessReminderJob_2 = AdminlessReminderJob teamId conversationId (Just originUserId) deletionScheduledFor requestId

testObject_AdminlessSetupJob_1 :: AdminlessSetupJob
testObject_AdminlessSetupJob_1 = AdminlessSetupJob teamId Nothing requestId

testObject_AdminlessSetupJob_2 :: AdminlessSetupJob
testObject_AdminlessSetupJob_2 = AdminlessSetupJob teamId (Just originUserId) requestId

testObject_MeetingsJobPayload_MeetingsCleanup_1 :: MeetingsJobPayload
testObject_MeetingsJobPayload_MeetingsCleanup_1 = MeetingsCleanup MeetingsCleanupJob

testObject_ConversationsJobPayload_AdminlessDeletion_1 :: ConversationsJobPayload
testObject_ConversationsJobPayload_AdminlessDeletion_1 = AdminlessDeletion testObject_AdminlessDeletionJob_1

testObject_ConversationsJobPayload_AdminlessReminder_1 :: ConversationsJobPayload
testObject_ConversationsJobPayload_AdminlessReminder_1 = AdminlessReminder testObject_AdminlessReminderJob_1

testObject_ConversationsJobPayload_AdminlessSetup_1 :: ConversationsJobPayload
testObject_ConversationsJobPayload_AdminlessSetup_1 = AdminlessSetup testObject_AdminlessSetupJob_1
