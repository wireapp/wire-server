{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
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

module Wire.JobSubsystem.Interpreter
  ( interpretJobSubsystem,
  )
where

import Arbiter.Core qualified as ArbiterCore
import Data.Id
import Data.Json.Util (UTCTimeMillis)
import Data.Qualified
import Data.Text qualified as Text
import Data.Time
import Hasql.Pool.Extended qualified as HasqlPoolExt
import Imports
import Polysemy
import Polysemy.Input (Input, input)
import Wire.API.Jobs
import Wire.JobSubsystem (JobSubsystem (..), JobSubsystemConfig (..))
import Wire.JobSubsystem.ArbiterAdapter
import Wire.Postgres (PGConstraints)

interpretJobSubsystem ::
  (PGConstraints r, Member (Input RequestId) r) =>
  JobSubsystemConfig ->
  InterpreterFor JobSubsystem r
interpretJobSubsystem conf =
  interpret
    \case
      ScheduleAdminlessSetupJob lusr tid -> scheduleAdminlessSetupJob conf lusr tid
      ScheduleAdminlessDeletionJob lusr tid cid scheduledFor -> scheduleAdminlessDeletionJob conf lusr tid cid scheduledFor
      ScheduleAdminlessReminderJob lusr tid cid deletionScheduledFor reminderTimeout scheduledFor -> scheduleAdminlessReminderJob conf lusr tid cid deletionScheduledFor reminderTimeout scheduledFor

scheduleAdminlessSetupJob ::
  forall r.
  (PGConstraints r, Member (Input RequestId) r) =>
  JobSubsystemConfig ->
  Maybe (Local UserId) ->
  TeamId ->
  Sem r ()
scheduleAdminlessSetupJob JobSubsystemConfig {..} lusr teamId = do
  requestId <- input @RequestId
  pool <- input @HasqlPoolExt.Pool
  let arbiterEnv = mkNewWireArbiterEnv jobSubsystemSchemaName pool
      groupKey = "adminless-setup:" <> idToText teamId
      arbiterJob =
        ( ArbiterCore.defaultGroupedJob
            groupKey
            (AdminlessSetup (AdminlessSetupJob teamId (tUnqualified <$> lusr) requestId))
        )
          { ArbiterCore.dedupKey = Just . ArbiterCore.IgnoreDuplicate $ adminlessSetupJobDedupKey teamId,
            ArbiterCore.maxAttempts = Just 3
          }
  embed $ void $ runWireArbiter arbiterEnv $ ArbiterCore.insertJob @(WireArbiter JobRegistry) @JobRegistry @ConversationsJobPayload arbiterJob

scheduleAdminlessDeletionJob ::
  forall r.
  (PGConstraints r, Member (Input RequestId) r) =>
  JobSubsystemConfig ->
  Maybe (Local UserId) ->
  TeamId ->
  ConvId ->
  UTCTime ->
  Sem r ()
scheduleAdminlessDeletionJob JobSubsystemConfig {..} lusr teamId convId scheduledFor = do
  requestId <- input @RequestId
  pool <- input
  let arbiterEnv = mkNewWireArbiterEnv jobSubsystemSchemaName pool
      groupKey = "adminless-deletion:" <> idToText convId
      arbiterJob =
        ( ArbiterCore.defaultGroupedJob
            groupKey
            (AdminlessDeletion (AdminlessDeletionJob teamId convId (tUnqualified <$> lusr) requestId))
        )
          { ArbiterCore.notVisibleUntil = Just scheduledFor,
            ArbiterCore.dedupKey = Just . ArbiterCore.IgnoreDuplicate $ adminlessJobDedupKey "deletion" convId,
            ArbiterCore.maxAttempts = Just 3
          }
  embed $ void $ runWireArbiter arbiterEnv $ ArbiterCore.insertJob @(WireArbiter JobRegistry) @JobRegistry @ConversationsJobPayload arbiterJob

scheduleAdminlessReminderJob ::
  forall r.
  (PGConstraints r, Member (Input RequestId) r) =>
  JobSubsystemConfig ->
  Maybe (Local UserId) ->
  TeamId ->
  ConvId ->
  UTCTimeMillis ->
  NominalDiffTime ->
  UTCTime ->
  Sem r ()
scheduleAdminlessReminderJob JobSubsystemConfig {..} lusr teamId convId deletionScheduledFor reminderTimeout scheduledFor = do
  requestId <- input @RequestId
  pool <- input @HasqlPoolExt.Pool
  let arbiterEnv = mkNewWireArbiterEnv jobSubsystemSchemaName pool
      groupKey = "adminless-reminder:" <> idToText convId
      arbiterJob =
        ( ArbiterCore.defaultGroupedJob
            groupKey
            (AdminlessReminder (AdminlessReminderJob teamId convId (tUnqualified <$> lusr) deletionScheduledFor requestId))
        )
          { ArbiterCore.notVisibleUntil = Just scheduledFor,
            ArbiterCore.dedupKey = Just . ArbiterCore.IgnoreDuplicate $ adminlessReminderJobDedupKey convId reminderTimeout,
            ArbiterCore.maxAttempts = Just 3
          }
  embed $ void $ runWireArbiter arbiterEnv $ ArbiterCore.insertJob @(WireArbiter JobRegistry) @JobRegistry @ConversationsJobPayload arbiterJob

adminlessJobDedupKey :: Text -> ConvId -> Text
adminlessJobDedupKey jobType convId =
  "adminless-" <> jobType <> ":" <> idToText convId

adminlessSetupJobDedupKey :: TeamId -> Text
adminlessSetupJobDedupKey teamId =
  "adminless-setup:" <> idToText teamId

adminlessReminderJobDedupKey :: ConvId -> NominalDiffTime -> Text
adminlessReminderJobDedupKey convId reminderTimeout =
  adminlessJobDedupKey "reminder" convId <> ":" <> Text.pack (show reminderTimeout)
