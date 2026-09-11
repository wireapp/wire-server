{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

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
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
-- for more details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.BackgroundJobsPublisher.Arbiter
  ( interpretBackgroundJobPublisherArbiter,
  )
where

import Arbiter.Core qualified as ArbiterCore
import Data.Id
import Hasql.Pool.Extended qualified as HasqlPoolExt
import Imports
import Polysemy
import Polysemy.Input (input)
import Wire.API.BackgroundJobs
import Wire.API.Jobs
import Wire.BackgroundJobsPublisher (BackgroundJobPublisher (..))
import Wire.JobSubsystem (JobSubsystemConfig (..))
import Wire.JobSubsystem.ArbiterAdapter
import Wire.Postgres (PGConstraints)

interpretBackgroundJobPublisherArbiter ::
  (PGConstraints r) =>
  RequestId ->
  JobSubsystemConfig ->
  InterpreterFor BackgroundJobPublisher r
interpretBackgroundJobPublisherArbiter requestId conf =
  interpret
    \case
      PublishJob payload -> publishJob requestId conf payload

publishJob ::
  (PGConstraints r) =>
  RequestId ->
  JobSubsystemConfig ->
  BackgroundJobPayload ->
  Sem r ()
publishJob requestId JobSubsystemConfig {..} = \case
  BackgroundJobSyncUserGroup syncUserGroup ->
    insertUserGroupsJob
      jobSubsystemSchemaName
      (UserGroupsSyncUserGroup (UserGroupsSyncUserGroupJob requestId syncUserGroup))
      syncUserGroup.userGroupId
  BackgroundJobSyncUserGroupAndChannel syncUserGroupAndChannel ->
    insertUserGroupsJob
      jobSubsystemSchemaName
      (UserGroupsSyncUserGroupAndChannel (UserGroupsSyncUserGroupAndChannelJob requestId syncUserGroupAndChannel))
      syncUserGroupAndChannel.userGroupId

insertUserGroupsJob ::
  (PGConstraints r) =>
  Text ->
  UserGroupsJobPayload ->
  UserGroupId ->
  Sem r ()
insertUserGroupsJob schemaName payload userGroupId = do
  pool <- input @HasqlPoolExt.Pool
  let arbiterEnv = mkNewWireArbiterEnv schemaName pool
      groupKey = "user-group-sync:" <> idToText userGroupId
      arbiterJob =
        (ArbiterCore.defaultGroupedJob groupKey payload)
          { ArbiterCore.maxAttempts = Just 3
          }
  embed $ void $ runWireArbiter arbiterEnv $ ArbiterCore.insertJob @UserGroupsJobPayload @(WireArbiter JobRegistry) arbiterJob
