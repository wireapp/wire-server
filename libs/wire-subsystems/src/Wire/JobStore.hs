{-# LANGUAGE TemplateHaskell #-}

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
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.JobStore
  ( JobStore (..),
    createJob,
    deleteJob,
    deleteJobsByTeamAndKind,
    findJobById,
    findJobsByConversationId,
    findJobsByTeamAndKind,
  )
where

import Imports
import Polysemy
import Data.Id (ConvId, ScheduledJobId, TeamId)
import Wire.API.Jobs

data JobStore m a where
  CreateJob :: ScheduledJob -> JobStore m ()
  FindJobById :: ScheduledJobId -> JobStore m (Maybe ScheduledJob)
  FindJobsByTeamAndKind :: TeamId -> ScheduledJobKind -> JobStore m [ScheduledJob]
  FindJobsByConversationId :: ConvId -> JobStore m [ScheduledJob]
  DeleteJob :: ScheduledJobId -> JobStore m ()
  DeleteJobsByTeamAndKind :: TeamId -> ScheduledJobKind -> JobStore m ()

makeSem ''JobStore
