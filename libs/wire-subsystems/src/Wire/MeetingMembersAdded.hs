{-# LANGUAGE TemplateHaskell #-}

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

module Wire.MeetingMembersAdded
  ( MeetingMembersAdded,
    notifyMeetingMembersAdded,
    newLocalMeetingMembers,
    discardMeetingMembersAdded,
  )
where

import Data.Id
import Data.Qualified (Qualified)
import Data.Set (Set)
import Data.Set qualified as Set
import Imports
import Polysemy

-- | Post-commit hook for notifying users who became members of an MLS meeting
-- conversation. Keeping this effect narrow avoids a dependency from the
-- conversation subsystem onto the meetings subsystem.
data MeetingMembersAdded m a where
  NotifyMeetingMembersAdded ::
    Qualified UserId ->
    Qualified ConvId ->
    Maybe TeamId ->
    [UserId] ->
    MeetingMembersAdded m ()

makeSem ''MeetingMembersAdded

-- | Find users who were absent before the commit, are present afterwards, and
-- belong to the local backend. Adding another client for an existing user does
-- not change either membership set and therefore produces no result.
newLocalMeetingMembers ::
  Set UserId ->
  Set UserId ->
  [UserId]
newLocalMeetingMembers before after =
  Set.toList (Set.difference after before)

-- | Interpreter for runtimes which expose no MLS write endpoints.
discardMeetingMembersAdded :: InterpreterFor MeetingMembersAdded r
discardMeetingMembersAdded = interpret $ \case
  NotifyMeetingMembersAdded {} -> pure ()
