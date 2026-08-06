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

module Wire.MeetingNotifier
  ( MeetingNotifier (NotifyMeetingMembersAdded, NotifyMeetingEvent),
    notifyMeetingMembersAdded,
    notifyMeetingEvent,
  )
where

import Data.Id
import Data.Qualified (Local, Qualified)
import Imports
import Polysemy
import Wire.API.Event.Meeting qualified as MeetingEvent
import Wire.StoredConversation (LocalMember)

-- | Interface for all meeting notifications. Routing both the post-commit
-- member-add hook and the create/update/delete lifecycle events through one
-- effect avoids a dependency from the conversation subsystem onto the meetings
-- subsystem.
data MeetingNotifier m a where
  -- | Post-commit member-add hook: notify the users added by a successful
  -- membership commit to a meeting conversation. Delivered fire-and-forget.
  NotifyMeetingMembersAdded ::
    Qualified UserId ->
    Qualified ConvId ->
    Maybe TeamId ->
    [UserId] ->
    MeetingNotifier m ()
  -- | Create/update/delete lifecycle event. Delivered synchronously.
  NotifyMeetingEvent ::
    Local UserId ->
    Maybe ConnId ->
    [LocalMember] ->
    Qualified ConvId ->
    Maybe TeamId ->
    MeetingEvent.EventType ->
    Qualified MeetingId ->
    MeetingNotifier m ()

makeSem ''MeetingNotifier
