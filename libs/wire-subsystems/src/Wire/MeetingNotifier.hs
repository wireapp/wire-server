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
    newLocalMeetingMembers,
    discardMeetingNotifier,
  )
where

import Data.Id
import Data.Qualified (Local, Qualified)
import Data.Set qualified as Set
import Imports
import Polysemy
import Wire.API.Event.Meeting qualified as MeetingEvent
import Wire.StoredConversation (LocalMember)

-- | Seam for all meeting notifications. 'NotifyMeetingMembersAdded' covers the
-- post-commit member-add hook (delivered fire-and-forget), while
-- 'NotifyMeetingEvent' covers the create/update/delete lifecycle events
-- (delivered synchronously). Routing both through one effect avoids a
-- dependency from the conversation subsystem onto the meetings subsystem.
data MeetingNotifier m a where
  NotifyMeetingMembersAdded ::
    Qualified UserId ->
    Qualified ConvId ->
    Maybe TeamId ->
    [UserId] ->
    MeetingNotifier m ()
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

-- | Find users who were absent before the commit, are present afterwards, and
-- belong to the local backend. Adding another client for an existing user does
-- not change either membership set and therefore produces no result.
newLocalMeetingMembers ::
  Set UserId ->
  Set UserId ->
  [UserId]
newLocalMeetingMembers before after =
  Set.toList (Set.difference after before)

-- | Interpreter for runtimes which expose no meeting write endpoints.
discardMeetingNotifier :: InterpreterFor MeetingNotifier r
discardMeetingNotifier = interpret $ \case
  NotifyMeetingMembersAdded {} -> pure ()
  NotifyMeetingEvent {} -> pure ()
