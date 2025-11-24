{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.MeetingsSubsystem where

import Data.Id
import Data.Qualified
import Data.Time.Clock (UTCTime)
import Imports
import Polysemy
import Wire.API.Meeting
import Wire.API.User.Identity (EmailAddress)
import Wire.StoredConversation (StoredConversation)

data MeetingsSubsystem m a where
  CreateMeeting ::
    Local UserId ->
    NewMeeting ->
    -- | trial: True if this is a trial meeting
    Bool ->
    MeetingsSubsystem m (Meeting, StoredConversation)
  GetMeeting ::
    Local UserId ->
    Qualified MeetingId ->
    MeetingsSubsystem m (Maybe Meeting)
  ListMeetings ::
    Local UserId ->
    MeetingsSubsystem m [Meeting]
  UpdateMeeting ::
    Local UserId ->
    Qualified MeetingId ->
    UpdateMeeting ->
    MeetingsSubsystem m (Maybe Meeting)
  DeleteMeeting ::
    Local UserId ->
    Qualified MeetingId ->
    MeetingsSubsystem m Bool
  AddInvitedEmails ::
    Local UserId ->
    Qualified MeetingId ->
    [EmailAddress] ->
    MeetingsSubsystem m Bool
  RemoveInvitedEmails ::
    Local UserId ->
    Qualified MeetingId ->
    [EmailAddress] ->
    MeetingsSubsystem m Bool
  -- Cleanup operation
  CleanupOldMeetings ::
    UTCTime ->
    Int ->
    MeetingsSubsystem m Int64

makeSem ''MeetingsSubsystem
