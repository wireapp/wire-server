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

module Wire.MeetingsStore where

import Data.Id
import Data.Qualified
import Data.Time.Clock
import Imports
import Polysemy
import Wire.API.Meeting
import Wire.API.User.Identity (EmailAddress)

data MeetingsStore m a where
  CreateMeeting ::
    Qualified MeetingId ->
    Qualified UserId ->
    Text ->
    UTCTime ->
    UTCTime ->
    Maybe Recurrence ->
    Qualified ConvId ->
    [EmailAddress] ->
    Bool ->
    MeetingsStore m ()
  GetMeeting ::
    Qualified MeetingId ->
    MeetingsStore m (Maybe Meeting)
  ListMeetingsByUser ::
    UserId ->
    MeetingsStore m [Meeting]
  ListMeetingsByConversation ::
    Qualified ConvId ->
    MeetingsStore m [Meeting]
  UpdateMeeting ::
    Qualified MeetingId ->
    Maybe Text ->
    Maybe UTCTime ->
    Maybe UTCTime ->
    Maybe Recurrence ->
    MeetingsStore m (Maybe Meeting)
  DeleteMeeting ::
    Qualified MeetingId ->
    MeetingsStore m ()
  AddInvitedEmails ::
    Qualified MeetingId ->
    [EmailAddress] ->
    MeetingsStore m ()
  RemoveInvitedEmails ::
    Qualified MeetingId ->
    [EmailAddress] ->
    MeetingsStore m ()
  -- Cleanup operations
  GetOldMeetings ::
    UTCTime ->
    Int ->
    MeetingsStore m [Meeting]
  DeleteMeetingBatch ::
    [Qualified MeetingId] ->
    MeetingsStore m Int64

makeSem ''MeetingsStore
