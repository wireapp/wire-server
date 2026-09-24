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
import Wire.API.User.EmailAddress (EmailAddress)

data MeetingsSubsystem m a where
  CreateMeeting ::
    Local UserId ->
    ConnId ->
    NewMeeting ->
    MeetingsSubsystem m MeetingWithConversation
  UpdateMeeting ::
    Local UserId ->
    ConnId ->
    Qualified MeetingId ->
    UpdateMeeting ->
    MeetingsSubsystem m (Maybe MeetingWithConversation)
  DeleteMeeting ::
    Local UserId ->
    ConnId ->
    Qualified MeetingId ->
    MeetingsSubsystem m Bool
  RefreshMeetingLink ::
    Local UserId ->
    ConnId ->
    Qualified MeetingId ->
    MeetingsSubsystem m (Maybe MeetingWithConversation)
  -- | Fetch a meeting through its join link (WPB-28989). Unlike 'GetMeeting'
  -- this does not require the caller to be the creator or a conversation
  -- member: anyone may resolve a live local meeting link that carries a real
  -- join code. 'Nothing' (surfaced as 404) when the meeting does not exist,
  -- is expired, is remote, or was created without a join code.
  GetMeetingByLink ::
    Local UserId ->
    Qualified MeetingId ->
    MeetingsSubsystem m (Maybe Meeting)
  -- | Join a meeting conversation through its join link (WPB-28989), like
  -- @POST /conversations/join@. Guards mirror 'GetMeetingByLink'; returns
  -- 'Nothing' (surfaced as 404) when the link does not resolve.
  JoinMeeting ::
    Local UserId ->
    ConnId ->
    Qualified MeetingId ->
    MeetingsSubsystem m (Maybe MeetingWithConversation)
  GetMeeting ::
    Local UserId ->
    Qualified MeetingId ->
    MeetingsSubsystem m (Maybe Meeting)
  ListMeetings ::
    Local UserId ->
    MeetingsSubsystem m [Meeting]
  CreateMeetingV16 ::
    Local UserId ->
    ConnId ->
    NewMeetingV16 ->
    MeetingsSubsystem m MeetingWithConversationV16
  UpdateMeetingV16 ::
    Local UserId ->
    ConnId ->
    Qualified MeetingId ->
    UpdateMeetingV16 ->
    MeetingsSubsystem m (Maybe MeetingWithConversationV16)
  GetMeetingV16 ::
    Local UserId ->
    Qualified MeetingId ->
    MeetingsSubsystem m (Maybe MeetingV16)
  ListMeetingsV16 ::
    Local UserId ->
    MeetingsSubsystem m [MeetingV16]
  CreateMeetingV18 ::
    Local UserId ->
    ConnId ->
    NewMeetingV18 ->
    MeetingsSubsystem m MeetingWithConversationV18
  UpdateMeetingV18 ::
    Local UserId ->
    ConnId ->
    Qualified MeetingId ->
    UpdateMeetingV18 ->
    MeetingsSubsystem m (Maybe MeetingWithConversationV18)
  GetMeetingV18 ::
    Local UserId ->
    Qualified MeetingId ->
    MeetingsSubsystem m (Maybe MeetingV18)
  ListMeetingsV18 ::
    Local UserId ->
    MeetingsSubsystem m [MeetingV18]
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
  ReplaceInvitedEmails ::
    Local UserId ->
    Qualified MeetingId ->
    [EmailAddress] ->
    MeetingsSubsystem m Bool
  CleanupOldMeetings ::
    UTCTime ->
    Int ->
    MeetingsSubsystem m Int64

makeSem ''MeetingsSubsystem
