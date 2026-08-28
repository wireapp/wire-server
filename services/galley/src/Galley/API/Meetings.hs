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

module Galley.API.Meetings
  ( createMeeting,
    updateMeeting,
    deleteMeeting,
    getMeeting,
    listMeetings,
    createMeetingV16,
    updateMeetingV16,
    getMeetingV16,
    listMeetingsV16,
    addMeetingInvitation,
    removeMeetingInvitation,
    replaceMeetingInvitation,
  )
where

import Data.Domain (Domain)
import Data.Id
import Data.Qualified
import Imports
import Polysemy
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Meeting
import Wire.MeetingsSubsystem qualified as Meetings

createMeeting ::
  (Member Meetings.MeetingsSubsystem r) =>
  Local UserId ->
  ConnId ->
  NewMeeting ->
  Sem r MeetingWithConversation
createMeeting lUser connId newMeeting = Meetings.createMeeting lUser connId newMeeting

updateMeeting ::
  ( Member Meetings.MeetingsSubsystem r,
    Member (ErrorS 'MeetingNotFound) r
  ) =>
  Local UserId ->
  ConnId ->
  Domain ->
  MeetingId ->
  UpdateMeeting ->
  Sem r MeetingWithConversation
updateMeeting zUser connId domain meetingId update = do
  let qMeetingId = Qualified meetingId domain
  noteS @'MeetingNotFound =<< Meetings.updateMeeting zUser connId qMeetingId update

deleteMeeting ::
  ( Member Meetings.MeetingsSubsystem r,
    Member (ErrorS 'MeetingNotFound) r
  ) =>
  Local UserId ->
  ConnId ->
  Domain ->
  MeetingId ->
  Sem r ()
deleteMeeting zUser connId domain meetingId = do
  let qMeetingId = Qualified meetingId domain
  success <- Meetings.deleteMeeting zUser connId qMeetingId
  unless success $ throwS @'MeetingNotFound

getMeeting ::
  ( Member Meetings.MeetingsSubsystem r,
    Member (ErrorS 'MeetingNotFound) r
  ) =>
  Local UserId ->
  Domain ->
  MeetingId ->
  Sem r Meeting
getMeeting zUser domain meetingId = do
  let qMeetingId = Qualified meetingId domain
  noteS @'MeetingNotFound =<< Meetings.getMeeting zUser qMeetingId

listMeetings ::
  (Member Meetings.MeetingsSubsystem r) =>
  Local UserId ->
  Sem r [Meeting]
listMeetings lUser = Meetings.listMeetings lUser

createMeetingV16 ::
  (Member Meetings.MeetingsSubsystem r) =>
  Local UserId ->
  ConnId ->
  NewMeetingV16 ->
  Sem r MeetingWithConversationV16
createMeetingV16 lUser connId newMeeting = Meetings.createMeetingV16 lUser connId newMeeting

updateMeetingV16 ::
  ( Member Meetings.MeetingsSubsystem r,
    Member (ErrorS 'MeetingNotFound) r
  ) =>
  Local UserId ->
  ConnId ->
  Domain ->
  MeetingId ->
  UpdateMeetingV16 ->
  Sem r MeetingWithConversationV16
updateMeetingV16 zUser connId domain meetingId update = do
  let qMeetingId = Qualified meetingId domain
  noteS @'MeetingNotFound =<< Meetings.updateMeetingV16 zUser connId qMeetingId update

getMeetingV16 ::
  ( Member Meetings.MeetingsSubsystem r,
    Member (ErrorS 'MeetingNotFound) r
  ) =>
  Local UserId ->
  Domain ->
  MeetingId ->
  Sem r MeetingV16
getMeetingV16 zUser domain meetingId = do
  let qMeetingId = Qualified meetingId domain
  noteS @'MeetingNotFound =<< Meetings.getMeetingV16 zUser qMeetingId

listMeetingsV16 ::
  (Member Meetings.MeetingsSubsystem r) =>
  Local UserId ->
  Sem r [MeetingV16]
listMeetingsV16 lUser = Meetings.listMeetingsV16 lUser

addMeetingInvitation ::
  ( Member Meetings.MeetingsSubsystem r,
    Member (ErrorS 'MeetingNotFound) r
  ) =>
  Local UserId ->
  Domain ->
  MeetingId ->
  MeetingEmailsInvitation ->
  Sem r ()
addMeetingInvitation zUser domain meetingId (MeetingEmailsInvitation emails) = do
  let qMeetingId = Qualified meetingId domain
  success <- Meetings.addInvitedEmails zUser qMeetingId emails
  unless success $ throwS @'MeetingNotFound

removeMeetingInvitation ::
  ( Member Meetings.MeetingsSubsystem r,
    Member (ErrorS 'MeetingNotFound) r
  ) =>
  Local UserId ->
  Domain ->
  MeetingId ->
  MeetingEmailsInvitation ->
  Sem r ()
removeMeetingInvitation zUser domain meetingId (MeetingEmailsInvitation emails) = do
  let qMeetingId = Qualified meetingId domain
  success <- Meetings.removeInvitedEmails zUser qMeetingId emails
  unless success $ throwS @'MeetingNotFound

replaceMeetingInvitation ::
  ( Member Meetings.MeetingsSubsystem r,
    Member (ErrorS 'MeetingNotFound) r
  ) =>
  Local UserId ->
  Domain ->
  MeetingId ->
  MeetingEmailsInvitation ->
  Sem r ()
replaceMeetingInvitation zUser domain meetingId (MeetingEmailsInvitation emails) = do
  let qMeetingId = Qualified meetingId domain
  success <- Meetings.replaceInvitedEmails zUser qMeetingId emails
  unless success $ throwS @'MeetingNotFound
