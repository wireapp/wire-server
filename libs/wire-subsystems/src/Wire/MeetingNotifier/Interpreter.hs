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

module Wire.MeetingNotifier.Interpreter
  ( interpretMeetingNotifier,
  )
where

import Data.ByteString.Conversion (toByteString')
import Data.Id
import Data.Qualified (Local, Qualified (..), tDomain, tUnqualified)
import Imports
import Polysemy
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as TinyLog
import System.Logger qualified as Log
import Wire.API.Event.Meeting qualified as MeetingEvent
import Wire.MeetingNotifier
import Wire.MeetingsStore qualified as Store
import Wire.MeetingsSubsystem.Notification
import Wire.NotificationSubsystem
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now
import Wire.StoredConversation (LocalMember)

-- | Interpret 'MeetingNotifier'.
interpretMeetingNotifier ::
  ( Member Store.MeetingsStore r,
    Member NotificationSubsystem r,
    Member Now r,
    Member TinyLog r
  ) =>
  InterpreterFor MeetingNotifier r
interpretMeetingNotifier = interpret $ \case
  NotifyMeetingMembersAdded qUser qConvId mTeamId users ->
    notifyMeetingMembersAddedImpl qUser qConvId mTeamId users
  NotifyMeetingEvent lUser conn members qConvId mTeamId meetingType qMeetingId ->
    notifyMeetingEventImpl lUser conn members qConvId mTeamId meetingType qMeetingId

-- | Deliver @meeting.member-add@ notifications fire-and-forget via 'pushNotificationAsync'. Resolve each alive meeting for the conversation and notify only the users added by the successful membership commit, logging a warning when no alive meeting is found.
notifyMeetingMembersAddedImpl ::
  ( Member Store.MeetingsStore r,
    Member NotificationSubsystem r,
    Member Now r,
    Member TinyLog r
  ) =>
  Qualified UserId ->
  Qualified ConvId ->
  Maybe TeamId ->
  [UserId] ->
  Sem r ()
notifyMeetingMembersAddedImpl qUser qConvId mTeamId users = do
  now <- Now.get
  meetings <- Store.listMeetingsByConversation (qUnqualified qConvId) now
  when (null meetings) $
    TinyLog.warn $
      Log.msg ("alive meeting not found for meeting member-add event" :: ByteString)
        . Log.field "conversationId" (toByteString' (qUnqualified qConvId))
  -- `users` are the members added by the commit; the commit creator is already
  -- a member and never in `users`, so mkMeetingEventPush (which no longer
  -- filters the originator by UserId) does not echo member-add back to them.
  -- conn is Nothing: every client connection of each added user should be notified.
  for_ meetings $ \meeting ->
    pushNotificationAsync $
      mkMeetingEventPush
        now
        qUser
        Nothing
        (map userRecipient users)
        qConvId
        mTeamId
        MeetingEvent.MemberAdd
        (Qualified meeting.id (qDomain qConvId))

-- | Deliver a create/update/delete lifecycle event synchronously via 'pushNotifications'.
notifyMeetingEventImpl ::
  ( Member NotificationSubsystem r,
    Member Now r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  [LocalMember] ->
  Qualified ConvId ->
  Maybe TeamId ->
  MeetingEvent.EventType ->
  Qualified MeetingId ->
  Sem r ()
notifyMeetingEventImpl lUser conn members qConvId mTeamId meetingType qMeetingId = do
  now <- Now.get
  pushNotifications
    [ mkMeetingEventPush
        now
        (Qualified (tUnqualified lUser) (tDomain lUser))
        conn
        (map localMemberToRecipient members)
        qConvId
        mTeamId
        meetingType
        qMeetingId
    ]
