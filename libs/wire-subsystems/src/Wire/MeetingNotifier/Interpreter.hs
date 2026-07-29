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
import Data.Qualified (Qualified (..), tDomain, tUnqualified)
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

-- | Interpret 'MeetingNotifier'. Member-add notifications are delivered
-- fire-and-forget via 'pushNotificationAsync': we resolve each alive meeting
-- for the conversation and notify only the users added by the successful
-- membership commit, logging when no alive meeting is found. The
-- create/update/delete lifecycle events are delivered synchronously via
-- 'pushNotifications'.
interpretMeetingNotifier ::
  ( Member Store.MeetingsStore r,
    Member NotificationSubsystem r,
    Member Now r,
    Member TinyLog r
  ) =>
  InterpreterFor MeetingNotifier r
interpretMeetingNotifier = interpret $ \case
  NotifyMeetingMembersAdded qUser qConvId mTeamId users -> do
    now <- Now.get
    meetings <- Store.listMeetingsByConversation (qUnqualified qConvId) now
    when (null meetings) $
      TinyLog.warn $
        Log.msg ("alive meeting not found for meeting member-add event" :: ByteString)
          . Log.field "conversationId" (toByteString' (qUnqualified qConvId))
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
  NotifyMeetingEvent lUser conn members qConvId mTeamId meetingType qMeetingId -> do
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
