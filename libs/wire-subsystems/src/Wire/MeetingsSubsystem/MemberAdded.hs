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

module Wire.MeetingsSubsystem.MemberAdded
  ( interpretMeetingMembersAdded,
  )
where

import Data.ByteString.Conversion (toByteString')
import Data.Qualified (Qualified (..))
import Imports
import Polysemy
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as TinyLog
import System.Logger qualified as Log
import Wire.API.Event.Meeting qualified as MeetingEvent
import Wire.MeetingMembersAdded
import Wire.MeetingsStore qualified as Store
import Wire.MeetingsSubsystem.Notification
import Wire.NotificationSubsystem
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now

-- | Resolve each alive meeting for the conversation and notify only the users
-- added by the successful membership commit. Best-effort delivery waits for
-- each push before continuing, preserving its ordering before MLS Welcome,
-- while the notification interpreter logs and suppresses delivery failures.
interpretMeetingMembersAdded ::
  ( Member Store.MeetingsStore r,
    Member NotificationSubsystem r,
    Member Now r,
    Member TinyLog r
  ) =>
  InterpreterFor MeetingMembersAdded r
interpretMeetingMembersAdded = interpret $ \case
  NotifyMeetingMembersAdded qUser qConvId mTeamId users -> do
    now <- Now.get
    meetings <- Store.listMeetingsByConversation (qUnqualified qConvId) now
    when (null meetings) $
      TinyLog.warn $
        Log.msg ("alive meeting not found for meeting member-add event" :: ByteString)
          . Log.field "conversationId" (toByteString' (qUnqualified qConvId))
    for_ meetings $ \meeting ->
      pushNotificationBestEffort $
        mkMeetingEventPush
          now
          qUser
          Nothing
          (map userRecipient users)
          qConvId
          mTeamId
          MeetingEvent.MemberAdd
          (Qualified meeting.id (qDomain qConvId))
