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

module Wire.MeetingsSubsystem.Notification
  ( mkMeetingEventPush,
  )
where

import Data.Default (def)
import Data.Id
import Data.Json.Util (toJSONObject)
import Data.Qualified (Qualified (..))
import Data.Time.Clock (UTCTime)
import Imports
import Wire.API.Event.Meeting qualified as MeetingEvent
import Wire.API.Push.V2 qualified as PushV2
import Wire.NotificationSubsystem

-- | Build the common push envelope used by all meeting lifecycle events.
mkMeetingEventPush ::
  UTCTime ->
  Qualified UserId ->
  Maybe ConnId ->
  [Recipient] ->
  Qualified ConvId ->
  Maybe TeamId ->
  MeetingEvent.EventType ->
  Qualified MeetingId ->
  Push
mkMeetingEventPush now qUser conn recipients qConvId mTeamId meetingType qMeetingId =
  def
    { origin = Just (qUnqualified qUser),
      json =
        toJSONObject
          MeetingEvent.Event
            { evtType = meetingType,
              evtMeeting = qMeetingId,
              evtConv = qConvId,
              evtFrom = MeetingEvent.EventFromUser qUser,
              evtTime = now,
              evtTeam = mTeamId
            },
      recipients,
      route = PushV2.RouteDirect,
      conn
    }
