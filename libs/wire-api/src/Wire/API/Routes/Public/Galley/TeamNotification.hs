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

module Wire.API.Routes.Public.Galley.TeamNotification where

import Data.Range
import Imports
import Servant
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Notification
import Wire.API.Routes.Named
import Wire.API.Routes.Public

type TeamNotificationAPI =
  Named
    "get-team-notifications"
    ( Summary "Read recently added team members from team queue"
        :> Description GetTeamNotificationsDescription
        :> "teams"
        :> "notifications"
        :> ZUser
        :> CanThrow 'TeamNotFound
        :> CanThrow 'InvalidTeamNotificationId
        :> QueryParam'
             [ Optional,
               Strict,
               Description "Notification id to start with in the response (UUIDv1)"
             ]
             "since"
             NotificationId
        :> QueryParam'
             [ Optional,
               Strict,
               Description "Maximum number of events to return (1..10000; default: 1000)"
             ]
             "size"
             (Range 1 10000 Int32)
        :> Get '[Servant.JSON] QueuedNotificationList
    )

type GetTeamNotificationsDescription =
  "This is a work-around for scalability issues with gundeck user event fan-out. \
  \It does not track all team-wide events, but only `member-join`.\
  \\n\
  \Note that `/teams/notifications` behaves differently from `/notifications`:\
  \\n\
  \- If there is a gap between the notification id requested with `since` and the \
  \available data, team queues respond with 200 and the data that could be found. \
  \They do NOT respond with status 404, but valid data in the body.\
  \\n\
  \- The notification with the id given via `since` is included in the \
  \response if it exists.  You should remove this and only use it to decide whether \
  \there was a gap between your last request and this one.\
  \\n\
  \- If the notification id does *not* exist, you get the more recent events from the queue \
  \(instead of all of them).  This can be done because a notification id is a UUIDv1, which \
  \is essentially a time stamp.\
  \\n\
  \- There is no corresponding `/last` end-point to get only the most recent event. \
  \That end-point was only useful to avoid having to pull the entire queue.  In team \
  \queues, if you have never requested the queue before and \
  \have no prior notification id, just pull with timestamp 'now'."
