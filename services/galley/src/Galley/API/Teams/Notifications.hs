{-# LANGUAGE StrictData #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

-- | This module is a work-around for the fact that we do not send notifications to all users
-- of large teams any more.  For greeting bots to hear from new users, the 'MemberJoin' event
-- is stored in a queue that every member of a team can pull, in a similar (but not identical)
-- way as gundeck's @/notifications@ end-point, galley has a @/teams/notifications@ end-point
-- where these events can be pulled.
--
-- This module is a clone of "Gundeck.Notification".
--
-- This could have been added to gundeck, but we didn't.  Some motivation: (1) It is a *team*
-- event queue; teams live in galley, and only galley triggers the team events to be stored in
-- the teams.  (2) The team event queue differs from the other queues in that it is not a
-- fallback for websockets / push notifications, but the only source of the events; so a big
-- part of gundeck isn't really needed.  (3) Fewer RPCs, less code.
--
-- FUTUREWORK: this is a work-around because it only solves *some* problems with team events.
-- We should really use a scalable message queue instead.
module Galley.API.Teams.Notifications
  ( getTeamNotifications,
    pushTeamEvent,
  )
where

import Data.Id
import Data.Json.Util (toJSONObject)
import Data.List1 qualified as List1
import Data.Range (Range)
import Galley.Data.TeamNotifications qualified as DataTeamQueue
import Galley.Effects
import Galley.Effects.BrigAccess as Intra
import Galley.Effects.TeamNotificationStore qualified as E
import Imports
import Polysemy
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Team (Event)
import Wire.API.Internal.Notification
import Wire.API.User

getTeamNotifications ::
  ( Member BrigAccess r,
    Member (ErrorS 'TeamNotFound) r,
    Member TeamNotificationStore r
  ) =>
  UserId ->
  Maybe NotificationId ->
  Range 1 10000 Int32 ->
  Sem r QueuedNotificationList
getTeamNotifications zusr mSinceText size = do
  let since = case mSinceText of
        Nothing -> Nothing
        -- TODO: error is bad
        Just sinceText -> either error Just $ parseIdFromText sinceText
  tid <- (noteS @'TeamNotFound =<<) $ (userTeam . accountUser =<<) <$> Intra.getUser zusr
  page <- E.getTeamNotifications tid since size
  pure $
    queuedNotificationList
      (toList (DataTeamQueue.resultSeq page))
      (DataTeamQueue.resultHasMore page)
      Nothing

pushTeamEvent :: (Member TeamNotificationStore r) => TeamId -> Event -> Sem r ()
pushTeamEvent tid evt = do
  nid <- E.mkNotificationId
  E.createTeamNotification tid nid (List1.singleton $ toJSONObject evt)
