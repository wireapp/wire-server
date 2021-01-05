{-# LANGUAGE StrictData #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

import Brig.Types.Intra (accountUser)
import Brig.Types.User (userTeam)
import Control.Monad.Catch
import Control.Retry (exponentialBackoff, limitRetries, retrying)
import Data.Id
import Data.Json.Util (toJSONObject)
import qualified Data.List1 as List1
import Data.Range (Range)
import qualified Data.UUID.V1 as UUID
import Galley.API.Error
import Galley.App
import qualified Galley.Data.TeamNotifications as DataTeamQueue
import Galley.Intra.User as Intra
import Galley.Types.Teams hiding (newTeam)
import Gundeck.Types.Notification
import Imports
import Network.HTTP.Types
import Network.Wai.Utilities

getTeamNotifications ::
  UserId ->
  Maybe NotificationId ->
  Range 1 10000 Int32 ->
  Galley QueuedNotificationList
getTeamNotifications zusr since size = do
  tid :: TeamId <- do
    mtid <- (userTeam . accountUser =<<) <$> Intra.getUser zusr
    let err = throwM teamNotFound
    maybe err pure mtid
  page <- DataTeamQueue.fetch tid since size
  pure $
    queuedNotificationList
      (toList (DataTeamQueue.resultSeq page))
      (DataTeamQueue.resultHasMore page)
      Nothing

pushTeamEvent :: TeamId -> Event -> Galley ()
pushTeamEvent tid evt = do
  nid <- mkNotificationId
  DataTeamQueue.add tid nid (List1.singleton $ toJSONObject evt)

-- | 'Data.UUID.V1.nextUUID' is sometimes unsuccessful, so we try a few times.
mkNotificationId :: (MonadIO m, MonadThrow m) => m NotificationId
mkNotificationId = do
  ni <- fmap Id <$> retrying x10 fun (const (liftIO UUID.nextUUID))
  maybe (throwM err) return ni
  where
    x10 = limitRetries 10 <> exponentialBackoff 10
    fun = const (return . isNothing)
    err = Error status500 "internal-error" "unable to generate notification ID"
