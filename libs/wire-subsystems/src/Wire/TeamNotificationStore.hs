{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

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

-- | See also: "Galley.API.TeamNotifications".
--
-- This module is a clone of "Gundeck.Notification.Data".
--
-- FUTUREWORK: this is a work-around because it only solves *some* problems with team events.
-- We should really use a scalable message queue instead.
module Wire.TeamNotificationStore
  ( ResultPage (..),
    TeamNotificationStore (..),
    createTeamNotification,
    getTeamNotifications,
    mkNotificationId,
  )
where

import Data.Aeson qualified as JSON
import Data.Id
import Data.List.NonEmpty
import Data.Range
import Data.Sequence (Seq)
import Imports
import Polysemy
import Wire.API.Internal.Notification

data ResultPage = ResultPage
  { resultSeq :: Seq QueuedNotification,
    resultHasMore :: !Bool
  }

data TeamNotificationStore m a where
  CreateTeamNotification ::
    TeamId ->
    NotificationId ->
    NonEmpty JSON.Object ->
    TeamNotificationStore m ()
  GetTeamNotifications ::
    TeamId ->
    Maybe NotificationId ->
    Range 1 10000 Int32 ->
    TeamNotificationStore m ResultPage
  MkNotificationId :: TeamNotificationStore m NotificationId

makeSem ''TeamNotificationStore
