{-# LANGUAGE TemplateHaskell #-}

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

module Galley.Effects.TeamNotificationStore
  ( TeamNotificationStore (..),
    TeamNotificationId,
    createTeamNotification,
    getTeamNotifications,
    mkNotificationId,
  )
where

import Data.Aeson qualified as JSON
import Data.Id
import Data.List1 (List1)
import Data.Range
import Galley.Data.TeamNotifications
import Imports
import Polysemy

data TeamNotification

type TeamNotificationId = Id TeamNotification

data TeamNotificationStore m a where
  CreateTeamNotification ::
    TeamId ->
    TeamNotificationId ->
    List1 JSON.Object ->
    TeamNotificationStore m ()
  GetTeamNotifications ::
    TeamId ->
    Maybe TeamNotificationId ->
    Range 1 10000 Int32 ->
    TeamNotificationStore m ResultPage
  MkNotificationId :: TeamNotificationStore m TeamNotificationId

makeSem ''TeamNotificationStore
