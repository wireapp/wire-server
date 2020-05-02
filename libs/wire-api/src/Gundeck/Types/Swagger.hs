{-# LANGUAGE OverloadedStrings #-}

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

module Gundeck.Types.Swagger where

import Data.Swagger.Build.Api
import Imports

gundeckModels :: [Model]
gundeckModels =
  [ notificationList,
    notification,
    event
  ]

-------------------------------------------------------------------------------
-- Notification Models

notificationList :: Model
notificationList = defineModel "NotificationList" $ do
  description "Zero or more notifications"
  property "notifications" (array (ref notification)) $
    description "Notifications"
  property "has_more" bool' $
    description "Whether there are still more notifications."

notification :: Model
notification = defineModel "Notification" $ do
  description "A single notification"
  property "id" bytes' $
    description "Notification ID"
  property "payload" (array (ref event)) $
    description "List of events"

event :: Model
event = defineModel "NotificationEvent" $ do
  description "A single event"
  property "type" string' $
    description "Event type"
