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
import qualified Wire.API.Notification as Notification
import qualified Wire.API.Push.V2.Token as Push.Token
import qualified Wire.API.Swagger

-- | Actually all models of the whole API,
-- but it doesn't hurt and makes it less likely to forget one.
gundeckModels :: [Model]
gundeckModels = Wire.API.Swagger.models

-------------------------------------------------------------------------------
-- Push Models

pushTransport :: DataType
pushTransport = Push.Token.typeTransport

pushToken :: Model
pushToken = Push.Token.modelPushToken

pushTokenList :: Model
pushTokenList = Push.Token.modelPushTokenList

-------------------------------------------------------------------------------
-- Notification Models

notificationList :: Model
notificationList = Notification.modelNotificationList

notification :: Model
notification = Notification.modelNotification

event :: Model
event = Notification.modelEvent
