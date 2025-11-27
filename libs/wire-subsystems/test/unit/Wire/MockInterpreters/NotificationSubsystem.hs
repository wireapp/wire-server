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

module Wire.MockInterpreters.NotificationSubsystem where

import Imports
import Polysemy
import Polysemy.State
import Wire.NotificationSubsystem

inMemoryNotificationSubsystemInterpreter ::
  (Member (State [Push]) r) =>
  InterpreterFor NotificationSubsystem r
inMemoryNotificationSubsystemInterpreter = interpret \case
  PushNotifications ps -> modify (ps <>) $> ()
  PushNotificationsSlowly {} -> error "PushNotificationsSlowly: Implement on demand"
  PushNotificationAsync {} -> error "PushNotificationAsync: Implement on demand"
  CleanupUser {} -> error "CleanupUser: Implement on demand"
  UnregisterPushClient {} -> error "UnregisterPushClient: Implement on demand"
  GetPushTokens {} -> error "GetPushTokens: Implement on demand"
  SetupConsumableNotifications {} -> error "SetupConsumableNotifications: Implement on demand"
