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

module Wire.BackendNotificationSubsystem.InMemory
  ( runBackendNotificationSubsystemInMemory,
    runBackendNotificationSubsystemNoOp,
  )
where

import Imports
import Polysemy
import Polysemy.TinyLog qualified as Log
import System.Logger.Class qualified as LogClass
import Wire.BackendNotificationSubsystem

-- | No-op interpreter that logs warnings when notifications can't be sent
-- Use this when RabbitMQ is not configured
runBackendNotificationSubsystemNoOp ::
  (Member Log.TinyLog r) =>
  InterpreterFor BackendNotificationSubsystem r
runBackendNotificationSubsystemNoOp = interpret $ \case
  SendBackendNotification originDomain targetDomain _notification -> do
    Log.warn $
      LogClass.msg (LogClass.val "Federation notification not sent: RabbitMQ not configured")
        . LogClass.field "origin_domain" (show originDomain)
        . LogClass.field "target_domain" (show targetDomain)

-- | In-memory interpreter for testing
-- Tracks all notifications that would have been sent
runBackendNotificationSubsystemInMemory ::
  ( Member (Embed IO) r,
    Member Log.TinyLog r
  ) =>
  IORef Int ->
  InterpreterFor BackendNotificationSubsystem r
runBackendNotificationSubsystemInMemory notificationsRef = interpret $ \case
  SendBackendNotification originDomain targetDomain _notification -> do
    embed $ modifyIORef notificationsRef (+ 1)
    Log.info $
      LogClass.msg (LogClass.val "Federation notification tracked (in-memory)")
        . LogClass.field "origin_domain" (show originDomain)
        . LogClass.field "target_domain" (show targetDomain)
