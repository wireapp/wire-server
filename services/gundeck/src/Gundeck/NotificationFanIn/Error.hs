-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Gundeck.NotificationFanIn.Error
  ( fanInSubsystemErrorToHttpError,
    fanInStoreErrorToHttpError,
    fanInMembershipStoreErrorToHttpError,
    fanInPostgresErrorToHttpError,
  )
where

import Hasql.Pool (UsageError)
import Network.HTTP.Types (status500)
import Network.Wai.Utilities.Error qualified as Wai
import Wire.API.Error (errorToWai)
import Wire.API.Error.Gundeck qualified as E
import Wire.FanInEpochMembershipStore
import Wire.FanInNotificationStore
import Wire.FanInNotificationSubsystem

fanInSubsystemErrorToHttpError :: FanInNotificationSubsystemError -> Wai.Error
fanInSubsystemErrorToHttpError = \case
  FanInInvalidNotificationId -> errorToWai @'E.FanInInvalidNotificationId
  FanInTransientUnsupported -> errorToWai @'E.FanInTransientUnsupported
  FanInLocalDomainMissing -> errorToWai @'E.FanInLocalDomainMissing
  FanInOperationNotImplemented -> Wai.mkError status500 "server-error" "Fan-in operation is unavailable"

fanInStoreErrorToHttpError :: FanInNotificationStoreError -> Wai.Error
fanInStoreErrorToHttpError = \case
  InvalidFanInNotificationId _ -> errorToWai @'E.FanInInvalidNotificationId

-- | Database diagnostics can contain SQL and parameters. Keep them out of the
-- HTTP response, including errors from the shared Hasql pool infrastructure.
fanInPostgresErrorToHttpError :: UsageError -> Wai.Error
fanInPostgresErrorToHttpError _ = Wai.mkError status500 "server-error" "Could not persist fan-in notifications"

fanInMembershipStoreErrorToHttpError :: FanInEpochMembershipStoreError -> Wai.Error
fanInMembershipStoreErrorToHttpError = \case
  FanInStaleEpoch -> errorToWai @'E.FanInStaleEpoch
  FanInConflictingEpoch -> errorToWai @'E.FanInConflictingEpoch
