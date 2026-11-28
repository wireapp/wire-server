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

module Gundeck.NotificationFanIn.ErrorSpec (tests) where

import Data.Aeson qualified as A
import Data.Id (parseIdFromText)
import Gundeck.NotificationFanIn.Error
import Hasql.Pool (UsageError (AcquisitionTimeoutUsageError))
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Wire.FanInEpochMembershipStore
import Wire.FanInNotificationStore
import Wire.FanInNotificationSubsystem

tests :: TestTree
tests =
  testGroup
    "Fan-in HTTP errors"
    [ testCase "invalid ids are client errors" $
        A.toJSON (fanInSubsystemErrorToHttpError FanInInvalidNotificationId)
          @?= expected 400 "invalid-notification-id" "Fan-in notification IDs must be UUIDv7",
      testCase "unsupported transient events are explicit" $
        A.toJSON (fanInSubsystemErrorToHttpError FanInTransientUnsupported)
          @?= expected 501 "fan-in-transient-unsupported" "Transient fan-in notifications are not supported yet",
      testCase "missing local domain is a service configuration error" $
        A.toJSON (fanInSubsystemErrorToHttpError FanInLocalDomainMissing)
          @?= expected 503 "fan-in-domain-missing" "The local fan-in domain is not configured",
      testCase "unimplemented subsystem operations return a generic error" $
        A.toJSON (fanInSubsystemErrorToHttpError FanInOperationNotImplemented)
          @?= expected 500 "server-error" "Fan-in operation is unavailable",
      testCase "store validation errors do not expose the submitted id" $ do
        let nid = either (error . show) id (parseIdFromText "00000000-0000-0000-0000-000000000000")
        A.toJSON (fanInStoreErrorToHttpError (InvalidFanInNotificationId nid))
          @?= expected 400 "invalid-notification-id" "Fan-in notification IDs must be UUIDv7",
      testCase "stale epochs are conflicts without membership details" $
        A.toJSON (fanInMembershipStoreErrorToHttpError FanInStaleEpoch)
          @?= expected 409 "fan-in-stale-epoch" "A newer fan-in epoch is already recorded",
      testCase "conflicting epoch membership is a conflict without member details" $
        A.toJSON (fanInMembershipStoreErrorToHttpError FanInConflictingEpoch)
          @?= expected 409 "fan-in-conflicting-epoch" "The fan-in epoch already has different membership",
      testCase "database failures return a generic response without diagnostics" $
        A.toJSON (fanInPostgresErrorToHttpError AcquisitionTimeoutUsageError)
          @?= expected 500 "server-error" "Could not persist fan-in notifications"
    ]

expected :: Int -> Text -> Text -> A.Value
expected code label message = A.object ["code" A..= code, "label" A..= label, "message" A..= message]
