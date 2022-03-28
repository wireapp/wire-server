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
{-# OPTIONS_GHC -Wwarn #-}

-- | Most of the errors thrown by galley are defined as static errors in
-- 'Wire.API.Error.Galley' and declared as part of the API. Errors defined here
-- are dynamic, and mostly internal.
module Galley.API.Error
  ( -- * Internal errors
    InvalidInput (..),
    InternalError (..),
    LegalHoldError (..),
    internalErrorWithDescription,
    legalHoldServiceUnavailable,

    -- * Errors thrown by wai-routing handlers
    bulkGetMemberLimitExceeded,
    invalidTeamNotificationId,
  )
where

import Data.Id
import Data.String.Conversions (cs)
import Data.Text.Lazy as LT (pack)
import Galley.Types.Teams (hardTruncationLimit)
import Imports
import Network.HTTP.Types.Status
import qualified Network.Wai.Utilities.Error as Wai
import Wire.API.Error
import qualified Wire.API.Error.Brig as BrigError

data InternalError
  = BadConvState ConvId
  | BadMemberState
  | NoPrekeyForUser
  | CannotCreateManagedConv
  | InternalErrorWithDescription LText

instance APIError InternalError where
  toWai (BadConvState convId) = badConvState convId
  toWai BadMemberState = Wai.mkError status500 "bad-state" "Bad internal member state."
  toWai NoPrekeyForUser = internalError
  toWai CannotCreateManagedConv = internalError
  toWai (InternalErrorWithDescription t) = internalErrorWithDescription t

data InvalidInput
  = CustomRolesNotSupported
  | InvalidRange LText
  | InvalidUUID4
  | BulkGetMemberLimitExceeded
  | InvalidPayload LText
  | InvalidTeamNotificationId

instance APIError InvalidInput where
  toWai CustomRolesNotSupported = badRequest "Custom roles not supported"
  toWai (InvalidRange t) = invalidRange t
  toWai InvalidUUID4 = invalidUUID4
  toWai BulkGetMemberLimitExceeded = bulkGetMemberLimitExceeded
  toWai (InvalidPayload t) = invalidPayload t
  toWai InvalidTeamNotificationId = invalidTeamNotificationId

data LegalHoldError
  = MissingLegalholdConsent
  | NoUserLegalHoldConsent
  | LegalHoldNotEnabled
  | LegalHoldDisableUnimplemented
  | LegalHoldServiceInvalidKey
  | LegalHoldServiceBadResponse
  | UserLegalHoldAlreadyEnabled
  | LegalHoldServiceNotRegistered
  | LegalHoldCouldNotBlockConnections
  | UserLegalHoldIllegalOperation
  | TooManyTeamMembersOnTeamWithLegalhold
  | NoLegalHoldDeviceAllocated
  | UserLegalHoldNotPending

instance APIError LegalHoldError where
  toWai MissingLegalholdConsent = errorToWai @'BrigError.MissingLegalholdConsent
  toWai NoUserLegalHoldConsent = userLegalHoldNoConsent
  toWai LegalHoldNotEnabled = legalHoldNotEnabled
  toWai LegalHoldDisableUnimplemented = legalHoldDisableUnimplemented
  toWai LegalHoldServiceInvalidKey = legalHoldServiceInvalidKey
  toWai LegalHoldServiceBadResponse = legalHoldServiceBadResponse
  toWai UserLegalHoldAlreadyEnabled = userLegalHoldAlreadyEnabled
  toWai LegalHoldServiceNotRegistered = legalHoldServiceNotRegistered
  toWai LegalHoldCouldNotBlockConnections = legalHoldCouldNotBlockConnections
  toWai UserLegalHoldIllegalOperation = userLegalHoldIllegalOperation
  toWai TooManyTeamMembersOnTeamWithLegalhold = tooManyTeamMembersOnTeamWithLegalhold
  toWai NoLegalHoldDeviceAllocated = noLegalHoldDeviceAllocated
  toWai UserLegalHoldNotPending = userLegalHoldNotPending

----------------------------------------------------------------------------
-- Other errors

internalError :: Wai.Error
internalError = internalErrorWithDescription "internal error"

internalErrorWithDescription :: LText -> Wai.Error
internalErrorWithDescription = Wai.mkError status500 "internal-error"

invalidPayload :: LText -> Wai.Error
invalidPayload = Wai.mkError status400 "invalid-payload"

badRequest :: LText -> Wai.Error
badRequest = Wai.mkError status400 "bad-request"

invalidUUID4 :: Wai.Error
invalidUUID4 = Wai.mkError status400 "client-error" "Invalid UUID v4 format"

invalidRange :: LText -> Wai.Error
invalidRange = Wai.mkError status400 "client-error"

badConvState :: ConvId -> Wai.Error
badConvState cid =
  Wai.mkError status500 "bad-state" $
    "Connect conversation with more than 2 members: "
      <> LT.pack (show cid)

bulkGetMemberLimitExceeded :: Wai.Error
bulkGetMemberLimitExceeded =
  Wai.mkError
    status400
    "too-many-uids"
    ("Can only process " <> cs (show @Int hardTruncationLimit) <> " user ids per request.")

tooManyTeamMembersOnTeamWithLegalhold :: Wai.Error
tooManyTeamMembersOnTeamWithLegalhold = Wai.mkError status403 "too-many-members-for-legalhold" "cannot add more members to team when legalhold service is enabled."

legalHoldServiceInvalidKey :: Wai.Error
legalHoldServiceInvalidKey = Wai.mkError status400 "legalhold-invalid-key" "legal hold service pubkey is invalid"

legalHoldServiceUnavailable :: Wai.Error
legalHoldServiceUnavailable = Wai.mkError status412 "legalhold-unavailable" "legal hold service does not respond or tls handshake could not be completed (did you pin the wrong public key?)"

legalHoldServiceNotRegistered :: Wai.Error
legalHoldServiceNotRegistered = Wai.mkError status400 "legalhold-not-registered" "legal hold service has not been registered for this team"

legalHoldServiceBadResponse :: Wai.Error
legalHoldServiceBadResponse = Wai.mkError status400 "legalhold-status-bad" "legal hold service: invalid response"

legalHoldNotEnabled :: Wai.Error
legalHoldNotEnabled = Wai.mkError status403 "legalhold-not-enabled" "legal hold is not enabled for this team"

legalHoldDisableUnimplemented :: Wai.Error
legalHoldDisableUnimplemented = Wai.mkError status403 "legalhold-disable-unimplemented" "legal hold cannot be disabled for whitelisted teams"

userLegalHoldAlreadyEnabled :: Wai.Error
userLegalHoldAlreadyEnabled = Wai.mkError status409 "legalhold-already-enabled" "legal hold is already enabled for this user"

userLegalHoldNoConsent :: Wai.Error
userLegalHoldNoConsent = Wai.mkError status409 "legalhold-no-consent" "user has not given consent to using legal hold"

userLegalHoldIllegalOperation :: Wai.Error
userLegalHoldIllegalOperation = Wai.mkError status500 "legalhold-illegal-op" "internal server error: inconsistent change of user's legalhold state"

userLegalHoldNotPending :: Wai.Error
userLegalHoldNotPending = Wai.mkError status412 "legalhold-not-pending" "legal hold cannot be approved without being in a pending state"

noLegalHoldDeviceAllocated :: Wai.Error
noLegalHoldDeviceAllocated = Wai.mkError status404 "legalhold-no-device-allocated" "no legal hold device is registered for this user. POST /teams/:tid/legalhold/:uid/ to start the flow."

legalHoldCouldNotBlockConnections :: Wai.Error
legalHoldCouldNotBlockConnections = Wai.mkError status500 "legalhold-internal" "legal hold service: could not block connections when resolving policy conflicts."

invalidTeamNotificationId :: Wai.Error
invalidTeamNotificationId = Wai.mkError status400 "invalid-notification-id" "Could not parse notification id (must be UUIDv1)."
