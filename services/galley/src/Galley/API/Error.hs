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

-- | Most of the errors thrown by galley are defined as static errors in
-- 'Wire.API.Error.Galley' and declared as part of the API. Errors defined here
-- are dynamic, and mostly internal.
module Galley.API.Error
  ( -- * Internal errors
    InvalidInput (..),
    InternalError (..),
    internalErrorWithDescription,
    internalErrorDescription,
    legalHoldServiceUnavailable,
  )
where

import Data.Id
import Data.Text.Lazy as LT (pack)
import Imports
import Network.HTTP.Types.Status
import Network.Wai.Utilities (Error (message))
import Network.Wai.Utilities.Error qualified as Wai
import Wire.API.Error

data InternalError
  = BadConvState ConvId
  | BadMemberState
  | NoPrekeyForUser
  | CannotCreateManagedConv
  | InternalErrorWithDescription LText
  deriving (Eq)

internalErrorDescription :: InternalError -> LText
internalErrorDescription = message . internalErrorToWai

internalErrorToWai :: InternalError -> Wai.Error
internalErrorToWai (BadConvState convId) = badConvState convId
internalErrorToWai BadMemberState = Wai.mkError status500 "bad-state" "Bad internal member state."
internalErrorToWai NoPrekeyForUser = internalError
internalErrorToWai CannotCreateManagedConv = internalError
internalErrorToWai (InternalErrorWithDescription t) = internalErrorWithDescription t

instance APIError InternalError where
  toResponse = toResponse . internalErrorToWai

data InvalidInput
  = CustomRolesNotSupported
  | InvalidRange LText
  | InvalidUUID4
  | InvalidPayload LText
  | FederationFunctionNotSupported LText

instance APIError InvalidInput where
  toResponse CustomRolesNotSupported = toResponse $ badRequest "Custom roles not supported"
  toResponse (InvalidRange t) = toResponse $ invalidRange t
  toResponse InvalidUUID4 = toResponse invalidUUID4
  toResponse (InvalidPayload t) = toResponse $ invalidPayload t
  toResponse (FederationFunctionNotSupported t) = toResponse $ federationFunctionNotSupported t

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

federationFunctionNotSupported :: LText -> Wai.Error
federationFunctionNotSupported = Wai.mkError status400 "federation-function-not-supported"

invalidUUID4 :: Wai.Error
invalidUUID4 = Wai.mkError status400 "client-error" "Invalid UUID v4 format"

invalidRange :: LText -> Wai.Error
invalidRange = Wai.mkError status400 "client-error"

badConvState :: ConvId -> Wai.Error
badConvState cid =
  Wai.mkError status500 "bad-state" $
    "Connect conversation with more than 2 members: "
      <> LT.pack (show cid)

legalHoldServiceUnavailable :: (Show a) => a -> Wai.Error
legalHoldServiceUnavailable e = Wai.mkError status412 "legalhold-unavailable" ("legal hold service unavailable with underlying error: " <> (LT.pack . show $ e))
