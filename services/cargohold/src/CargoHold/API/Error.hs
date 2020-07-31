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

module CargoHold.API.Error where

import CargoHold.Types.V3.Resumable (Offset, TotalSize)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Imports
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error

assetTooLarge :: Error
assetTooLarge = Error status413 "client-error" "Asset too large."

unauthorised :: Error
unauthorised = Error status403 "unauthorised" "Unauthorised operation."

invalidLength :: Error
invalidLength = Error status400 "invalid-length" "Invalid content length."

assetNotFound :: Error
assetNotFound = Error status404 "not-found" "Asset not found."

invalidMD5 :: Error
invalidMD5 = Error status400 "client-error" "Invalid MD5."

requestTimeout :: Error
requestTimeout =
  Error
    status408
    "request-timeout"
    "The request timed out. The server was still expecting more data \
    \but none was sent over an extended period of time. Idle connections \
    \will be closed."

invalidOffset :: Offset -> Offset -> Error
invalidOffset expected given =
  Error status409 "invalid-offset" $
    toLazyText $
      "Invalid offset: "
        <> "expected: "
        <> decimal expected
        <> ", "
        <> "given: "
        <> decimal given
        <> "."

uploadTooSmall :: Error
uploadTooSmall =
  Error
    status403
    "client-error"
    "The current chunk size is \
    \smaller than the minimum allowed."

uploadTooLarge :: Error
uploadTooLarge =
  Error
    status413
    "client-error"
    "The current chunk size + offset \
    \is larger than the full upload size."

uploadIncomplete :: TotalSize -> TotalSize -> Error
uploadIncomplete expected actual =
  Error status403 "client-error" $
    toLazyText $
      "The upload is incomplete: "
        <> "expected size: "
        <> decimal expected
        <> ", "
        <> "current size: "
        <> decimal actual
        <> "."

clientError :: LText -> Error
clientError = Error status400 "client-error"

serverError :: Error
serverError = Error status500 "server-error" "Server Error."
