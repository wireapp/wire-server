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

import Imports
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error

assetTooLarge :: Error
assetTooLarge = mkError status413 "client-error" "Asset too large."

unauthorised :: Error
unauthorised = mkError status403 "unauthorised" "Unauthorised operation."

invalidLength :: Error
invalidLength = mkError status400 "invalid-length" "Invalid content length."

assetNotFound :: Error
assetNotFound = mkError status404 "not-found" "Asset not found."

invalidMD5 :: Error
invalidMD5 = mkError status400 "client-error" "Invalid MD5."

requestTimeout :: Error
requestTimeout =
  mkError
    status408
    "request-timeout"
    "The request timed out. The server was still expecting more data \
    \but none was sent over an extended period of time. Idle connections \
    \will be closed."

uploadTooSmall :: Error
uploadTooSmall =
  mkError
    status403
    "client-error"
    "The current chunk size is \
    \smaller than the minimum allowed."

uploadTooLarge :: Error
uploadTooLarge =
  mkError
    status413
    "client-error"
    "The current chunk size + offset \
    \is larger than the full upload size."

clientError :: LText -> Error
clientError = mkError status400 "client-error"

serverError :: Error
serverError = mkError status500 "server-error" "Server Error."
