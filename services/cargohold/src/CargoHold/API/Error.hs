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

module CargoHold.API.Error where

import Imports
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error
import Wire.API.Error
import Wire.API.Error.Cargohold

assetTooLarge :: Error
assetTooLarge = errorToWai @'AssetTooLarge

unauthorised :: Error
unauthorised = errorToWai @'Unauthorised

invalidLength :: Error
invalidLength = errorToWai @'InvalidLength

assetNotFound :: Error
assetNotFound = errorToWai @'AssetNotFound

unverifiedUser :: Error
unverifiedUser = errorToWai @'UnverifiedUser

userNotFound :: Error
userNotFound = errorToWai @'UserNotFound

noMatchingAssetEndpoint :: Error
noMatchingAssetEndpoint = errorToWai @'NoMatchingAssetEndpoint

clientError :: LText -> Error
clientError = mkError status400 "client-error"

serverError :: Error
serverError = mkError status500 "server-error" "Server Error."
