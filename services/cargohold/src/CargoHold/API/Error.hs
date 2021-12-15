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

import Data.Proxy
import qualified Data.Text.Lazy as LT
import GHC.TypeLits
import Imports
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error
import Servant.API.Status
import Wire.API.ErrorDescription

errorDescriptionToWai ::
  forall (code :: Nat) (lbl :: Symbol) (desc :: Symbol).
  (KnownStatus code, KnownSymbol lbl) =>
  ErrorDescription code lbl desc ->
  Error
errorDescriptionToWai (ErrorDescription msg) =
  mkError
    (statusVal (Proxy @code))
    (LT.pack (symbolVal (Proxy @lbl)))
    (LT.fromStrict msg)

errorDescriptionTypeToWai ::
  forall e (code :: Nat) (lbl :: Symbol) (desc :: Symbol).
  ( KnownStatus code,
    KnownSymbol lbl,
    KnownSymbol desc,
    e ~ ErrorDescription code lbl desc
  ) =>
  Error
errorDescriptionTypeToWai = errorDescriptionToWai (mkErrorDescription :: e)

assetTooLarge :: Error
assetTooLarge = errorDescriptionTypeToWai @AssetTooLarge

unauthorised :: Error
unauthorised = errorDescriptionTypeToWai @Unauthorised

invalidLength :: Error
invalidLength = errorDescriptionTypeToWai @InvalidLength

assetNotFound :: Error
assetNotFound = errorDescriptionTypeToWai @AssetNotFound

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
