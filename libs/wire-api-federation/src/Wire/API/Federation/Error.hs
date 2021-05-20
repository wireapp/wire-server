-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Federation.Error where

import qualified Data.Text.Lazy as LT
import Imports
import Network.HTTP.Types.Status
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai.Utilities.Error as Wai
import qualified Wire.API.Federation.GRPC.Types as Proto

noFederationStatus :: Status
noFederationStatus = status403

unexpectedFederationResponseStatus :: Status
unexpectedFederationResponseStatus = HTTP.Status 533 "Unexpected Federation Response"

federatorConnectionRefusedStatus :: Status
federatorConnectionRefusedStatus = HTTP.Status 521 "Remote Federator Connection Refused"

federationNotImplemented :: Wai.Error
federationNotImplemented =
  Wai.Error
    noFederationStatus
    "federation-not-implemented"
    "Federation is not yet implemented for this endpoint"

federationInvalidCode :: Word32 -> Wai.Error
federationInvalidCode code =
  Wai.Error
    unexpectedFederationResponseStatus
    "federation-invalid-code"
    ("Invalid response code from remote federator: " <> LT.pack (show code))

federationInvalidBody :: Text -> Wai.Error
federationInvalidBody msg =
  Wai.Error
    unexpectedFederationResponseStatus
    "federation-invalid-body"
    ("Could not parse remote federator response: " <> LT.fromStrict msg)

federationNotConfigured :: Wai.Error
federationNotConfigured =
  Wai.Error
    HTTP.status400
    "federation-not-enabled"
    "no federator configured on brig"

federationRpcError :: Text -> Wai.Error
federationRpcError msg =
  Wai.Error
    HTTP.status500
    "federation-rpc-error"
    (LT.fromStrict msg)

federationUnavailable :: Text -> Wai.Error
federationUnavailable err =
  Wai.Error
    HTTP.status500
    "federation-not-available"
    ("Local federator not available: " <> LT.fromStrict err)

federationRemoteError :: Proto.OutwardError -> Wai.Error
federationRemoteError err = Wai.Error status (LT.fromStrict label) (LT.fromStrict msg)
  where
    decodeError :: Maybe Proto.ErrorPayload -> (Text, Text)
    decodeError Nothing = ("unknown-federation-error", "Unknown federation error")
    decodeError (Just (Proto.ErrorPayload label' msg')) = (label', msg')

    (label, msg) = decodeError (Proto.outwardErrorPayload err)

    status = case Proto.outwardErrorType err of
      Proto.RemoteNotFound -> HTTP.status422
      Proto.DiscoveryFailed -> HTTP.status500
      Proto.ConnectionRefused -> HTTP.Status 521 "Web Server Is Down"
      Proto.TLSFailure -> HTTP.Status 525 "SSL Handshake Failure"
      Proto.InvalidCertificate -> HTTP.Status 526 "Invalid SSL Certificate"
      Proto.VersionMismatch -> HTTP.Status 531 "Version Mismatch"
      Proto.FederationDeniedByRemote -> HTTP.Status 532 "Federation Denied"
      Proto.FederationDeniedLocally -> HTTP.status400
      Proto.RemoteFederatorError -> unexpectedFederationResponseStatus
      Proto.InvalidRequest -> HTTP.status500

federationInvalidCall :: LText -> Wai.Error
federationInvalidCall = Wai.Error HTTP.status500 "federation-invalid-call"
