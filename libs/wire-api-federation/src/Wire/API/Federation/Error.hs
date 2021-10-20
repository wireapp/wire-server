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

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import Imports
import Network.HTTP.Types.Status
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai.Utilities.Error as Wai
import qualified Servant.Client as Servant
import Wire.API.Federation.Client
  ( FederationClientError (..),
    FederationClientFailure (..),
    FederationError (..),
  )
import qualified Wire.API.Federation.GRPC.Types as Proto

federationErrorToWai :: FederationError -> Wai.Error
federationErrorToWai (FederationUnavailable err) = federationUnavailable err
federationErrorToWai FederationNotImplemented = federationNotImplemented
federationErrorToWai FederationNotConfigured = federationNotConfigured
federationErrorToWai (FederationCallFailure failure) = addErrorData $
  case fedFailError failure of
    FederationClientRPCError msg ->
      Wai.mkError
        HTTP.status500
        "client-rpc-error"
        (LT.fromStrict msg)
    FederationClientInvalidMethod mth ->
      federationInvalidCall
        ("Unexpected method: " <> LT.fromStrict (T.decodeUtf8 mth))
    FederationClientStreamingUnsupported -> federationInvalidCall "Streaming unsupported"
    FederationClientOutwardError outwardErr -> federationRemoteError outwardErr
    FederationClientInwardError inwardErr -> federationRemoteInwardError inwardErr
    FederationClientServantError (Servant.DecodeFailure msg _) -> federationInvalidBody msg
    FederationClientServantError (Servant.FailureResponse _ _) ->
      Wai.mkError unexpectedFederationResponseStatus "unknown-federation-error" "Unknown federation error"
    FederationClientServantError (Servant.InvalidContentTypeHeader res) ->
      Wai.mkError
        unexpectedFederationResponseStatus
        "federation-invalid-content-type-header"
        ("Content-type: " <> contentType res)
    FederationClientServantError (Servant.UnsupportedContentType mediaType res) ->
      Wai.mkError
        unexpectedFederationResponseStatus
        "federation-unsupported-content-type"
        ("Content-type: " <> contentType res <> ", Media-Type: " <> LT.pack (show mediaType))
    FederationClientServantError (Servant.ConnectionError exception) ->
      federationUnavailable . T.pack . show $ exception
  where
    contentType = LT.fromStrict . T.decodeUtf8 . maybe "" snd . find (\(name, _) -> name == "Content-Type") . Servant.responseHeaders
    addErrorData :: Wai.Error -> Wai.Error
    addErrorData err =
      err
        { Wai.errorData =
            Just
              Wai.FederationErrorData
                { Wai.federrDomain = fedFailDomain failure,
                  Wai.federrPath = T.decodeUtf8 (fedFailPath failure)
                }
        }

noFederationStatus :: Status
noFederationStatus = status403

unexpectedFederationResponseStatus :: Status
unexpectedFederationResponseStatus = HTTP.Status 533 "Unexpected Federation Response"

federatorConnectionRefusedStatus :: Status
federatorConnectionRefusedStatus = HTTP.Status 521 "Remote Federator Connection Refused"

federationNotImplemented :: Wai.Error
federationNotImplemented =
  Wai.mkError
    noFederationStatus
    "federation-not-implemented"
    "Federation is not yet implemented for this endpoint"

federationInvalidCode :: Word32 -> Wai.Error
federationInvalidCode code =
  Wai.mkError
    unexpectedFederationResponseStatus
    "federation-invalid-code"
    ("Invalid response code from remote federator: " <> LT.pack (show code))

federationInvalidBody :: Text -> Wai.Error
federationInvalidBody msg =
  Wai.mkError
    unexpectedFederationResponseStatus
    "federation-invalid-body"
    ("Could not parse remote federator response: " <> LT.fromStrict msg)

federationUnexpectedBody :: Text -> Wai.Error
federationUnexpectedBody msg =
  Wai.mkError
    unexpectedFederationResponseStatus
    "federation-unexpected-body"
    ("Could parse body, but response was not expected: " <> LT.fromStrict msg)

federationNotConfigured :: Wai.Error
federationNotConfigured =
  Wai.mkError
    HTTP.status400
    "federation-not-enabled"
    "no federator configured"

federationUnavailable :: Text -> Wai.Error
federationUnavailable err =
  Wai.mkError
    HTTP.status500
    "federation-not-available"
    ("Local federator not available: " <> LT.fromStrict err)

federationRemoteInwardError :: Proto.InwardError -> Wai.Error
federationRemoteInwardError err = Wai.mkError status (LT.fromStrict label) (LT.fromStrict msg)
  where
    msg = Proto.inwardErrorMsg err
    (status, label) = case Proto.inwardErrorType err of
      Proto.IInvalidEndpoint -> (HTTP.Status 531 "Version Mismatch", "inward-invalid-endpoint")
      Proto.IFederationDeniedByRemote -> (HTTP.Status 532 "Federation Denied", "federation-denied-by-remote")
      Proto.IAuthenticationFailed -> (unexpectedFederationResponseStatus, "server-to-server-authentication-failed")
      Proto.IForbiddenEndpoint -> (unexpectedFederationResponseStatus, "forbidden-endpoint")
      Proto.IDiscoveryFailed -> (HTTP.status500, "remote-discovery-failure")
      Proto.IOther -> (unexpectedFederationResponseStatus, "inward-other")

federationRemoteError :: Proto.OutwardError -> Wai.Error
federationRemoteError err = case Proto.outwardErrorType err of
  Proto.RemoteNotFound -> mkErr HTTP.status422 "srv-record-not-found"
  Proto.DiscoveryFailed -> mkErr HTTP.status500 "srv-lookup-dns-error"
  Proto.ConnectionRefused ->
    mkErr
      (HTTP.Status 521 "Web Server Is Down")
      "cannot-connect-to-remote-federator"
  Proto.TLSFailure -> mkErr (HTTP.Status 525 "SSL Handshake Failure") "tls-failure"
  Proto.VersionMismatch -> mkErr (HTTP.Status 531 "Version Mismatch") "version-mismatch"
  Proto.FederationDeniedByRemote ->
    mkErr
      (HTTP.Status 532 "Federation Denied")
      "federation-denied-remotely"
  Proto.FederationDeniedLocally -> mkErr HTTP.status400 "federation-not-allowed"
  Proto.TooMuchConcurrency -> mkErr unexpectedFederationResponseStatus "too-much-concurrency"
  Proto.GrpcError -> mkErr unexpectedFederationResponseStatus "grpc-error"
  Proto.InvalidRequest -> mkErr HTTP.status500 "invalid-request-to-federator"
  where
    mkErr status label = Wai.mkError status label (LT.fromStrict (Proto.outwardErrorMessage err))

federationInvalidCall :: LText -> Wai.Error
federationInvalidCall = Wai.mkError HTTP.status500 "federation-invalid-call"
