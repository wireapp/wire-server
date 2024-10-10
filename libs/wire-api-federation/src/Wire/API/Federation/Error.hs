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

-- | Map federation errors to client-facing errors.
--
-- This module contains most of the error-mapping logic that turns the various
-- possible errors that can occur while making a federated request into errors
-- that are meaningful for the clients.
--
-- There are three types of errors, from lowest level to highest:
--
--  * 'FederatorClientHTTP2Error': this is thrown when something fails while
--     connecting or making a request to the local federator.
--  * 'FederatorClientError': this is the most common type of error,
--     corresponding to a failure at the level of the federator client. It
--     includes, for example, a failure to reach a remote federator, or an
--     error on the remote side.
--  * 'FederationError': this is created by users of the federator client. It
--     can either wrap a 'FederatorClientError', or be an error that is outside
--     the scope of the client, such as when a federated request succeeds with
--     an unexpected result.
--
-- A general federated request is normally performed as a chain of HTTP
-- requests (some of which are HTTP2). Errors can occur at each node of the
-- chain, as well as in the communication between two adjacent nodes. A
-- successful request goes through the following stages:
--
--  1) a service (say brig) makes a request to (the outward service of) the
--     local federator (HTTP2);
--  2) the local federator processes this request;
--  3) the local federator makes a request to (the inward service of) a remote
--     one (HTTP2);
--  4) the remote federator processes this request;
--  5) from the remote federator to a service on that backend (HTTP);
--  6) the remote service processes this request.
--
-- Failures at step 1 in the chain result in 'FederatorClientHTTP2Error', while
-- any other failure results in a 'FederatorClientError'.
--
-- Immediate failures in the outward service of a federator (stage 2) result in
-- a 403 status code being returned to the federator client, which is then
-- translated into an error with label federation-local-error.
--
-- Failures which occurred while making a request to a remote federator (stages
-- 3 to 6) are turned into 5xx errors by federator itself, and then passed on
-- through without any further mapping. This includes issues in stage 4,
-- which are seen by the local federator as 403 status codes returned by the
-- remote, as well as arbitrary error codes returned by a service.
--
-- Note that the federation API follows the convention that any error should be
-- returned as part of a successful response with status code 200. Therefore any
-- error response from services during a federated call should be considered a bug
-- in the implementation of the federation API, and is therefore wrapped in a 533.
module Wire.API.Federation.Error
  ( -- * Federation errors
    FederatorClientHTTP2Error (..),
    FederatorClientError (..),
    FederationError (..),
    VersionNegotiationError (..),
    federationErrorToWai,
    federationRemoteHTTP2Error,
    federationRemoteResponseError,
    federationNotImplemented,
    federationNotConfigured,

    -- * Error status codes
    unexpectedFederationResponseStatus,
    federatorConnectionRefusedStatus,
  )
where

import Data.Aeson qualified as Aeson
import Data.Domain
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Imports
import Network.HTTP.Types.Status
import Network.HTTP.Types.Status qualified as HTTP
import Network.HTTP2.Client qualified as HTTP2
import Network.Wai.Utilities.Error qualified as Wai
import OpenSSL.Session (SomeSSLException)
import Servant.Client
import Wire.API.Error
import Wire.Network.DNS.SRV

-- | Transport-layer errors in federator client.
data FederatorClientHTTP2Error
  = FederatorClientNoStatusCode
  | FederatorClientHTTP2Exception HTTP2.HTTP2Error
  | FederatorClientTLSException SomeSSLException
  | FederatorClientConnectionError IOException
  deriving (Show, Typeable)

instance Exception FederatorClientHTTP2Error

-- | Possible errors resulting from a use of the federator client.
data FederatorClientError
  = -- | An error that occurred when establishing a connection to or
    -- communicating with the local federator.
    FederatorClientHTTP2Error FederatorClientHTTP2Error
  | -- | Federator client does not currently support streaming, so this error
    -- will be thrown when using federator client to call APIs that contain a
    -- streaming body.
    FederatorClientStreamingNotSupported
  | -- | This error will be thrown when the response received from federator
    -- cannot be parsed by the servant machinery (e.g. its content type is
    -- malformed or unsupported).
    FederatorClientServantError ClientError
  | -- | This error will be thrown when federator returns an error response.
    FederatorClientError Wai.Error
  | -- | This happens when an invalid version information response is returned
    -- by federator, or when negotiation fails because no common version could
    -- be found.
    FederatorClientVersionNegotiationError VersionNegotiationError
  | -- | This happens when no endpoint for the negotiated version could be
    -- found among the alternative. This error could in principle be checked
    -- statically, but it is not trivial to do so.
    FederatorClientVersionMismatch
  deriving (Show, Typeable)

instance Exception FederatorClientError

-- | High level federation errors. When something goes wrong during a federated
-- call, this error type should be used to represent the failure that occurred.
--
-- Note that federator client itself can only throw errors of type
-- 'FederatorClientError', corresponding to the 'FederationCallFailure'
-- constructor of 'FederationError'.
data FederationError
  = -- | To be used by endpoints to signal federation code paths that haven't
    -- been fully implemented yet.
    FederationNotImplemented
  | -- | No federator endpoint has been set, so no call to federator client can
    -- be made.
    FederationNotConfigured
  | -- | Federation is disabled for the given protocol
    FederationDisabledForProtocol
  | -- | An error occurred while invoking federator client (see
    -- 'FederatorClientError' for more details).
    FederationCallFailure FederatorClientError
  | -- | Federator client was invoked successfully, but the returned value is
    -- incorrect. For example, if a single conversation was requested from the
    -- remote backend, but multiple conversations have been returned. This can
    -- indicate a bug in either backend, or an incompatibility in the
    -- server-to-server API.
    FederationUnexpectedBody Text
  | -- | Federator client got an unexpected error response from remote backend.
    -- Also used for error conditions that will go away in a future release,
    -- like "can't delete remote domains from config file", which is only
    -- needed until we start disregarding the config file.
    FederationUnexpectedError Text
  deriving (Show, Typeable)

data VersionNegotiationError
  = InvalidVersionInfo
  | RemoteTooOld
  | RemoteTooNew
  deriving (Show, Typeable)

versionNegotiationErrorMessage :: VersionNegotiationError -> LText
versionNegotiationErrorMessage InvalidVersionInfo =
  "Remote federator returned invalid version information"
versionNegotiationErrorMessage RemoteTooOld =
  "Version negotiation failed: the remote backend is too old"
versionNegotiationErrorMessage RemoteTooNew =
  "Version negotiation failed: the remote backend is too new"

instance Exception FederationError

instance APIError FederationError where
  toResponse = toResponse . federationErrorToWai

federationErrorToWai :: FederationError -> Wai.Error
federationErrorToWai FederationNotImplemented = federationNotImplemented
federationErrorToWai FederationNotConfigured = federationNotConfigured
federationErrorToWai FederationDisabledForProtocol = federationDisabledForProtocol
federationErrorToWai (FederationCallFailure err) = federationClientErrorToWai err
federationErrorToWai (FederationUnexpectedBody s) = federationUnexpectedBody s
federationErrorToWai (FederationUnexpectedError t) = federationUnexpectedError t

federationClientErrorToWai :: FederatorClientError -> Wai.Error
federationClientErrorToWai (FederatorClientHTTP2Error e) =
  federationClientHTTP2Error e
federationClientErrorToWai FederatorClientStreamingNotSupported =
  Wai.mkError HTTP.status500 "internal-error" "Federated streaming not implemented"
federationClientErrorToWai (FederatorClientServantError err) =
  federationServantErrorToWai err
federationClientErrorToWai (FederatorClientError err) = err
federationClientErrorToWai (FederatorClientVersionNegotiationError err) =
  Wai.mkError
    unexpectedFederationResponseStatus
    "federation-version-error"
    (versionNegotiationErrorMessage err)
federationClientErrorToWai FederatorClientVersionMismatch =
  Wai.mkError
    HTTP.status500
    "internal-error"
    "Endpoint version mismatch in federation client"

federationRemoteHTTP2Error :: SrvTarget -> Text -> FederatorClientHTTP2Error -> Wai.Error
federationRemoteHTTP2Error target path = \case
  FederatorClientNoStatusCode ->
    ( Wai.mkError
        unexpectedFederationResponseStatus
        "federation-http2-error"
        "No status code in HTTP2 response"
    )
      & addErrData
  (FederatorClientHTTP2Exception e) ->
    ( Wai.mkError
        unexpectedFederationResponseStatus
        "federation-http2-error"
        (LT.pack (displayException e))
    )
      & addErrData
  (FederatorClientTLSException e) ->
    ( Wai.mkError
        (HTTP.mkStatus 525 "SSL Handshake Failure")
        "federation-tls-error"
        (LT.pack (displayException e))
    )
      & addErrData
  (FederatorClientConnectionError e) ->
    ( Wai.mkError
        federatorConnectionRefusedStatus
        "federation-connection-refused"
        (LT.pack (displayException e))
    )
      & addErrData
  where
    addErrData err =
      err
        { Wai.errorData =
            ( (mkDomain . T.decodeUtf8With T.lenientDecode . srvTargetDomain $ target) ::
                Either String Domain
            )
              & either (const Nothing) (\dom -> Just (Wai.FederationErrorData dom path))
        }

federationClientHTTP2Error :: FederatorClientHTTP2Error -> Wai.Error
federationClientHTTP2Error (FederatorClientConnectionError e) =
  Wai.mkError
    HTTP.status500
    "federation-not-available"
    (LT.pack (displayException e))
federationClientHTTP2Error e =
  Wai.mkError
    HTTP.status500
    "federation-local-error"
    (LT.pack (displayException e))

federationRemoteResponseError :: SrvTarget -> Text -> HTTP.Status -> LByteString -> Wai.Error
federationRemoteResponseError target path status body =
  ( Wai.mkError
      unexpectedFederationResponseStatus
      "federation-remote-error"
      ( "A remote federator failed with status code: "
          <> LT.pack (show (HTTP.statusCode status))
      )
  )
    { Wai.errorData =
        ( (mkDomain . T.decodeUtf8With T.lenientDecode . srvTargetDomain $ target) ::
            Either String Domain
        )
          & either (const Nothing) (\dom -> Just (Wai.FederationErrorData dom path)),
      Wai.innerError =
        Just $
          fromMaybe
            ( Wai.mkError
                status
                "unknown-error"
                (LT.decodeUtf8With T.lenientDecode body)
            )
            (Aeson.decode body)
    }

federationServantErrorToWai :: ClientError -> Wai.Error
federationServantErrorToWai (DecodeFailure msg _) = federationInvalidBody msg
-- the following error is never thrown by federator client
federationServantErrorToWai (FailureResponse _ _) = federationUnknownError
federationServantErrorToWai (InvalidContentTypeHeader res) =
  Wai.mkError
    unexpectedFederationResponseStatus
    "federation-invalid-content-type"
    ("Content-type: " <> federationErrorContentType res)
federationServantErrorToWai (UnsupportedContentType mediaType res) =
  Wai.mkError
    unexpectedFederationResponseStatus
    "federation-unsupported-content-type"
    ( "Content-type: "
        <> federationErrorContentType res
        <> ", Media-Type: "
        <> LT.pack (show mediaType)
    )
federationServantErrorToWai (ConnectionError e) =
  federationUnavailable . T.pack . displayException $ e

federationErrorContentType :: ResponseF a -> LT.Text
federationErrorContentType =
  LT.fromStrict
    . T.decodeUtf8
    . maybe "" snd
    . find (\(name, _) -> name == "Content-Type")
    . responseHeaders

unexpectedFederationResponseStatus :: Status
unexpectedFederationResponseStatus = HTTP.Status 533 "Unexpected Federation Response"

federatorConnectionRefusedStatus :: Status
federatorConnectionRefusedStatus = HTTP.Status 521 "Remote Federator Connection Refused"

federationNotImplemented :: Wai.Error
federationNotImplemented =
  Wai.mkError
    HTTP.status500
    "federation-not-implemented"
    "Federation is not yet implemented for this endpoint"

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

federationUnexpectedError :: Text -> Wai.Error
federationUnexpectedError msg =
  Wai.mkError
    unexpectedFederationResponseStatus
    "federation-unexpected-wai-error"
    ("Could parse body, but got an unexpected error response: " <> LT.fromStrict msg)

federationNotConfigured :: Wai.Error
federationNotConfigured =
  Wai.mkError
    HTTP.status400
    "federation-not-enabled"
    "no federator configured"

federationDisabledForProtocol :: Wai.Error
federationDisabledForProtocol =
  Wai.mkError
    HTTP.status409
    "federation-disabled-for-protocol"
    "Federation is disabled for the given protocol"

federationUnavailable :: Text -> Wai.Error
federationUnavailable err =
  Wai.mkError
    HTTP.status500
    "federation-not-available"
    ("Local federator not available: " <> LT.fromStrict err)

federationUnknownError :: Wai.Error
federationUnknownError =
  Wai.mkError
    unexpectedFederationResponseStatus
    "unknown-federation-error"
    "Unknown federation error"
