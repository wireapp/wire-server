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

module Wire.API.Federation.Error
  ( FederatorClientHTTP2Error (..),
    FederatorClientError (..),
    FederationError (..),
    federationErrorToWai,
    federationRemoteHTTP2Error,
    federationRemoteResponseError,
    federationNotImplemented,
    federationNotConfigured,
  )
where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import Imports
import Network.HTTP.Types.Status
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.HTTP2.Frame as HTTP2
import Network.TLS
import qualified Network.Wai.Utilities.Error as Wai
import Servant.Client

-- | Transport-layer errors in federator client.
data FederatorClientHTTP2Error
  = FederatorClientNoStatusCode
  | FederatorClientHTTP2Exception HTTP2.HTTP2Error
  | FederatorClientTLSException TLSException
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
  | -- | An error occurred while invoking federator client (see
    -- 'FederatorClientError' for more details).
    FederationCallFailure FederatorClientError
  | -- | Federator client was invoked successfully, but the returned value is
    -- incorrect. For example, if a single conversation was requested from the
    -- remote backend, but multiple conversations have been returned. This can
    -- indicate a bug in either backend, or an incompatibility in the
    -- server-to-server API.
    FederationUnexpectedBody Text
  deriving (Show, Typeable)

instance Exception FederationError

federationErrorToWai :: FederationError -> Wai.Error
federationErrorToWai FederationNotImplemented = federationNotImplemented
federationErrorToWai FederationNotConfigured = federationNotConfigured
federationErrorToWai (FederationCallFailure err) = federationClientErrorToWai err
federationErrorToWai (FederationUnexpectedBody s) = federationUnexpectedBody s

federationClientErrorToWai :: FederatorClientError -> Wai.Error
federationClientErrorToWai (FederatorClientHTTP2Error e) =
  federationClientHTTP2Error e
federationClientErrorToWai FederatorClientStreamingNotSupported =
  Wai.mkError HTTP.status500 "internal-error" "Federated streaming not implemented"
federationClientErrorToWai (FederatorClientServantError err) =
  federationServantErrorToWai err
federationClientErrorToWai (FederatorClientError err) = err

federationRemoteHTTP2Error :: FederatorClientHTTP2Error -> Wai.Error
federationRemoteHTTP2Error FederatorClientNoStatusCode =
  Wai.mkError
    unexpectedFederationResponseStatus
    "federation-http2-error"
    "No status code in HTTP2 response"
federationRemoteHTTP2Error (FederatorClientHTTP2Exception e) =
  Wai.mkError
    unexpectedFederationResponseStatus
    "federation-http2-error"
    (LT.pack (displayException e))
federationRemoteHTTP2Error (FederatorClientTLSException e) =
  Wai.mkError
    (HTTP.mkStatus 525 "SSL Handshake Failure")
    "tls-failure"
    (LT.fromStrict (displayTLSException e))
federationRemoteHTTP2Error (FederatorClientConnectionError e) =
  Wai.mkError
    federatorConnectionRefusedStatus
    "federation-connection-refused"
    (LT.pack (displayException e))

federationClientHTTP2Error :: FederatorClientHTTP2Error -> Wai.Error
federationClientHTTP2Error (FederatorClientConnectionError e) =
  Wai.mkError
    HTTP.status500
    "federation-not-available"
    (LT.pack (displayException e))
federationClientHTTP2Error e =
  Wai.mkError
    HTTP.status500
    "federator-client-error"
    (LT.pack (displayException e))

federationRemoteResponseError :: HTTP.Status -> Wai.Error
federationRemoteResponseError status =
  Wai.mkError
    unexpectedFederationResponseStatus
    "federation-remote-error"
    ( "A remote federator failed with status code "
        <> LT.pack (show (HTTP.statusCode status))
    )

displayTLSException :: TLSException -> Text
displayTLSException (Terminated _ reason err) = T.pack reason <> ": " <> displayTLSError err
displayTLSException (HandshakeFailed err) = T.pack "handshake failed: " <> displayTLSError err
displayTLSException ConnectionNotEstablished = T.pack "connection not established"

displayTLSError :: TLSError -> Text
displayTLSError (Error_Misc msg) = T.pack msg
displayTLSError (Error_Protocol (msg, _, _)) = "protocol error: " <> T.pack msg
displayTLSError (Error_Certificate msg) = "certificate error: " <> T.pack msg
displayTLSError (Error_HandshakePolicy msg) = "handshake policy error: " <> T.pack msg
displayTLSError Error_EOF = "end-of-file error"
displayTLSError (Error_Packet msg) = "packet error: " <> T.pack msg
displayTLSError (Error_Packet_unexpected actual expected) =
  "unexpected packet: " <> T.pack expected <> ", " <> "got " <> T.pack actual
displayTLSError (Error_Packet_Parsing msg) = "packet parsing error: " <> T.pack msg

federationServantErrorToWai :: ClientError -> Wai.Error
federationServantErrorToWai (DecodeFailure msg _) = federationInvalidBody msg
federationServantErrorToWai (FailureResponse _ _) = federationUnknownError
federationServantErrorToWai (InvalidContentTypeHeader res) =
  Wai.mkError
    unexpectedFederationResponseStatus
    "federation-invalid-content-type-header"
    ("Content-type: " <> federationErrorContentType res)
federationServantErrorToWai (UnsupportedContentType mediaType res) =
  Wai.mkError
    unexpectedFederationResponseStatus
    "federation-unsupported-content-type"
    ( "Content-type: " <> federationErrorContentType res
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

federationUnknownError :: Wai.Error
federationUnknownError =
  Wai.mkError
    unexpectedFederationResponseStatus
    "unknown-federation-error"
    "Unknown federation error"
