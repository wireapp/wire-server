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

module Federator.ExternalServer
  ( callInward,
    serveInward,
    RPC (..),
    CertHeader (..),
    server,
  )
where

import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.ByteString.Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Domain
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.X509 qualified as X509
import Federator.Discovery
import Federator.Env
import Federator.Error.ServerError
import Federator.Health qualified as Health
import Federator.Interpreter
import Federator.Metrics
import Federator.RPC
import Federator.Response
import Federator.Service
import Federator.Validation
import Imports
import Network.HTTP.Client (Manager)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import Servant qualified
import Servant.API
import Servant.API.Extended.Endpath
import Servant.Client.Core
import Servant.Server.Generic (AsServerT)
import System.Logger.Message qualified as Log
import Wire.API.Federation.Component
import Wire.API.Federation.Domain
import Wire.API.Routes.FederationDomainConfig
import Wire.API.VersionInfo

-- | Used to get PEM encoded certificate out of an HTTP header
newtype CertHeader = CertHeader X509.Certificate

instance FromHttpApiData CertHeader where
  parseUrlPiece :: Text -> Either Text CertHeader
  parseUrlPiece cert =
    bimap Text.pack CertHeader $ decodeCertificate $ HTTP.urlDecode True $ Text.encodeUtf8 cert

data API mode = API
  { status ::
      mode
        :- "i"
          :> "status"
          -- When specified only returns status of the internal service,
          -- otherwise ensures that the external service is also up.
          :> QueryFlag "standalone"
          :> Get '[PlainText] NoContent,
    externalRequest ::
      mode
        :- "federation"
          :> Capture "component" Component
          :> Capture "rpc" RPC
          :> Header' '[Required, Strict] OriginDomainHeaderName Domain
          :> Header' '[Required, Strict] "X-SSL-Certificate" CertHeader
          :> Endpath
          -- We need to use 'RawM' so we can stream request body regardless of
          -- content-type and send a response with arbitrary content-type.
          :> RawM
  }
  deriving (Generic)

server ::
  ( Member ServiceStreaming r,
    Member (Embed IO) r,
    Member TinyLog r,
    Member DiscoverFederator r,
    Member (Error ValidationError) r,
    Member (Error DiscoveryFailure) r,
    Member (Error ServerError) r,
    Member (Error Servant.ServerError) r,
    Member (Input FederationDomainConfigs) r,
    Member Metrics r
  ) =>
  Manager ->
  Word16 ->
  API (AsServerT (Sem r))
server mgr intPort =
  API
    { status = Health.status mgr "internal server" intPort,
      externalRequest = callInward
    }

-- FUTUREWORK(federation): Versioning of the federation API.
callInward ::
  ( Member ServiceStreaming r,
    Member (Embed IO) r,
    Member TinyLog r,
    Member DiscoverFederator r,
    Member (Error ValidationError) r,
    Member (Error DiscoveryFailure) r,
    Member (Error ServerError) r,
    Member (Input FederationDomainConfigs) r,
    Member Metrics r
  ) =>
  Component ->
  RPC ->
  Domain ->
  CertHeader ->
  Wai.Request ->
  (Wai.Response -> IO Wai.ResponseReceived) ->
  Sem r Wai.ResponseReceived
callInward component (RPC rpc) originDomain (CertHeader cert) wreq cont = do
  incomingCounterIncr originDomain
  -- only POST is supported
  when (Wai.requestMethod wreq /= HTTP.methodPost) $
    throw InvalidRoute
  -- No query parameters are allowed
  unless (BS.null . Wai.rawQueryString $ wreq) $
    throw InvalidRoute

  ensureCanFederateWith originDomain
  Log.debug $
    Log.msg ("Inward Request" :: ByteString)
      . Log.field "originDomain" (domainText originDomain)
      . Log.field "component" (show component)
      . Log.field "rpc" rpc

  validatedDomain <- validateDomain cert originDomain

  let path = LBS.toStrict (toLazyByteString (HTTP.encodePathSegments ["federation", rpc]))

  body <- embed $ Wai.lazyRequestBody wreq
  let headers = filter ((== versionHeader) . fst) (Wai.requestHeaders wreq)
  resp <- serviceCall component path headers body validatedDomain
  Log.debug $
    Log.msg ("Inward Request response" :: ByteString)
      . Log.field "status" (show (responseStatusCode resp))
  embed . cont $
    streamingResponseToWai
      resp
        { responseHeaders =
            Seq.filter
              (\(name, _) -> name == "Content-Type")
              (responseHeaders resp)
        }

serveInward :: Env -> Int -> IORef [IO ()] -> IO ()
serveInward env port cleanupsRef =
  serveServant @(ToServantApi API) env port cleanupsRef $ toServant $ server env._httpManager env._internalPort
