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

import Control.Monad.Codensity
import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.ByteString.Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Domain
import Data.Id (RequestId (..))
import Data.Metrics.Servant qualified as Metrics
import Data.Proxy (Proxy (Proxy))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import Data.X509 qualified as X509
import Federator.Discovery
import Federator.Env
import Federator.Error.ServerError
import Federator.Health qualified as Health
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
import Servant.API
import Servant.API.Extended.Endpath
import Servant.Client.Core
import Servant.Server (Tagged (..))
import Servant.Server.Generic
import System.Logger (msg, val, (.=), (~~))
import System.Logger.Message qualified as Log
import Wire.API.Federation.Component
import Wire.API.Federation.Domain
import Wire.API.Routes.FederationDomainConfig
import Wire.Sem.Logger (info)

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
          :> Header "Wire-Origin-Request-Id" RequestId
          :> Header' '[Required, Strict] OriginDomainHeaderName Domain
          :> Header' '[Required, Strict] "X-SSL-Certificate" CertHeader
          :> Endpath
          -- We need to use 'Raw' so we can stream request body regardless of
          -- content-type and send a response with arbitrary content-type. Not
          -- sure if this is the right approach.
          :> Raw
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
    Member (Input FederationDomainConfigs) r,
    Member Metrics r
  ) =>
  Manager ->
  Word16 ->
  (Sem r Wai.Response -> Codensity IO Wai.Response) ->
  API AsServer
server mgr intPort interpreter =
  API
    { status = Health.status mgr "internal server" intPort,
      externalRequest = \component rpc mReqId remoteDomain remoteCert ->
        Tagged $ \req respond -> runCodensity (interpreter (callInward component rpc mReqId remoteDomain remoteCert req)) respond
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
  Maybe RequestId ->
  Domain ->
  CertHeader ->
  Wai.Request ->
  Sem r Wai.Response
callInward component (RPC rpc) mReqId originDomain (CertHeader cert) wreq = do
  rid <- case mReqId of
    Just r -> pure r
    Nothing -> do
      localRid <- liftIO $ RequestId . cs . UUID.toText <$> UUID.nextRandom
      info $
        "request-id" .= localRid
          ~~ "method" .= Wai.requestMethod wreq
          ~~ "path" .= Wai.rawPathInfo wreq
          ~~ msg (val "generated a new request id for local request")
      pure localRid
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
      . Log.field "request" rid

  validatedDomain <- validateDomain cert originDomain

  let path = LBS.toStrict (toLazyByteString (HTTP.encodePathSegments ["federation", rpc]))

  body <- embed $ Wai.lazyRequestBody wreq
  resp <- serviceCall component path body rid validatedDomain
  Log.debug $
    Log.msg ("Inward Request response" :: ByteString)
      . Log.field "status" (show (responseStatusCode resp))
      . Log.field "request" rid
  pure $
    streamingResponseToWai
      resp
        { responseHeaders =
            Seq.filter
              (\(name, _) -> name == "Content-Type")
              (responseHeaders resp)
        }

serveInward :: Env -> Int -> IO ()
serveInward env =
  serveServant
    (Metrics.servantPrometheusMiddleware $ Proxy @(ToServantApi API))
    (server env._httpManager env._internalPort $ runFederator env)
    env
