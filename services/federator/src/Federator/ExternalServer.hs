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
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Domain
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.X509 as X509
import Federator.Discovery
import Federator.Env
import Federator.Error.ServerError
import Federator.Response
import Federator.Service
import Federator.Validation
import Imports
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import qualified Polysemy.TinyLog as Log
import Servant.API
import Servant.API.Extended.Endpath
import Servant.Client.Core
import Servant.Server (Tagged (..))
import Servant.Server.Generic
import qualified System.Logger.Message as Log
import Wire.API.Federation.Component
import Wire.API.Federation.Domain
import Wire.API.Routes.FederationDomainConfig

newtype RPC = RPC Text

instance FromHttpApiData RPC where
  parseUrlPiece :: Text -> Either Text RPC
  parseUrlPiece rpcPath = do
    unless (Text.all isAllowedRPCChar rpcPath) $
      Left "invalid-endpoint"

    when (Text.null rpcPath) $
      Left "invalid-endpoint"
    pure $ RPC rpcPath

isAllowedRPCChar :: Char -> Bool
isAllowedRPCChar c = isAsciiLower c || isAsciiUpper c || isNumber c || c == '_' || c == '-'

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
          :> Get '[PlainText] NoContent,
    externalRequest ::
      mode
        :- "federation"
          :> Capture "component" Component
          :> Capture "rpc" RPC
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
    Member (Input FederationDomainConfigs) r
  ) =>
  (Sem r Wai.Response -> Codensity IO Wai.Response) ->
  API AsServer
server interpreter =
  API
    { status = pure NoContent,
      externalRequest = \component rpc remoteDomain remoteCert ->
        Tagged $ \req respond -> runCodensity (interpreter (callInward component rpc remoteDomain remoteCert req)) respond
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
    Member (Input FederationDomainConfigs) r
  ) =>
  Component ->
  RPC ->
  Domain ->
  CertHeader ->
  Wai.Request ->
  Sem r Wai.Response
callInward component (RPC rpc) originDomain (CertHeader cert) wreq = do
  -- only POST is supported
  when (Wai.requestMethod wreq /= HTTP.methodPost) $
    throw InvalidRoute
  -- No query parameters are allowed
  unless (BS.null . Wai.rawQueryString $ wreq) $
    throw InvalidRoute

  Log.debug $
    Log.msg ("Inward Request" :: ByteString)
      . Log.field "originDomain" (domainText originDomain)
      . Log.field "component" (show component)
      . Log.field "rpc" rpc

  validatedDomain <- validateDomain cert originDomain

  let path = LBS.toStrict (toLazyByteString (HTTP.encodePathSegments ["federation", rpc]))

  body <- embed $ Wai.lazyRequestBody wreq
  resp <- serviceCall component path body validatedDomain
  Log.debug $
    Log.msg ("Inward Request response" :: ByteString)
      . Log.field "status" (show (responseStatusCode resp))
  pure $
    streamingResponseToWai
      resp
        { responseHeaders =
            Seq.filter
              (\(name, _) -> name == "Content-Type")
              (responseHeaders resp)
        }

serveInward :: Env -> Int -> IO ()
serveInward env = serveServant (server $ runFederator env) env
