{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -Wno-unused-imports #-}

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

module Federator.InternalServer where

import Control.Exception (bracketOnError)
import qualified Control.Exception as E
import Control.Lens (view)
import Data.Binary.Builder
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import Data.Default
import Data.Domain (domainText)
import Data.Either.Validation (Validation (..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.X509.CertificateStore
import Federator.App (runAppT)
import Federator.Discovery (DiscoverFederator, DiscoveryFailure (DiscoveryFailureDNSError, DiscoveryFailureSrvNotAvailable), runFederatorDiscovery)
import Federator.Env (Env, TLSSettings, applog, caStore, dnsResolver, runSettings, tls)
import Federator.Error.ServerError
import Federator.Options (RunSettings)
import Federator.Remote
import Federator.Response
import Federator.Validation
import Foreign (mallocBytes)
import Foreign.Marshal (free)
import Imports
import Network.HPACK (BufferSize)
import Network.HTTP.Client.Internal (openSocketConnection)
import Network.HTTP.Client.OpenSSL (withOpenSSL)
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP2.Client as HTTP2
import Network.Socket (Socket)
import qualified Network.Socket as NS
import Network.TLS
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Polysemy
import Polysemy.Error
import qualified Polysemy.Error as Polysemy
import Polysemy.IO (embedToMonadIO)
import Polysemy.Input
import qualified Polysemy.Input as Polysemy
import qualified Polysemy.Resource as Polysemy
import Polysemy.TinyLog (TinyLog)
import qualified Polysemy.TinyLog as Log
import Servant.Client.Core
import qualified System.TimeManager as T
import qualified System.X509 as TLS
import Wire.API.Federation.Component
import Wire.Network.DNS.Effect (DNSLookup)
import qualified Wire.Network.DNS.Effect as Lookup
import Wire.Network.DNS.SRV (SrvTarget (..))

data RequestData = RequestData
  { rdTargetDomain :: Text,
    rdComponent :: Component,
    rdRPC :: Text,
    rdHeaders :: [HTTP.Header],
    rdBody :: LByteString
  }

parseRequestData ::
  ( Member (Error ServerError) r,
    Member (Embed IO) r
  ) =>
  Wai.Request ->
  Sem r RequestData
parseRequestData req = do
  -- only POST is supported
  when (Wai.requestMethod req /= HTTP.methodPost) $
    throw InvalidRoute
  -- No query parameters are allowed
  unless (BS.null . Wai.rawQueryString $ req) $
    throw InvalidRoute
  -- check that the path has the expected form
  (domain, componentSeg, rpcPath) <- case Wai.pathInfo req of
    ["rpc", domain, comp, rpc] -> pure (domain, comp, rpc)
    _ -> throw InvalidRoute
  when (Text.null rpcPath) $
    throw InvalidRoute

  -- get component and body
  component <- note (UnknownComponent componentSeg) $ parseComponent componentSeg
  body <- embed $ Wai.lazyRequestBody req
  pure $
    RequestData
      { rdTargetDomain = domain,
        rdComponent = component,
        rdRPC = rpcPath,
        rdHeaders = Wai.requestHeaders req,
        rdBody = body
      }

callOutward ::
  ( Member Remote r,
    Member (Embed IO) r,
    Member (Error ValidationError) r,
    Member (Error ServerError) r,
    Member (Input RunSettings) r
  ) =>
  Wai.Request ->
  Sem r Wai.Response
callOutward req = do
  rd <- parseRequestData req
  domain <- parseDomainText (rdTargetDomain rd)
  ensureCanFederateWith domain
  resp <-
    discoverAndCall
      domain
      (rdComponent rd)
      (rdRPC rd)
      (rdHeaders rd)
      (fromLazyByteString (rdBody rd))
  pure $ streamingResponseToWai resp

serveOutward :: Env -> Int -> IO ()
serveOutward = serve callOutward
