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

module Federator.ExternalServer (callInward, serveInward, parseRequestData, RequestData (..)) where

import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Federator.Discovery
import Federator.Env
import Federator.Error.ServerError
import Federator.Options (RunSettings)
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
import qualified System.Logger.Message as Log
import Wire.API.Federation.Component
import Wire.API.Federation.Domain

-- FUTUREWORK(federation): Versioning of the federation API.
callInward ::
  Members
    '[ ServiceStreaming,
       Embed IO,
       TinyLog,
       DiscoverFederator,
       Error ValidationError,
       Error DiscoveryFailure,
       Error ServerError,
       Input RunSettings
     ]
    r =>
  Wai.Request ->
  Sem r Wai.Response
callInward wreq = do
  req <- parseRequestData wreq
  Log.debug $
    Log.msg ("Inward Request" :: ByteString)
      . Log.field "originDomain" (rdOriginDomain req)
      . Log.field "component" (show (rdComponent req))
      . Log.field "rpc" (rdRPC req)

  validatedDomain <- validateDomain (rdCertificate req) (rdOriginDomain req)

  let path = LBS.toStrict (toLazyByteString (HTTP.encodePathSegments ["federation", rdRPC req]))

  (status, body) <- serviceCall (rdComponent req) path (rdBody req) validatedDomain
  Log.debug $
    Log.msg ("Inward Request response" :: ByteString)
      . Log.field "status" (show status)

  let streamingBody output flush = go
        where
          go = do
            chunk <- body
            unless (BS.null chunk) $ do
              output (byteString chunk)
              flush
              go
  pure $ Wai.responseStream status defaultHeaders streamingBody

data RequestData = RequestData
  { rdComponent :: Component,
    rdRPC :: Text,
    rdBody :: LByteString,
    rdCertificate :: Maybe ByteString,
    rdOriginDomain :: ByteString
  }

-- path format: /federation/<component>/<rpc-path>
-- inward service removes <component> and forwards to component
-- where component = brig|galley|..
-- Headers:
--   Wire-Origin-Domain
--   X-SSL-Certificate
--
-- FUTUREWORK: use higher-level effects
parseRequestData ::
  Members '[Error ServerError, Embed IO] r =>
  Wai.Request ->
  Sem r RequestData
parseRequestData req = do
  -- only POST is supported
  when (Wai.requestMethod req /= HTTP.methodPost) $
    throw InvalidRoute
  -- No query parameters are allowed
  when (not . BS.null . Wai.rawQueryString $ req) $
    throw InvalidRoute
  -- check that the path has the expected form
  (componentSeg, rpcPath) <- case Wai.pathInfo req of
    ["federation", comp, rpc] -> pure (comp, rpc)
    _ -> throw InvalidRoute

  when (not (Text.all isAllowedRPCChar rpcPath)) $
    throw InvalidRoute

  when (Text.null rpcPath) $
    throw InvalidRoute

  -- get component, domain and body
  component <- note (UnknownComponent componentSeg) $ parseComponent componentSeg
  domain <-
    note NoOriginDomain $
      lookup originDomainHeaderName (Wai.requestHeaders req)
  body <- embed $ Wai.lazyRequestBody req
  pure $
    RequestData
      { rdComponent = component,
        rdRPC = rpcPath,
        rdBody = body,
        rdCertificate = lookupCertificate req,
        rdOriginDomain = domain
      }

isAllowedRPCChar :: Char -> Bool
isAllowedRPCChar c = isAsciiLower c || isAsciiUpper c || isNumber c || c == '_' || c == '-'

serveInward :: Env -> Int -> IO ()
serveInward = serve callInward

lookupCertificate :: Wai.Request -> Maybe ByteString
lookupCertificate req = HTTP.urlDecode True <$> lookup "X-SSL-Certificate" (Wai.requestHeaders req)
