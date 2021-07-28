{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

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

module Federator.ExternalServer where

import Control.Lens (view)
import Data.Aeson (decode)
import Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LBS
-- import Data.PEM as X509
import Data.String.Conversions (cs)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Federator.App (Federator, runAppT)
import Federator.Discovery
import Federator.Env (Env, applog, dnsResolver, runSettings)
import Federator.Options (RunSettings)
import Federator.Service (Service, interpretService, serviceCall)
import Federator.Utils.PolysemyServerError (absorbServerError)
import Federator.Validation
import Imports
import Mu.GRpc.Server (gRpcAppTrans, msgProtoBuf)
import Mu.Server (ServerError, ServerErrorIO, SingleServerT, singleService)
import qualified Mu.Server as Mu
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Wai
import qualified Network.Wai.Utilities.Error as Wai
import Polysemy
import qualified Polysemy.Error as Polysemy
import Polysemy.IO (embedToMonadIO)
import qualified Polysemy.Reader as Polysemy
import Polysemy.TinyLog (TinyLog)
import qualified Polysemy.TinyLog as Log
import qualified System.Logger.Message as Log
import Wire.API.Federation.GRPC.Types
import Wire.Network.DNS.Effect as Polysemy

-- FUTUREWORK(federation): Versioning of the federation API. See
-- https://higherkindness.io/mu-haskell/registry/ for some mu-haskell support
-- for versioning schemas here.

-- https://wearezeta.atlassian.net/wiki/spaces/CORE/pages/224166764/Limiting+access+to+federation+endpoints
--
-- FUTUREWORK(federation): implement server2server authentication!
-- (current validation only checks parsing and compares to allowList)
callLocal ::
  ( Members
      '[ Service,
         Embed IO,
         TinyLog,
         DiscoverFederator,
         Polysemy.Reader RunSettings
       ]
      r
  ) =>
  Maybe ByteString ->
  Request ->
  Sem r InwardResponse
callLocal cert = runInwardError . callLocal' cert
  where
    runInwardError :: Sem (Polysemy.Error InwardError ': r) ByteString -> Sem r InwardResponse
    runInwardError action = toResponse <$> Polysemy.runError action

    toResponse :: Either InwardError ByteString -> InwardResponse
    toResponse (Left err) = InwardResponseError err
    toResponse (Right bs) = InwardResponseBody bs

callLocal' ::
  ( Members
      '[ Service,
         Embed IO,
         TinyLog,
         DiscoverFederator,
         Polysemy.Reader RunSettings,
         Polysemy.Error InwardError
       ]
      r
  ) =>
  Maybe ByteString ->
  Request ->
  Sem r ByteString
callLocal' mcert req@Request {..} = do
  Log.debug $
    Log.msg ("Inward Request" :: ByteString)
      . Log.field "request" (show req)
      . maybe id (\cert -> Log.field "certificate" (Text.decodeUtf8 cert)) mcert

  validatedDomain <- validateDomain mcert originDomain
  validatedPath <- sanitizePath path
  Log.debug $
    Log.msg ("Path validation" :: ByteString)
      . Log.field "original path:" (show path)
      . Log.field "sanitized result:" (show validatedPath)
  (resStatus, resBody) <- serviceCall component validatedPath body validatedDomain
  Log.debug $
    Log.msg ("Inward Request response" :: ByteString)
      . Log.field "resStatus" (show resStatus)
  case HTTP.statusCode resStatus of
    200 -> pure $ maybe mempty LBS.toStrict resBody
    404 ->
      case Wai.label <$> (decode =<< resBody) of
        Just "no-endpoint" -> throwInward IInvalidEndpoint (cs $ "component " <> show component <> "does not have an endpoint " <> cs validatedPath)
        _ -> throwInward IOther (cs $ show resStatus)
    code -> do
      let description = "Invalid HTTP status from component: " <> Text.pack (show code) <> " " <> Text.decodeUtf8 (HTTP.statusMessage resStatus) <> " Response body: " <> maybe mempty cs resBody
      throwInward IOther description

routeToInternal ::
  (Members '[Service, Embed IO, Polysemy.Error ServerError, TinyLog, DiscoverFederator, Polysemy.Reader RunSettings] r) =>
  Maybe ByteString ->
  SingleServerT info Inward (Sem r) _
routeToInternal cert = singleService (Mu.method @"call" (callLocal cert))

lookupCertificate :: Wai.Request -> Maybe ByteString
lookupCertificate req = B8.map unescape <$> lookup "X-SSL-Certificate" (Wai.requestHeaders req)
  where
    unescape '\t' = '\n'
    unescape x = x

serveInward :: Env -> Int -> IO ()
serveInward env port = do
  let app req = gRpcAppTrans msgProtoBuf transformer (routeToInternal (lookupCertificate req)) req
  Wai.run port app
  where
    transformer ::
      Sem
        '[ DiscoverFederator,
           DNSLookup,
           TinyLog,
           Embed IO,
           Polysemy.Error ServerError,
           Service,
           Polysemy.Reader RunSettings,
           Embed Federator
         ]
        a ->
      ServerErrorIO a
    transformer action =
      runAppT env
        . runM @Federator
        . Polysemy.runReader (view runSettings env)
        . interpretService
        . absorbServerError
        . embedToMonadIO @Federator
        . Log.runTinyLog (view applog env)
        . Polysemy.runDNSLookupWithResolver (view dnsResolver env)
        . runFederatorDiscovery
        $ action
