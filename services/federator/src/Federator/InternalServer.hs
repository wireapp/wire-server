{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

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

import Control.Monad.Codensity
import Data.Binary.Builder
import Data.ByteString qualified as BS
import Data.Domain
import Data.Metrics.Servant qualified as Metrics
import Data.Proxy
import Federator.Env
import Federator.Error.ServerError
import Federator.Health qualified as Health
import Federator.Metrics (Metrics, outgoingCounterIncr)
import Federator.RPC
import Federator.Remote
import Federator.Response
import Federator.Validation
import Imports
import Network.HTTP.Client (Manager)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Servant.API
import Servant.API.Extended.Endpath
import Servant.Server (Tagged (..))
import Servant.Server.Generic
import System.Logger.Class qualified as Log
import Wire.API.Federation.Component
import Wire.API.Routes.FederationDomainConfig
import Wire.Sem.Logger (Logger, debug)

data API mode = API
  { status ::
      mode
        :- "i"
          :> "status"
          -- When specified only returns status of the internal service,
          -- otherwise ensures that the external service is also up.
          :> QueryFlag "standalone"
          :> Get '[PlainText] NoContent,
    internalRequest ::
      mode
        :- "rpc"
          :> Capture "domain" Domain
          :> Capture "component" Component
          :> Capture "rpc" RPC
          :> Endpath
          -- We need to use 'Raw' so we can stream request body regardless of
          -- content-type and send a response with arbitrary content-type. Not
          -- sure if this is the right approach.
          :> Raw
  }
  deriving (Generic)

server ::
  ( Member Remote r,
    Member (Embed IO) r,
    Member (Error ValidationError) r,
    Member (Error ServerError) r,
    Member (Input FederationDomainConfigs) r,
    Member Metrics r,
    Member (Logger (Log.Msg -> Log.Msg)) r
  ) =>
  Manager ->
  Word16 ->
  (Sem r Wai.Response -> Codensity IO Wai.Response) ->
  API AsServer
server mgr extPort interpreter =
  API
    { status = Health.status mgr "external server" extPort,
      internalRequest = \remoteDomain component rpc ->
        Tagged $ \req respond -> runCodensity (interpreter (callOutward remoteDomain component rpc req)) respond
    }

callOutward ::
  ( Member Remote r,
    Member (Embed IO) r,
    Member (Error ValidationError) r,
    Member (Error ServerError) r,
    Member (Input FederationDomainConfigs) r,
    Member Metrics r,
    Member (Logger (Log.Msg -> Log.Msg)) r
  ) =>
  Domain ->
  Component ->
  RPC ->
  Wai.Request ->
  Sem r Wai.Response
callOutward targetDomain component (RPC path) req = do
  outgoingCounterIncr targetDomain
  -- only POST is supported
  when (Wai.requestMethod req /= HTTP.methodPost) $
    throw InvalidRoute
  -- No query parameters are allowed
  unless (BS.null . Wai.rawQueryString $ req) $
    throw InvalidRoute
  ensureCanFederateWith targetDomain
  body <- embed $ Wai.lazyRequestBody req
  debug $
    Log.msg (Log.val "Federator outward call")
      . Log.field "domain" targetDomain._domainText
      . Log.field "component" (show component)
      . Log.field "path" path
      . Log.field "body" body
  resp <-
    discoverAndCall
      targetDomain
      component
      path
      (Wai.requestHeaders req)
      (fromLazyByteString body)
  pure $ streamingResponseToWai resp

serveOutward :: Env -> Int -> IO ()
serveOutward env =
  serveServant
    (Metrics.servantPrometheusMiddleware $ Proxy @(ToServantApi API))
    (server env._httpManager env._externalPort $ runFederator env)
    env
