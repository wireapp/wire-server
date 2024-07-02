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

import Data.Binary.Builder
import Data.ByteString qualified as BS
import Data.Domain
import Federator.Env
import Federator.Error.ServerError
import Federator.Health qualified as Health
import Federator.Interpreter
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
import Polysemy.TinyLog
import Servant qualified
import Servant.API
import Servant.API.Extended.Endpath
import Servant.API.Extended.RawM qualified as RawM
import Servant.Server.Generic
import System.Logger.Class qualified as Log
import Wire.API.Federation.Component
import Wire.API.Routes.FederationDomainConfig

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
          -- We need to use 'RawM' so we can stream request body regardless of
          -- content-type and send a response with arbitrary content-type.
          :> RawM.RawM
  }
  deriving (Generic)

server ::
  ( Member Remote r,
    Member (Embed IO) r,
    Member (Error ValidationError) r,
    Member (Error ServerError) r,
    Member (Input FederationDomainConfigs) r,
    Member Metrics r,
    Member (Logger (Log.Msg -> Log.Msg)) r,
    Member (Error Servant.ServerError) r
  ) =>
  Manager ->
  Word16 ->
  API (AsServerT (Sem r))
server mgr extPort =
  API
    { status = Health.status mgr "external server" extPort,
      internalRequest = callOutward
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
  (Wai.Response -> IO Wai.ResponseReceived) ->
  Sem r Wai.ResponseReceived
callOutward targetDomain component (RPC path) req cont = do
  -- only POST is supported
  when (Wai.requestMethod req /= HTTP.methodPost) $
    throw InvalidRoute
  -- No query parameters are allowed
  unless (BS.null . Wai.rawQueryString $ req) $
    throw InvalidRoute
  ensureCanFederateWith targetDomain
  outgoingCounterIncr targetDomain
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
  embed . cont $ streamingResponseToWai resp

serveOutward :: Env -> Int -> IO ()
serveOutward env port = do
  serveServant @(ToServantApi API) env port (toServant $ server env._httpManager env._internalPort)
