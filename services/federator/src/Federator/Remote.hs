{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Federator.Remote
  ( Remote (..),
    RemoteError (..),
    interpretRemote,
    discoverAndCall,
  )
where

import Bilge.Request qualified as RPC
import Control.Exception qualified as E
import Control.Monad.Codensity
import Data.Binary.Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Domain
import Data.Id
import Data.Text.Encoding (decodeUtf8)
import Federator.Discovery
import Federator.Error
import HTTP2.Client.Manager (Http2Manager)
import HTTP2.Client.Manager qualified as H2Manager
import Imports
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Client qualified as HTTP2
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Servant.Client.Core
import Wire.API.Federation.Client
import Wire.API.Federation.Component
import Wire.API.Federation.Error
import Wire.Network.DNS.SRV

-- | An error that can occur as a result of making a request to a remote
-- federator.
data RemoteError
  = -- | This means that an error occurred while trying to make a request to a
    -- remote federator.
    RemoteError SrvTarget Text FederatorClientHTTP2Error
  | -- | This means that a request to a remote federator returned an error
    -- response. The error response could be due to an error in the remote
    -- federator itself, or in the services it proxied to.
    RemoteErrorResponse SrvTarget Text HTTP.Status LByteString
  deriving (Show)

instance AsWai RemoteError where
  toWai (RemoteError target msg err) =
    federationRemoteHTTP2Error target msg err
  toWai (RemoteErrorResponse target msg status body) =
    federationRemoteResponseError target msg status body

data Remote m a where
  DiscoverAndCall ::
    Domain ->
    Component ->
    Text ->
    [HTTP.Header] ->
    Builder ->
    Remote m StreamingResponse

makeSem ''Remote

interpretRemote ::
  ( Member (Embed (Codensity IO)) r,
    Member DiscoverFederator r,
    Member (Error DiscoveryFailure) r,
    Member (Error RemoteError) r,
    Member (Input Http2Manager) r,
    Member (Input RequestId) r
  ) =>
  Sem (Remote ': r) a ->
  Sem r a
interpretRemote = interpret $ \case
  DiscoverAndCall domain component rpc headers body -> do
    target@(SrvTarget hostname port) <- discoverFederatorWithError domain
    RequestId rid <- input
    let path =
          LBS.toStrict . toLazyByteString $
            HTTP.encodePathSegments ["federation", componentName component, rpc]
        pathT = decodeUtf8 path
        -- filter out Host header, because the HTTP2 client adds it back
        headers' =
          filter ((/= "Host") . fst) headers
            <> [(RPC.requestIdName, rid)]
        req' = HTTP2.requestBuilder HTTP.methodPost path headers' body

    mgr <- input
    resp <- mapError (RemoteError target pathT) . (fromEither @FederatorClientHTTP2Error =<<) . embed $
      Codensity $ \k ->
        E.catches
          (H2Manager.withHTTP2RequestOnSingleUseConn mgr (True, hostname, fromIntegral port) req' (consumeStreamingResponseWith $ k . Right))
          [ E.Handler $ k . Left,
            E.Handler $ k . Left . FederatorClientTLSException,
            E.Handler $ k . Left . FederatorClientHTTP2Exception,
            E.Handler $ k . Left . FederatorClientConnectionError
          ]

    unless (HTTP.statusIsSuccessful (responseStatusCode resp)) $ do
      bdy <- embed @(Codensity IO) . liftIO $ streamingResponseStrictBody resp
      throw $
        RemoteErrorResponse
          target
          pathT
          (responseStatusCode resp)
          (toLazyByteString bdy)
    pure resp
