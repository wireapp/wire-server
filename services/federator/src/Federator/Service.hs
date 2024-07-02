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
{-# LANGUAGE TemplateHaskell #-}

module Federator.Service
  ( Service (..),
    ServiceStreaming,
    interpretServiceHTTP,
    serviceCall,
  )
where

-- FUTUREWORK(federation): Once we authenticate the call, we should send authentication data
-- to brig so brig can do some authorization as required.

import Bilge qualified as RPC
import Control.Exception
import Control.Lens (view)
import Control.Monad.Codensity
import Data.ByteString qualified as BS
import Data.Domain
import Data.Id
import Data.Sequence qualified as Seq
import Data.Text.Encoding qualified as Text
import Federator.Env
import Imports
import Network.HTTP.Client
import Network.HTTP.Types qualified as HTTP
import Network.HTTP.Types.Header
import Polysemy
import Polysemy.Input
import Servant.Client.Core qualified as Servant
import Servant.Types.SourceT
import Util.Options
import Wire.API.Federation.Component
import Wire.API.Federation.Domain (originDomainHeaderName)

type ServiceStreaming = Service (SourceT IO ByteString)

data Service body m a where
  -- | Returns status, headers and body, 'HTTP.Response' is not nice to work with in tests
  ServiceCall :: Component -> ByteString -> RequestHeaders -> LByteString -> Domain -> Service body m (Servant.ResponseF body)

makeSem ''Service

bodyReaderToStreamT :: (Monad m) => m ByteString -> SourceT m ByteString
bodyReaderToStreamT action = fromStepT go
  where
    go = Effect $ do
      chunk <- action
      pure $
        if BS.null chunk
          then Stop
          else Yield chunk go

-- FUTUREWORK(federation): Do we want to use servant client here? May make
-- everything typed and safe
--
-- FUTUREWORK: Avoid letting the IO errors escape into `Embed IO` and
-- return them as `Left`
--
-- FUTUREWORK: unify this interpretation with similar ones in Galley
--
interpretServiceHTTP ::
  ( Member (Embed (Codensity IO)) r,
    Member (Input Env) r,
    Member (Input RequestId) r
  ) =>
  Sem (ServiceStreaming ': r) a ->
  Sem r a
interpretServiceHTTP = interpret $ \case
  ServiceCall component rpcPath headers body domain -> do
    Endpoint serviceHost servicePort <- inputs (view service) <*> pure component
    manager <- inputs (view httpManager)
    RequestId rid <- input
    let req =
          defaultRequest
            { method = HTTP.methodPost,
              host = Text.encodeUtf8 serviceHost,
              port = fromIntegral servicePort,
              requestBody = RequestBodyLBS body,
              path = rpcPath,
              requestHeaders =
                [ ("Content-Type", "application/json"),
                  (originDomainHeaderName, Text.encodeUtf8 (domainText domain)),
                  (RPC.requestIdName, rid)
                ]
                  <> headers
            }

    embed $
      Codensity $ \k ->
        bracket (responseOpen req manager) responseClose $ \resp ->
          k $
            Servant.Response
              { Servant.responseStatusCode = responseStatus resp,
                Servant.responseHeaders = Seq.fromList (responseHeaders resp),
                Servant.responseHttpVersion = HTTP.http11,
                Servant.responseBody = bodyReaderToStreamT (responseBody resp)
              }
