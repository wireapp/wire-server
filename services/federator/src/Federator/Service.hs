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

module Federator.Service where

-- FUTUREWORK(federation): Once we authenticate the call, we should send authentication data
-- to brig so brig can do some authorization as required.

import qualified Bilge as RPC
import Control.Lens (view)
import Data.Domain
import Data.String.Conversions (cs)
import qualified Data.Text.Encoding as Text
import Federator.Env
import Imports
import Network.HTTP.Client
import qualified Network.HTTP.Types as HTTP
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import Util.Options
import Wire.API.Federation.Component
import Wire.API.Federation.Domain (originDomainHeaderName)

newtype ServiceError = ServiceErrorInvalidStatus HTTP.Status
  deriving (Eq, Show)

type ServiceLBS = Service (Maybe LByteString)

type ServiceStreaming = Service BodyReader

data Service body m a where
  -- | Returns status, headers and body, 'HTTP.Response' is not nice to work with in tests
  ServiceCall :: Component -> ByteString -> LByteString -> Domain -> Service body m (HTTP.Status, [HTTP.Header], body)

makeSem ''Service

-- FUTUREWORK(federation): Do we want to use servant client here? May make
-- everything typed and safe
--
-- FUTUREWORK: Avoid letting the IO errors escape into `Embed IO` and
-- return them as `Left`
--
-- FUTUREWORK: unify this interpretation with similar ones in Galley
--
interpretServiceStreaming ::
  Members '[Embed IO, Input Env, TinyLog] r =>
  Sem (ServiceStreaming ': r) a ->
  Sem r a
interpretServiceStreaming = interpret $ \case
  ServiceCall component rpcPath body domain -> do
    Endpoint serviceHost servicePort <- inputs (view service) <*> pure component
    manager <- inputs (view httpManager)
    reqId <- inputs (view requestId)
    let req =
          defaultRequest
            { method = HTTP.methodPost,
              host = Text.encodeUtf8 serviceHost,
              port = fromIntegral servicePort,
              requestBody = RequestBodyLBS body,
              path = rpcPath,
              requestHeaders =
                [ ("Content-Type", "application/json"),
                  (originDomainHeaderName, cs (domainText domain)),
                  (RPC.requestIdName, RPC.unRequestId reqId)
                ]
            }

    resp <- embed $ responseOpen req manager

    -- TODO: close response
    pure (responseStatus resp, responseHeaders resp, responseBody resp)
