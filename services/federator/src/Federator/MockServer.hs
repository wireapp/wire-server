{-# LANGUAGE RecordWildCards #-}

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

module Federator.MockServer
  ( MockException (..),
    withTempMockFederator,
    FederatedRequest (..),
  )
where

import qualified Control.Exception as Exception
import Control.Exception.Base (throw)
import Control.Monad.Catch hiding (fromException)
import Data.Domain (Domain)
import qualified Data.Text.Lazy as LText
import Federator.Error
import Federator.Error.ServerError
import Federator.InternalServer
import Federator.Response
import Federator.Validation
import Imports hiding (fromException)
import qualified Network.HTTP.Media as HTTP
import Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Utilities.Error as Wai
import Network.Wai.Utilities.MockServer
import Polysemy
import Polysemy.Error hiding (throw)
import Polysemy.TinyLog
import Wire.API.Federation.API (Component)
import Wire.API.Federation.Domain

-- | This can be thrown by actions passed to mock federator to simulate
-- failures either in federator itself, or in the services it calls.
data MockException = MockErrorResponse HTTP.Status LText
  deriving (Eq, Show, Typeable)

instance AsWai MockException where
  toWai (MockErrorResponse status message) = Wai.mkError status "mock-error" message
  waiErrorDescription (MockErrorResponse _ message) = LText.toStrict message

instance Exception MockException

data FederatedRequest = FederatedRequest
  { frOriginDomain :: Domain,
    frTargetDomain :: Domain,
    frComponent :: Component,
    frRPC :: Text,
    frBody :: LByteString
  }
  deriving (Eq, Show)

-- | Spawn a mock federator on a random port and run an action while it is running.
--
-- A mock federator is a web application that parses requests of the same form
-- as a regular outward service, but runs the provided action instead of
-- forwarding them to a remote federator.
withTempMockFederator ::
  (MonadIO m, MonadMask m) =>
  [HTTP.Header] ->
  (FederatedRequest -> IO (HTTP.MediaType, LByteString)) ->
  (Warp.Port -> m a) ->
  m (a, [FederatedRequest])
withTempMockFederator headers resp action = do
  remoteCalls <- newIORef []

  let handleException :: SomeException -> MockException
      handleException e = case Exception.fromException e of
        Just mockE -> mockE
        Nothing -> MockErrorResponse HTTP.status500 (LText.pack (displayException e))

  let app request respond = do
        response <-
          runM
            . discardLogs
            . runWaiErrors
              @'[ ValidationError,
                  ServerError,
                  MockException
                ]
            $ do
              RequestData {..} <- parseRequestData request
              domainText <- note NoOriginDomain $ lookup originDomainHeaderName rdHeaders
              originDomain <- parseDomain domainText
              targetDomain <- parseDomainText rdTargetDomain
              let fedRequest =
                    ( FederatedRequest
                        { frOriginDomain = originDomain,
                          frTargetDomain = targetDomain,
                          frComponent = rdComponent,
                          frRPC = rdRPC,
                          frBody = rdBody
                        }
                    )
              embed @IO $ modifyIORef remoteCalls (<> [fedRequest])
              (ct, body) <-
                fromException @MockException
                  . handle (throw . handleException)
                  $ resp fedRequest
              let headers' = ("Content-Type", HTTP.renderHeader ct) : headers
              pure $ Wai.responseLBS HTTP.status200 headers' body
        respond response
  result <-
    bracket
      (liftIO (startMockServer Nothing app))
      (liftIO . fst)
      (\(_close, port) -> action port)
  calls <- readIORef remoteCalls
  pure (result, calls)
