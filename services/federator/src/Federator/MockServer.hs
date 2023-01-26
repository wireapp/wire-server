{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  ( -- * Federator mock server
    MockException (..),
    withTempMockFederator,
    FederatedRequest (..),

    -- * Mock utilities
    Mock,
    runMock,
    mockReply,
    mockFail,
    guardRPC,
    guardComponent,
    (~>),
    getRequest,
    getRequestRPC,
    getRequestBody,
  )
where

import qualified Control.Exception as Exception
import Control.Exception.Base (throw)
import Control.Monad.Catch hiding (fromException)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import qualified Data.Aeson as Aeson
import Data.Domain (Domain)
import qualified Data.Text as Text
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
import Wire.API.Federation.API (Component)
import Wire.API.Federation.Domain
import Wire.API.Federation.Version
import Wire.Sem.Logger.TinyLog

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
            . discardTinyLogs
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
              (ct, body) <-
                if rdRPC == "api-version"
                  then pure ("application/json", Aeson.encode versionInfo)
                  else do
                    embed @IO $ modifyIORef remoteCalls (<> [fedRequest])
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

--------------------------------------------------------------------------------
-- Mock creation utilities

-- | This is a monad that can be used to create mocked responses. It is a very
-- minimalistic web framework. One can expect a certain request (using
-- 'guardRPC') and reply accordingly (using 'mockReply'). Multiple possible
-- requests and responses can be combined using the 'Alternative' instance.  In
-- simple cases, one can also use the infix '(~>)' combinator, which concisely
-- binds a request to a hardcoded pure response.
newtype Mock a = Mock {unMock :: ReaderT FederatedRequest (MaybeT (ExceptT Text IO)) a}
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadIO)

-- | Convert a mocked response to a function which can be used as input to
-- 'tempMockFederator'.
runMock :: (Text -> IO a) -> Mock a -> FederatedRequest -> IO a
runMock err m req =
  runExceptT (runMaybeT (runReaderT (unMock m) req)) >>= \case
    Right Nothing -> err ("unmocked endpoint called: " <> frRPC req)
    Right (Just x) -> pure x
    Left e -> err e

-- | Retrieve the current request.
getRequest :: Mock FederatedRequest
getRequest = Mock $ ReaderT pure

-- | Retrieve the RPC of the current request.
getRequestRPC :: Mock Text
getRequestRPC = frRPC <$> getRequest

-- | Retrieve and deserialise the body of the current request.
getRequestBody :: Aeson.FromJSON a => Mock a
getRequestBody = do
  b <- frBody <$> getRequest
  case Aeson.eitherDecode b of
    Left e -> do
      rpc <- getRequestRPC
      mockFail ("Parse failure in " <> rpc <> ": " <> Text.pack e)
    Right x -> pure x

-- | Expect a given RPC. If the current request does not match, the whole
-- action fails. This can be used in combination with the 'Alternative'
-- instance to provide responses for multiple requests.
guardRPC :: Text -> Mock ()
guardRPC rpc = do
  rpc' <- getRequestRPC
  guard (rpc' == rpc)

guardComponent :: Component -> Mock ()
guardComponent c = do
  c' <- frComponent <$> getRequest
  guard (c == c')

-- | Serialise and return a response.
mockReply :: Aeson.ToJSON a => a -> Mock LByteString
mockReply = pure . Aeson.encode

-- | Abort the mock with an error.
mockFail :: Text -> Mock a
mockFail = Mock . lift . lift . throwE

infixl 5 ~>

-- | Expect a given RPC and simply return a pure response when the current
-- request matches.
(~>) :: Aeson.ToJSON a => Text -> a -> Mock LByteString
(~>) rpc x = guardRPC rpc *> mockReply x
