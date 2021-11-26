{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}

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

module Wire.API.Federation.Client
  ( FederatorClientEnv (..),
    FederatorClient,
    runFederatorClient,
    performHTTP2Request,
  )
where

import qualified Control.Exception as E
import Control.Monad.Catch
import Control.Monad.Except
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import Data.ByteString.Conversion (toByteString')
import qualified Data.ByteString.Lazy as LBS
import Data.Domain
import qualified Data.Sequence as Seq
import Data.Streaming.Network
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.Lazy.Encoding as LText
import Foreign.Marshal.Alloc
import Imports
import qualified Network.HPACK as HTTP2
import qualified Network.HPACK.Token as HTTP2
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP2.Client as HTTP2
import qualified Network.Socket as NS
import Network.TLS as TLS
import qualified Network.Wai.Utilities.Error as Wai
import Servant.Client
import Servant.Client.Core
import qualified System.TimeManager
import Util.Options (Endpoint (..))
import Wire.API.Federation.Component
import Wire.API.Federation.Domain (originDomainHeaderName)
import Wire.API.Federation.Error

data FederatorClientEnv = FederatorClientEnv
  { ceOriginDomain :: Domain,
    ceTargetDomain :: Domain,
    ceFederator :: Endpoint
  }

newtype FederatorClient (c :: Component) a = FederatorClient
  {unFederatorClient :: ReaderT FederatorClientEnv (ExceptT FederatorClientError IO) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader FederatorClientEnv,
      MonadError FederatorClientError,
      MonadIO
    )

headersFromTable :: HTTP2.HeaderTable -> [HTTP.Header]
headersFromTable (headerList, _) = flip map headerList $ \(token, headerValue) ->
  (HTTP2.tokenKey token, headerValue)

connectSocket :: ByteString -> Int -> IO NS.Socket
connectSocket hostname port =
  handle (E.throw . FederatorClientConnectionError)
    . fmap fst
    $ getSocketFamilyTCP hostname port NS.AF_UNSPEC

performHTTP2Request ::
  Maybe TLS.ClientParams ->
  HTTP2.Request ->
  ByteString ->
  Int ->
  IO (Either FederatorClientHTTP2Error (HTTP.Status, HTTP2.HeaderTable, Builder))
performHTTP2Request mtlsConfig req hostname port = do
  let drainResponse resp = go mempty
        where
          go acc = do
            chunk <- HTTP2.getResponseBodyChunk resp
            if BS.null chunk
              then pure acc
              else go (acc <> byteString chunk)
  let clientConfig =
        HTTP2.ClientConfig
          "https"
          hostname
          {- cacheLimit: -} 20
  flip
    E.catches
    [ -- catch FederatorClientHTTP2Error (e.g. connection and TLS errors)
      E.Handler (pure . Left),
      -- catch HTTP2 exceptions
      E.Handler (pure . Left . FederatorClientHTTP2Exception)
    ]
    $ bracket (connectSocket hostname port) NS.close $ \sock -> do
      let withHTTP2Config k = case mtlsConfig of
            Nothing -> bracket (HTTP2.allocSimpleConfig sock 4096) HTTP2.freeSimpleConfig k
            -- FUTUREWORK(federation): Use openssl
            Just tlsConfig -> do
              ctx <- E.handle (E.throw . FederatorClientTLSException) $ do
                ctx <- TLS.contextNew sock tlsConfig
                TLS.handshake ctx
                pure ctx
              bracket (allocTLSConfig ctx 4096) freeTLSConfig k
      withHTTP2Config $ \conf ->
        HTTP2.run clientConfig conf $ \sendRequest -> do
          sendRequest req $ \resp -> do
            result <- drainResponse resp
            pure $ case HTTP2.responseStatus resp of
              Nothing -> Left FederatorClientNoStatusCode
              Just status -> Right (status, HTTP2.responseHeaders resp, result)

instance KnownComponent c => RunClient (FederatorClient c) where
  runRequestAcceptStatus expectedStatuses req = do
    env <- ask
    let baseUrlPath =
          HTTP.encodePathSegments
            [ "rpc",
              domainText (ceTargetDomain env),
              componentName (componentVal @c)
            ]
    let path = baseUrlPath <> requestPath req
    body <- case requestBody req of
      Just (RequestBodyLBS lbs, _) -> pure lbs
      Just (RequestBodyBS bs, _) -> pure (LBS.fromStrict bs)
      Just (RequestBodySource _, _) ->
        throwError FederatorClientStreamingNotSupported
      Nothing -> pure mempty
    let req' =
          HTTP2.requestBuilder
            (requestMethod req)
            (LBS.toStrict (toLazyByteString path))
            (toList (requestHeaders req) <> [(originDomainHeaderName, toByteString' (ceOriginDomain env))])
            (lazyByteString body)
    let Endpoint (Text.encodeUtf8 -> hostname) (fromIntegral -> port) = ceFederator env
    eresp <- liftIO $ performHTTP2Request Nothing req' hostname port
    case eresp of
      Left err -> throwError (FederatorClientHTTP2Error err)
      Right (status, htable, result)
        | maybe (HTTP.statusIsSuccessful status) (elem status) expectedStatuses ->
          pure $
            Response
              { responseStatusCode = status,
                responseHeaders = Seq.fromList (headersFromTable htable),
                responseHttpVersion = HTTP.http20,
                responseBody = toLazyByteString result
              }
        | otherwise ->
          throwError $
            FederatorClientError
              ( mkFailureResponse
                  status
                  (ceTargetDomain env)
                  (toLazyByteString (requestPath req))
                  (toLazyByteString result)
              )

  throwClientError = throwError . FederatorClientServantError

mkFailureResponse :: HTTP.Status -> Domain -> LByteString -> LByteString -> Wai.Error
mkFailureResponse status domain path body =
  (fromMaybe defaultError (Aeson.decode body))
    { Wai.errorData =
        Just
          Wai.FederationErrorData
            { Wai.federrDomain = domain,
              Wai.federrPath =
                "/federation"
                  <> Text.decodeUtf8With Text.lenientDecode (LBS.toStrict path)
            }
    }
  where
    defaultError =
      Wai.mkError
        status
        "unknown-federation-error"
        (LText.decodeUtf8With Text.lenientDecode body)

runFederatorClient ::
  KnownComponent c =>
  FederatorClientEnv ->
  FederatorClient c a ->
  IO (Either FederatorClientError a)
runFederatorClient env action = runExceptT (runReaderT (unFederatorClient action) env)

freeTLSConfig :: HTTP2.Config -> IO ()
freeTLSConfig cfg = free (HTTP2.confWriteBuffer cfg)

allocTLSConfig :: TLS.Context -> HTTP2.BufferSize -> IO HTTP2.Config
allocTLSConfig ctx bufsize = do
  buf <- mallocBytes bufsize
  timmgr <- System.TimeManager.initialize $ 30 * 1000000
  ref <- newIORef mempty
  let readData :: Int -> IO ByteString
      readData n = do
        chunk <- readIORef ref
        if BS.length chunk >= n
          then case BS.splitAt n chunk of
            (result, chunk') -> do
              writeIORef ref chunk'
              pure result
          else do
            chunk' <- TLS.recvData ctx
            if BS.null chunk'
              then pure chunk
              else do
                modifyIORef ref (<> chunk')
                readData n
  pure
    HTTP2.Config
      { HTTP2.confWriteBuffer = buf,
        HTTP2.confBufferSize = bufsize,
        HTTP2.confSendAll = TLS.sendData ctx . LBS.fromStrict,
        HTTP2.confReadN = readData,
        HTTP2.confPositionReadMaker = HTTP2.defaultPositionReadMaker,
        HTTP2.confTimeoutManager = timmgr
      }
