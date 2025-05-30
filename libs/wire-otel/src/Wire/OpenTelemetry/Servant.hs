{-# LANGUAGE DerivingStrategies #-}

module Wire.OpenTelemetry.Servant where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ask)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.Foldable (toList)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.Encoding qualified as T
import Network.HTTP.Types
import OpenTelemetry.Context hiding (lookup)
import OpenTelemetry.Context.ThreadLocal
import OpenTelemetry.Instrumentation.HttpClient.Raw
import OpenTelemetry.Propagator
import OpenTelemetry.Trace
import OpenTelemetry.Trace.Core
import OpenTelemetry.Utils.Exceptions
import Servant.Client
import Servant.Client.Core
import Servant.Client.Internal.HttpClient

instrumentServantRequest :: Context -> Request -> ClientM Request
instrumentServantRequest ctx req = do
  env <- ask
  tracer <- httpTracerProvider
  forM_ (lookupSpan ctx) $ \s -> do
    let path = T.decodeUtf8 (BS.toStrict (toLazyByteString req.requestPath))
        queryString = T.decodeUtf8 (renderQuery True (toList req.requestQueryString))
        userAgent = maybe "" T.decodeUtf8 (lookup hUserAgent $ toList req.requestHeaders)
        url =
          Text.pack (showBaseUrl env.baseUrl)
            <> path
            <> queryString
        scheme = case env.baseUrl.baseUrlScheme of
          Https -> "https"
          Http -> "http"
        httpVersion = case req.requestHttpVersion of
          (HttpVersion major minor) -> T.pack (show major <> "." <> show minor)
    addAttributes
      s
      [ ("http.request.method", toAttribute $ T.decodeUtf8 req.requestMethod),
        ("url.full", toAttribute url),
        ("url.path", toAttribute path),
        ("url.query", toAttribute queryString),
        ("http.host", toAttribute $ T.pack env.baseUrl.baseUrlHost),
        ("url.scheme", toAttribute $ TextAttribute scheme),
        ("network.protocol.version", toAttribute httpVersion),
        ("user_agent.original", toAttribute userAgent)
      ]
  hdrs <- inject (getTracerProviderPropagators $ getTracerTracerProvider tracer) ctx $ toList req.requestHeaders
  pure req {requestHeaders = Seq.fromList hdrs}

instrumentServantResponse :: (MonadIO m) => Context -> Response -> m ()
instrumentServantResponse ctx0 resp = do
  tracer <- httpTracerProvider
  ctx <- extract (getTracerProviderPropagators $ getTracerTracerProvider tracer) (toList resp.responseHeaders) ctx0
  _ <- attachContext ctx
  forM_ (lookupSpan ctx) $ \s -> do
    when (statusCode resp.responseStatusCode >= 400) $ do
      setStatus s (Error "")
    addAttributes s [("http.response.statusCode", toAttribute $ statusCode $ resp.responseStatusCode)]

otelClientMiddleware :: Text.Text -> ClientMiddleware
otelClientMiddleware info app req0 = do
  tracer <- httpTracerProvider
  inSpanM tracer info defaultSpanArguments {kind = Client} $ do
    ctx <- getContext
    req <- instrumentServantRequest ctx req0
    resp <- app req
    instrumentServantResponse ctx resp
    pure resp
