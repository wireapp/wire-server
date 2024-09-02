module Wire.OpenTelemetry
  ( -- * instrumentation helpers
    withTracer,
    withTracerC,

    -- * inbound instrumentation

    -- ** wai instrumentation

    -- * outbound instrumentation

    -- ** http client
    withClientInstrumentation,

    -- ** http2 client
    instrumentHttp2Request,
    instrumentHttp2Response,
  )
where

import Control.Monad
import Control.Monad.Codensity (Codensity (Codensity))
import Data.ByteString.Char8 qualified as BS8
import Data.Foldable
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import GHC.IsList (fromList)
import HTTP2.Client.Manager
import Network.HPACK (foldedCase, original)
import Network.HPACK.Token
import Network.HTTP.Client
import Network.HTTP.Semantics
import Network.HTTP.Semantics.Client.Internal qualified as HTTP2
import Network.HTTP.Types
import Network.HTTP2.Client qualified as HTTP2
import OpenTelemetry.Context
import OpenTelemetry.Context.ThreadLocal
import OpenTelemetry.Instrumentation.HttpClient.Raw
import OpenTelemetry.Propagator
import OpenTelemetry.SemanticsConfig
import OpenTelemetry.Trace
import OpenTelemetry.Trace.Core
import UnliftIO

-- TODO(mangoiv): see https://github.com/iand675/hs-opentelemetry/blob/0b3c854a88113fc18df8561202a76357e593a294/instrumentation/http-client/src/OpenTelemetry/Instrumentation/HttpClient/Raw.hs#L60
instrumentHttp2Request :: HttpClientInstrumentationConfig -> Context -> Target -> HTTP2.Request -> IO HTTP2.Request
instrumentHttp2Request cfg ctx (tls, host, port) req = do
  tracer <- httpTracerProvider
  for_ (lookupSpan ctx) \s -> do
    let url = T.decodeUtf8 $ (if tls then "https://" else "http://") <> host <> ":" <> BS8.pack (show port) -- TODO(mangoiv): path + query params
    updateName s $ fromMaybe url $ requestName cfg
    let addStableAttributes = do
          addAttributes
            s
            [ -- TODO(mangoiv): there seems to be no way to get this information out of the http/2 request type without parsing it back
              -- ("http.request.method", toAttribute $ _ req),
              -- ("url.full", toAttribute url),
              -- ("url.path", toAttribute $ T.decodeUtf8 $ path req),
              -- ("url.query", toAttribute $ T.decodeUtf8 $ queryString req),
              -- ("url.scheme", toAttribute $ TextAttribute $ if secure req then "https" else "http"),
              -- ( "user_agent.original", toAttribute $ maybe "" T.decodeUtf8 (Prelude.lookup hUserAgent $ requestHeaders req)
              ("network.protocol.version", "2"), -- TODO(mangoiv): minor version?
              ("http.host", toAttribute $ T.decodeUtf8 $ host)
            ]
        -- TODO(mangoiv): the out obj has zero information we can use here
        -- addAttributes s
        --   $ fromList
        --   $ mapMaybe
        --     ( \h ->
        --         fmap
        --           (\fv -> ("http.response.header." <> T.decodeUtf8 (foldedCase h), toAttribute $ T.decodeUtf8 fv))
        --           $ Prelude.lookup (toToken $ original h)
        --           $ fst
        --           $ HTTP2Server.requestHeaders req
        --     )
        --   $ req
        addOldAttributes = do
          addAttributes s [("http.host", toAttribute $ T.decodeUtf8 $ host)]
    -- TODO(mangoiv): same as above; we don't seem to get any more information, perhaps we should add a span earlier that gives this some kind of context
    semanticsOptions <- liftIO getSemanticsOptions
    case httpOption semanticsOptions of
      Stable -> addStableAttributes
      StableAndOld -> do
        addStableAttributes
        addOldAttributes
      Old -> addOldAttributes

  hdrs <- inject (getTracerProviderPropagators $ getTracerTracerProvider tracer) ctx [] -- TODO(mangoiv): This sucks
  pure $ let HTTP2.Request outObj = req in HTTP2.Request $ outObj {outObjHeaders = hdrs <> outObj.outObjHeaders}

-- TODO(mangoiv): see https://github.com/iand675/hs-opentelemetry/blob/0b3c854a88113fc18df8561202a76357e593a294/instrumentation/http-client/src/OpenTelemetry/Instrumentation/HttpClient/Raw.hs#L136
instrumentHttp2Response :: HttpClientInstrumentationConfig -> Context -> HTTP2.Response -> IO ()
instrumentHttp2Response cfg ctx resp = do
  tracer <- httpTracerProvider
  ctx' <- extract (getTracerProviderPropagators $ getTracerTracerProvider tracer) (http2TableToHeaderList $ HTTP2.responseHeaders resp) ctx
  void $ attachContext ctx'
  for_ (lookupSpan ctx') \s -> do
    when (maybe True (>= status400) (HTTP2.responseStatus resp)) do
      setStatus s (Error "")
    let addResponseHeaderAttributes =
          addAttributes s
            $ fromList
            $ mapMaybe
              ( \h ->
                  fmap
                    (\fv -> ("http.response.header." <> T.decodeUtf8 (foldedCase h), toAttribute $ T.decodeUtf8 fv))
                    $ Prelude.lookup (toToken $ original h)
                    $ fst
                    $ HTTP2.responseHeaders resp
              )
            $ responseHeadersToRecord cfg
        addStableAttributes = do
          -- FUTUREWORK(mangoiv): we can instrument other things, like body size, body length uncompressed, net transport, server address, server port
          addAttributes s [("http.response.statusCode", maybe "" (toAttribute . statusCode) (HTTP2.responseStatus resp))]
          addResponseHeaderAttributes
        addOldAttributes = do
          addAttributes s [("http.status_code", maybe "" (toAttribute . statusCode) (HTTP2.responseStatus resp))]
          addResponseHeaderAttributes

    semanticsOptions <- liftIO getSemanticsOptions
    case httpOption semanticsOptions of
      Stable -> addStableAttributes
      StableAndOld -> do
        addStableAttributes
        addOldAttributes
      Old -> addOldAttributes
  where
    http2TableToHeaderList :: TokenHeaderTable -> ResponseHeaders
    http2TableToHeaderList (hdrNames, _hdrValues) = map (\(tok, fv) -> (tok.tokenKey, fv)) hdrNames

-- | a tracer for a service like brig, galley, etc.
withTracer :: (Tracer -> IO r) -> IO r
withTracer k =
  bracket
    initializeGlobalTracerProvider
    shutdownTracerProvider
    \tp -> k $ makeTracer tp "wire-otel" tracerOptions

-- | like 'withTracer' but in 'Codensity'
withTracerC :: Codensity IO Tracer
withTracerC = Codensity withTracer

-- | instrument a http client
withClientInstrumentation ::
  (MonadUnliftIO m) =>
  -- | name of the caller
  Text ->
  -- | continuation that takes a continuation that takes a request and a way to respond to a request
  ((Request -> (Request -> m (Response a)) -> m (Response a)) -> m b) ->
  m b
withClientInstrumentation info k = do
  tracer <- httpTracerProvider
  inSpan tracer info defaultSpanArguments {kind = Client} do
    otelCtx <- getContext
    k \req respond -> do
      resp <- respond =<< instrumentRequest httpClientInstrumentationConfig otelCtx req
      instrumentResponse httpClientInstrumentationConfig otelCtx resp
      pure resp
