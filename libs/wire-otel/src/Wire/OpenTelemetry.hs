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

import Control.Monad.Codensity (Codensity (Codensity))
import Data.Text
import Network.HTTP.Client
import Network.HTTP2.Client qualified as HTTP2
import OpenTelemetry.Context (Context)
import OpenTelemetry.Context.ThreadLocal (getContext)
import OpenTelemetry.Instrumentation.HttpClient.Raw (HttpClientInstrumentationConfig, httpClientInstrumentationConfig, httpTracerProvider, instrumentRequest, instrumentResponse)
import OpenTelemetry.Trace
import UnliftIO

-- TODO(mangoiv): see https://github.com/iand675/hs-opentelemetry/blob/0b3c854a88113fc18df8561202a76357e593a294/instrumentation/http-client/src/OpenTelemetry/Instrumentation/HttpClient/Raw.hs#L60
instrumentHttp2Request :: HttpClientInstrumentationConfig -> Context -> HTTP2.Request -> IO HTTP2.Request
instrumentHttp2Request cfg ctx req = _

-- TODO(mangoiv): see https://github.com/iand675/hs-opentelemetry/blob/0b3c854a88113fc18df8561202a76357e593a294/instrumentation/http-client/src/OpenTelemetry/Instrumentation/HttpClient/Raw.hs#L136
instrumentHttp2Response :: HttpClientInstrumentationConfig -> Context -> HTTP2.Response -> IO ()
instrumentHttp2Response cfg ctx resp = _

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
