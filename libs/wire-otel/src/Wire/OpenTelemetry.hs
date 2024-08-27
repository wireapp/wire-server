module Wire.OpenTelemetry
  ( -- * instrumentation helpers
    withTracer,
    withTracerC,

    -- * inbound instrumentation

    -- ** wai instrumentation

    -- * outbound instrumentation
    withClientInstrumentation,
  )
where

import Control.Monad.Codensity (Codensity (Codensity))
import Data.Text
import Network.HTTP.Client
import OpenTelemetry.Context.ThreadLocal (getContext)
import OpenTelemetry.Instrumentation.HttpClient.Raw (httpClientInstrumentationConfig, httpTracerProvider, instrumentRequest, instrumentResponse)
import OpenTelemetry.Trace
import UnliftIO

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
