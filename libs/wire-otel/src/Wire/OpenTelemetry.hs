-- FUTUREWORK(mangoiv):
-- instrument http/2 request similarly to how it was done for http-client here:
-- https://github.com/iand675/hs-opentelemetry/blob/0b3c854a88113fc18df8561202a76357e593a294/instrumentation/http-client/src/OpenTelemetry/Instrumentation/HttpClient/Raw.hs#L60
-- This is non-trivial because http/2 forgets the structure on the out objs.
module Wire.OpenTelemetry
  ( -- * instrumentation helpers
    withTracer,
    withTracerC,

    -- * outbound instrumentation

    -- ** http client
    withClientInstrumentation,
  )
where

import Control.Monad.Codensity (Codensity (Codensity))
import Data.Text (Text)
import Network.HTTP.Client (Request, Response)
import OpenTelemetry.Context.ThreadLocal (getContext)
import OpenTelemetry.Instrumentation.HttpClient.Raw
import OpenTelemetry.Trace
import UnliftIO (MonadUnliftIO, bracket, liftIO)

-- | a tracer for a service like brig, galley, etc.
withTracer :: (MonadUnliftIO m) => (Tracer -> m r) -> m r
withTracer k =
  bracket
    (liftIO initializeGlobalTracerProvider)
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
