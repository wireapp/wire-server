module Data.Metrics.Middleware.Prometheus (waiPrometheusMiddleware) where

import           Imports
import qualified Network.Wai                       as Wai
import           Network.Wai.Internal
import           Network.Wai.Routing.Route         (Routes, prepare)
import qualified Network.Wai.Middleware.Prometheus as Promth
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import qualified Network.HTTP.Types as HTTP
import           System.Clock

import Data.Metrics.WaiRoute (treeToPaths)
import Data.Metrics.Types (Paths)
import Data.Metrics.Types (treeLookup)


-- | Adds a prometheus metrics endpoint at @/i/metrics@
-- This middleware requires your servers 'Routes' because it does some normalization
-- (e.g. removing params from calls)
waiPrometheusMiddleware :: Monad m => Routes a m b -> Wai.Middleware
waiPrometheusMiddleware routes =
    Promth.prometheus conf . instrumentHandlerValue (normalizeWaiRequestRoute paths)
  where
    paths = treeToPaths $ prepare routes
    conf = Promth.def
        { Promth.prometheusEndPoint      = ["i", "metrics"]
          -- We provide our own instrumentation so we can normalize routes
        , Promth.prometheusInstrumentApp = False
        }

-- | Compute a normalized route for a given request.
-- Normalized routes have route parameters replaced with their identifier
-- e.g. @/user/1234@ might become @/user/userid@
normalizeWaiRequestRoute :: Paths -> Wai.Request -> Text
normalizeWaiRequestRoute paths req = pathInfo
  where
    mPathInfo :: Maybe ByteString
    mPathInfo = treeLookup paths (T.encodeUtf8 <$> Wai.pathInfo req)

    -- Use the normalized path info if available; otherwise dump the raw path info for
    -- debugging purposes
    pathInfo :: Text
    pathInfo  = T.decodeUtf8 $ fromMaybe "N/A" mPathInfo

-- | This can be refactored away once https://github.com/fimad/prometheus-haskell/pull/45 has
-- been released on hackage.
instrumentHandlerValue ::
     (Wai.Request -> Text) -- ^ The function used to derive the "handler" value in Prometheus
  -> Wai.Application -- ^ The app to instrument
  -> Wai.Application -- ^ The instrumented app
instrumentHandlerValue f app req respond = do
  start <- getTime Monotonic
  app req $ \case
    res@(ResponseRaw {}) -> respond res  -- See Note [Raw Response]
    res -> do
      end <- getTime Monotonic
      let method = Just $ decodeUtf8 (Wai.requestMethod req)
      let status = Just $ T.pack (show (HTTP.statusCode (Wai.responseStatus res)))
      Promth.observeSeconds (f req) method status start end
      respond res
