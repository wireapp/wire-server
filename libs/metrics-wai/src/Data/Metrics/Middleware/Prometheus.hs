module Data.Metrics.Middleware.Prometheus (waiPrometheusMiddleware) where

import Data.Maybe (fromMaybe)
import Data.Metrics.Types (Paths)
import Data.Metrics.Types (treeLookup)
import Data.Metrics.WaiRoute (treeToPaths)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Imports
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Prometheus as Promth
import Network.Wai.Routing.Route (Routes, prepare)

-- | Adds a prometheus metrics endpoint at @/i/metrics@
-- This middleware requires your servers 'Routes' because it does some normalization
-- (e.g. removing params from calls)
waiPrometheusMiddleware :: Monad m => Routes a m b -> Wai.Middleware
waiPrometheusMiddleware routes =
  Promth.prometheus conf . instrument (normalizeWaiRequestRoute paths)
  where
    -- See Note [Raw Response]
    instrument = Promth.instrumentHandlerValueWithFilter Promth.ignoreRawResponses
    paths = treeToPaths $ prepare routes
    conf =
      Promth.def
        { Promth.prometheusEndPoint = ["i", "metrics"],
          -- We provide our own instrumentation so we can normalize routes
          Promth.prometheusInstrumentApp = False
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
    pathInfo = T.decodeUtf8 $ fromMaybe "N/A" mPathInfo
