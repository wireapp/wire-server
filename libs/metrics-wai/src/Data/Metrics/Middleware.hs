{-# LANGUAGE OverloadedStrings #-}

-- | FUTUREWORK: use package wai-middleware-prometheus instead and deprecate collectd?
module Data.Metrics.Middleware
    ( PathTemplate
    , Paths
    , withPathTemplate
    , duration
    , requestCounter
    , module Data.Metrics
    ) where

import Imports
import Data.Metrics
import Data.Metrics.Types
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Internal (Response (ResponseRaw))
import System.Clock

import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Network.Wai.Route.Tree as Tree

-- | *DEPRECATED*
-- These are the exact histogram bucket markers which the old *custom* metrics-core
-- library used. Some grafana graphs are still built around these exact number
-- e.g. see gally's POST duration graph:
--   https://staging-ie-grafana.zinfra.io/dashboard/db/galley
--
-- This is annoying and very fragile, prometheus has a better way of handling this, but
-- until we've converted all of the dashboards over to use prometheus rather than collect-d
-- we're stuck with these exact bucket counts.
--
-- Once we use prometheus metrics (e.g. there are no graphs in grafana which depend on metrics
-- prefixed with @collectd@) then you can delete this middleware entirely since the prometheus
-- middleware records request durations already.
requestDurationBuckets :: Buckets
requestDurationBuckets = [0, 30, 42, 60, 85, 120, 170, 240, 339, 480, 679, 960, 1358]

withPathTemplate :: Paths -> (PathTemplate -> Middleware) -> Middleware
withPathTemplate t f app r k = f (fromMaybe def tmp) app r k
  where
    def = PathTemplate "N/A"
    tmp = PathTemplate
        . T.decodeUtf8
      <$> treeLookup t (Tree.segments $ rawPathInfo r)

duration :: Metrics -> PathTemplate -> Middleware
duration m (PathTemplate t) f rq k = do
    st <- getTime Monotonic
    rs <- f rq k
    ed <- getTime Monotonic
    let p = mkPath [t, methodName rq, "time"]
    let timeElapsed = timeSpecAsMilliSecs $ ed `diffTimeSpec` st
    let requestDurationHisto = customHistogram p requestDurationBuckets
    histoSubmit timeElapsed requestDurationHisto m
    return rs

-- Count Requests and their status code.
--
-- [Note [Raw Response]]:
--
-- We ignore the status code of raw responses which are returned after
-- websocket communication ends because there is no meaningful status code
-- to ask for. WAI uses the fallback response status code (i.e. 500) which
-- is only used in servers which do not support raw responses (i.e. not
-- Warp).
requestCounter :: Metrics -> PathTemplate -> Middleware
requestCounter m (PathTemplate t) f rq k = f rq onResponse
  where
    onResponse rs@(ResponseRaw _ _) = do -- See Note [Raw Response]
        counterIncr (path "net.requests") m
        k rs
    onResponse rs = do
        counterIncr (path "net.requests") m
        counterIncr (mkPath [t, methodName rq, "status", code rs]) m
        k rs

mkPath :: [Text] -> Path
mkPath = path . mconcat . intersperse "." . ("net.resources":)
{-# INLINE mkPath #-}

code :: Response -> Text
code = T.pack . show . statusCode . responseStatus
{-# INLINE code #-}

methodName :: Request -> Text
methodName = T.decodeUtf8 . requestMethod
{-# INLINE methodName #-}

timeSpecAsMilliSecs :: TimeSpec -> Double
timeSpecAsMilliSecs t = fromIntegral (sec t * 1000 + nsec t `div` 1000000)
