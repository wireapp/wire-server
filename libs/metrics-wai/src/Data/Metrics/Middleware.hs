{-# LANGUAGE OverloadedStrings #-}

-- | FUTUREWORK: use package wai-middleware-prometheus instead and deprecate collectd?
module Data.Metrics.Middleware
  ( PathTemplate,
    Paths,
    withPathTemplate,
    requestCounter,
    module Data.Metrics,
  )
where

import Data.Metrics
import Data.Metrics.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Imports
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Internal (Response (ResponseRaw))
import qualified Network.Wai.Route.Tree as Tree

withPathTemplate :: Paths -> (PathTemplate -> Middleware) -> Middleware
withPathTemplate t f app r k = f (fromMaybe def tmp) app r k
  where
    def = PathTemplate "N/A"
    tmp =
      PathTemplate
        . T.decodeUtf8
        <$> treeLookup t (Tree.segments $ rawPathInfo r)

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
    onResponse rs@(ResponseRaw _ _) = do
      -- See Note [Raw Response]
      counterIncr (path "net.requests") m
      k rs
    onResponse rs = do
      counterIncr (path "net.requests") m
      counterIncr (mkPath [t, methodName rq, "status", code rs]) m
      k rs

mkPath :: [Text] -> Path
mkPath = path . mconcat . intersperse "." . ("net.resources" :)
{-# INLINE mkPath #-}

code :: Response -> Text
code = T.pack . show . statusCode . responseStatus
{-# INLINE code #-}

methodName :: Request -> Text
methodName = T.decodeUtf8 . requestMethod
{-# INLINE methodName #-}
