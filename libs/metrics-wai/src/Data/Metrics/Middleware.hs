{-# LANGUAGE OverloadedStrings #-}

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
