-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Data.Metrics.Middleware.Prometheus
  ( waiPrometheusMiddleware,
    waiPrometheusMiddlewarePaths,
    normalizeWaiRequestRoute,
  )
where

import Data.Metrics.Types (Paths, treeLookup)
import Data.Metrics.WaiRoute (treeToPaths)
import Data.Text.Encoding qualified as T
import Imports
import Network.Wai qualified as Wai
import Network.Wai.Middleware.Prometheus qualified as Promth
import Network.Wai.Routing.Route (Routes, prepare)

-- | Adds a prometheus metrics endpoint at @/i/metrics@
-- This middleware requires your servers 'Routes' because it does some normalization
-- (e.g. removing params from calls)
waiPrometheusMiddleware :: (Monad m) => Routes a m b -> Wai.Middleware
waiPrometheusMiddleware routes = waiPrometheusMiddlewarePaths $ treeToPaths $ prepare routes

-- | Helper function that should only be needed as long as we have wai-routing code left in
-- proxy: run 'treeToPaths' on old routing tables and 'routeToPaths' on the servant ones, and
-- feed both to this function.
waiPrometheusMiddlewarePaths :: Paths -> Wai.Middleware
waiPrometheusMiddlewarePaths paths =
  Promth.prometheus conf . instrument (normalizeWaiRequestRoute paths)
  where
    -- See Note [Raw Response]
    instrument = Promth.instrumentHandlerValueWithFilter Promth.ignoreRawResponses
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
