{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Given a servant API type, this module gives you a 'Paths' for 'withPathTemplate'.
module Data.Metrics.Servant where

import Data.Metrics.Types
import qualified Data.Metrics.Types as Metrics
import Data.Proxy
import Data.String.Conversions
import Data.Tree
import GHC.TypeLits
import Imports
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Prometheus as Promth
import Servant.API

-- | This does not catch errors, so it must be called outside of 'WU.catchErrors'.
servantPrometheusMiddleware :: forall proxy api. (RoutesToPaths api) => proxy api -> Wai.Middleware
servantPrometheusMiddleware _ = Promth.prometheus conf . Promth.instrumentHandlerValue promthNormalize
  where
    conf =
      Promth.def
        { Promth.prometheusEndPoint = ["i", "metrics"],
          Promth.prometheusInstrumentApp = False
        }
    promthNormalize :: Wai.Request -> Text
    promthNormalize req = pathInfo
      where
        mPathInfo = Metrics.treeLookup (routesToPaths @api) $ cs <$> Wai.pathInfo req
        pathInfo = cs $ fromMaybe "N/A" mPathInfo

routesToPaths :: forall routes. RoutesToPaths routes => Paths
routesToPaths = Paths (meltTree (getRoutes @routes))

class RoutesToPaths routes where
  getRoutes :: Forest PathSegment

-- "seg" :> routes
instance
  {-# OVERLAPPING #-}
  ( KnownSymbol seg,
    RoutesToPaths segs
  ) =>
  RoutesToPaths (seg :> segs)
  where
  getRoutes = [Node (Right . cs $ symbolVal (Proxy @seg)) (getRoutes @segs)]

-- <capture> <:> routes
instance
  {-# OVERLAPPING #-}
  ( KnownSymbol capture,
    RoutesToPaths segs
  ) =>
  RoutesToPaths (Capture' mods capture a :> segs)
  where
  getRoutes = [Node (Left ":_") (getRoutes @segs)]

-- route <:> routes
instance
  {-# OVERLAPPING #-}
  ( RoutesToPaths route,
    RoutesToPaths routes
  ) =>
  RoutesToPaths (route :<|> routes)
  where
  getRoutes = getRoutes @route <> getRoutes @routes

-- stuff to ignore
instance {-# OVERLAPPING #-} RoutesToPaths (Verb 'HEAD status ctypes content) where
  getRoutes = []

instance {-# OVERLAPPING #-} RoutesToPaths (Verb 'GET status ctypes content) where
  getRoutes = []

instance {-# OVERLAPPING #-} RoutesToPaths (Verb 'POST status ctypes content) where
  getRoutes = []

instance {-# OVERLAPPING #-} RoutesToPaths (Verb 'PUT status ctypes content) where
  getRoutes = []

instance {-# OVERLAPPING #-} RoutesToPaths (Verb 'DELETE status ctypes content) where
  getRoutes = []

instance {-# OVERLAPPING #-} RoutesToPaths (Verb 'PATCH status ctypes content) where
  getRoutes = []

instance
  {-# OVERLAPPING #-}
  ( RoutesToPaths segs
  ) =>
  RoutesToPaths (ReqBody ctypes content :> segs)
  where
  getRoutes = getRoutes @segs

instance
  {-# OVERLAPPING #-}
  ( KnownSymbol sym,
    RoutesToPaths segs
  ) =>
  RoutesToPaths (Header sym content :> segs)
  where
  getRoutes = getRoutes @segs

instance
  {-# OVERLAPPING #-}
  ( KnownSymbol sym,
    RoutesToPaths segs
  ) =>
  RoutesToPaths (QueryParam sym content :> segs)
  where
  getRoutes = getRoutes @segs

instance
  {-# OVERLAPPABLE #-}
  ( RoutesToPaths segs
  ) =>
  RoutesToPaths (anything :> segs)
  where
  getRoutes = getRoutes @segs
