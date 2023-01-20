-- | Servant combinators related to Swagger docs
module Wire.API.SwaggerServant
  ( SwaggerTag,
    OmitDocs,
  )
where

import Control.Lens
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import qualified Data.HashSet.InsOrd as InsOrdSet
import Data.Metrics.Servant
import Data.Proxy
import qualified Data.Swagger as S
import Data.Text as T
import GHC.TypeLits
import Imports hiding (head)
import Servant
import Servant.Client.Core
import Servant.Swagger (HasSwagger (toSwagger))

-- | Add tag @tag@ to endpoints
--
-- @tag@ is a type level string. In swagger terms this combinator adds a
-- `Data.Swagger.Internal.TagName` to all `Data.Swagger.Internal.Operation`s
-- it's applied to. Tags are rendered as sections in swagger doc.
--
-- @
--        SwaggerTag "cannon"
--                :> "i"
--                :> ( Named
--                       "get-status"
--                       ( "status"
--                           :> MultiVerb
--                                'GET
--                                '[PlainText]
--                                '[RespondEmpty 200 "Service is alive."]
--                                ()
--                       )
-- @
data SwaggerTag tag

instance (HasSwagger b, KnownSymbol tag) => HasSwagger (SwaggerTag tag :> b) where
  toSwagger _ = prependTitle (T.pack (symbolVal (Proxy @tag))) $ toSwagger (Proxy :: Proxy b)
    where
      prependTitle :: Text -> S.Swagger -> S.Swagger
      prependTitle tagName s = s & (S.paths . InsOrdHashMap.unorderedTraversal) %~ prependTitle' tagName

      prependTitle' :: Text -> S.PathItem -> S.PathItem
      prependTitle' tagName it =
        it
          & (S.get . traverse . S.tags) <>~ tag tagName
          & (S.put . traverse . S.tags) <>~ tag tagName
          & (S.post . traverse . S.tags) <>~ tag tagName
          & (S.delete . traverse . S.tags) <>~ tag tagName
          & (S.options . traverse . S.tags) <>~ tag tagName
          & (S.patch . traverse . S.tags) <>~ tag tagName
          & (S.head_ . traverse . S.tags) <>~ tag tagName

      tag :: Text -> InsOrdSet.InsOrdHashSet S.TagName
      tag tagName = InsOrdSet.singleton @S.TagName tagName

instance HasServer api ctx => HasServer (SwaggerTag tag :> api) ctx where
  type ServerT (SwaggerTag tag :> api) m = ServerT api m

  route _ = route (Proxy :: Proxy api)
  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt s

instance RoutesToPaths api => RoutesToPaths (SwaggerTag tag :> api) where
  getRoutes = getRoutes @api

instance HasClient m api => HasClient m (SwaggerTag tag :> api) where
  type Client m (SwaggerTag tag :> api) = Client m api
  clientWithRoute proxyM _ = clientWithRoute proxyM (Proxy @api)
  hoistClientMonad proxyM _ = hoistClientMonad proxyM (Proxy @api)

-- | A type-level tag that lets us omit any branch from Swagger docs.
--
-- Those are likely to be:
--
--   * Endpoints for which we can't generate Swagger docs.
--   * The endpoint that serves Swagger docs.
--   * Internal endpoints.
data OmitDocs

instance HasSwagger (OmitDocs :> a) where
  toSwagger _ = mempty

instance HasServer api ctx => HasServer (OmitDocs :> api) ctx where
  type ServerT (OmitDocs :> api) m = ServerT api m

  route _ = route (Proxy :: Proxy api)
  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt s

instance RoutesToPaths api => RoutesToPaths (OmitDocs :> api) where
  getRoutes = getRoutes @api
