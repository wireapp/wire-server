-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

-- | Servant combinators related to Swagger docs
module Wire.API.SwaggerServant
  ( SwaggerTag,
    OmitDocs,
  )
where

import Control.Lens
import qualified Data.HashSet.InsOrd as InsOrdSet
import Data.Metrics.Servant
import Data.Proxy
import Data.Swagger (allOperations)
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
data SwaggerTag service port

-- | Adjust `Swagger` according to its `SwaggerTag`
--
-- As `SwaggerTags` are used for internal endpoints, use @port@ and @service@ to
-- render a `kubectl` command that will allow the user to access the internal
-- endpoint in the Kubernetes cluster.
instance (HasSwagger b, KnownSymbol service, KnownNat port) => HasSwagger (SwaggerTag service port :> b) where
  toSwagger _ = tagSwagger $ toSwagger (Proxy :: Proxy b)
    where
      serviceString :: String
      serviceString = symbolVal (Proxy @service)

      portInteger :: Integer
      portInteger = natVal (Proxy @port)

      portString :: String
      portString = show $ portInteger

      tagSwagger :: S.Swagger -> S.Swagger
      tagSwagger s =
        s
          & S.info . S.title .~ T.pack ("Wire-Server internal API (" ++ serviceString ++ ")")
          & S.info . S.description ?~ renderedDescription
          & S.host ?~ S.Host "localhost" ((Just . fromInteger) portInteger)
          & allOperations . S.tags <>~ tag

      tag :: InsOrdSet.InsOrdHashSet S.TagName
      tag = InsOrdSet.singleton @S.TagName (T.pack serviceString)

      renderedDescription :: Text
      renderedDescription =
        T.pack . Imports.unlines $
          [ "To have access to this *internal* endpoint, create a port forwarding to `"
              ++ serviceString
              ++ "` into the Kubernetes cluster. E.g.:",
            "```",
            "kubectl port-forward -n wire service/"
              ++ serviceString
              ++ " "
              ++ portString
              ++ ":8080",
            "```",
            "**N.B.:** Execution via this UI won't work due to CORS issues."
              ++ " But, the proposed `curl` commands will."
          ]

instance HasServer api ctx => HasServer (SwaggerTag service port :> api) ctx where
  type ServerT (SwaggerTag service port :> api) m = ServerT api m

  route _ = route (Proxy :: Proxy api)
  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt s

instance RoutesToPaths api => RoutesToPaths (SwaggerTag service port :> api) where
  getRoutes = getRoutes @api

instance HasClient m api => HasClient m (SwaggerTag service port :> api) where
  type Client m (SwaggerTag service port :> api) = Client m api
  clientWithRoute proxyM _ = clientWithRoute proxyM (Proxy @api)
  hoistClientMonad proxyM _ = hoistClientMonad proxyM (Proxy @api)

-- | A type-level tag that lets us omit any branch from Swagger docs.
--
-- FUTUREWORK(fisx): this is currently only used for the spar internal api
-- and spar scim, and we should probably eliminate those uses and this combinator.
-- it's only justification is laziness.
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
