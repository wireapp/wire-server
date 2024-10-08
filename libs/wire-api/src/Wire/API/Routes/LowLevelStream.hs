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

module Wire.API.Routes.LowLevelStream where

import Control.Lens (at, (.~), (?~), _Just)
import Control.Monad.Codensity
import Control.Monad.Trans.Resource
import Data.ByteString.Char8 as B8
import Data.CaseInsensitive qualified as CI
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Metrics.Servant
import Data.OpenApi qualified as S
import Data.Proxy
import Data.Text qualified as Text
import GHC.TypeLits
import Imports
import Network.HTTP.Media qualified as HTTP
import Network.HTTP.Types
import Network.Wai
import Servant.API
import Servant.API.ContentTypes
import Servant.API.Status
import Servant.OpenApi as S
import Servant.OpenApi.Internal as S
import Servant.Server hiding (respond)
import Servant.Server.Internal
import Wire.API.Routes.Version

-- | Used as the return type of a streaming handler. The 'Codensity' wrapper
-- makes it possible to add finalisation logic to the streaming action.
type LowLevelStreamingBody = Codensity IO StreamingBody

-- FUTUREWORK: make it possible to generate headers at runtime
data LowLevelStream method status (headers :: [(Symbol, Symbol)]) desc ctype

class RenderHeaders (headers :: [(Symbol, Symbol)]) where
  renderHeaders :: [(HeaderName, ByteString)]

instance RenderHeaders '[] where
  renderHeaders = []

instance
  (KnownSymbol name, KnownSymbol value, RenderHeaders headers) =>
  RenderHeaders ('(name, value) ': headers)
  where
  renderHeaders = (name, value) : renderHeaders @headers
    where
      name :: HeaderName
      name = CI.mk (B8.pack (symbolVal (Proxy @name)))
      value :: ByteString
      value = B8.pack (symbolVal (Proxy @value))

instance
  (ReflectMethod method, KnownNat status, RenderHeaders headers, Accept ctype) =>
  HasServer (LowLevelStream method status headers desc ctype) context
  where
  type
    ServerT (LowLevelStream method status headers desc ctype) m =
      m LowLevelStreamingBody
  hoistServerWithContext _ _ nt s = nt s

  route Proxy _ action = leafRouter $ \env request respond ->
    let AcceptHeader accH = getAcceptHeader request
        cmediatype = HTTP.matchAccept [contentType (Proxy @ctype)] accH
        accCheck = when (isNothing cmediatype) $ delayedFail err406
        contentHeader = (hContentType, HTTP.renderHeader . maybeToList $ cmediatype)
     in runResourceT $ do
          r <-
            runDelayed
              ( action
                  `addMethodCheck` methodCheck method request
                  `addAcceptCheck` accCheck
              )
              env
              request
          liftIO $ case r of
            Route h ->
              runHandler h >>= \case
                Left e -> respond $ FailFatal e
                Right getStreamingBody -> lowerCodensity $ do
                  body <- getStreamingBody
                  let resp = responseStream status (contentHeader : extraHeaders) body
                  lift $ respond $ Route resp
            Fail e -> respond $ Fail e
            FailFatal e -> respond $ FailFatal e
    where
      method = reflectMethod (Proxy :: Proxy method)
      status = statusFromNat (Proxy :: Proxy status)
      extraHeaders = renderHeaders @headers

type instance
  SpecialiseToVersion v (LowLevelStream m s h d t) =
    LowLevelStream m s h d t

instance
  (S.ToSchema ctype, Accept ctype, KnownNat status, KnownSymbol desc, OpenApiMethod method) =>
  HasOpenApi (LowLevelStream method status headers desc ctype)
  where
  toOpenApi _ =
    mempty
      & S.paths
        . at "/"
        ?~ ( mempty
               & method
                 ?~ ( mempty
                        & S.responses . S.responses .~ fmap S.Inline responses
                    )
           )
    where
      method = S.openApiMethod (Proxy @method)
      responses =
        InsOrdHashMap.singleton
          (fromIntegral (natVal (Proxy @status)))
          $ mempty
            & S.description .~ Text.pack (symbolVal (Proxy @desc))
            & S.content
              .~ InsOrdHashMap.singleton
                (contentType $ Proxy @ctype)
                (mempty & S.schema . _Just . S._Inline .~ S.toSchema (Proxy @ctype))

instance RoutesToPaths (LowLevelStream method status headers desc ctype) where
  getRoutes = []
