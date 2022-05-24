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

import Control.Lens (at, (.~), (?~))
import Data.ByteString.Char8 as B8
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Metrics.Servant
import Data.Proxy
import qualified Data.Swagger as S
import qualified Data.Text as Text
import GHC.TypeLits
import Imports
import qualified Network.HTTP.Media as HTTP
import Network.HTTP.Types
import Network.Wai
import Servant.API
import Servant.API.ContentTypes
import Servant.API.Status
import Servant.Server hiding (respond)
import Servant.Server.Internal
import Servant.Swagger as S
import Servant.Swagger.Internal as S

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
  type ServerT (LowLevelStream method status headers desc ctype) m = m StreamingBody
  hoistServerWithContext _ _ nt s = nt s

  route Proxy _ action = leafRouter $ \env request respond ->
    let AcceptHeader accH = getAcceptHeader request
        cmediatype = HTTP.matchAccept [contentType (Proxy @ctype)] accH
        accCheck = when (isNothing cmediatype) $ delayedFail err406
        contentHeader = (hContentType, HTTP.renderHeader . maybeToList $ cmediatype)
     in runAction
          ( action `addMethodCheck` methodCheck method request
              `addAcceptCheck` accCheck
          )
          env
          request
          respond
          $ Route . responseStream status (contentHeader : extraHeaders)
    where
      method = reflectMethod (Proxy :: Proxy method)
      status = statusFromNat (Proxy :: Proxy status)
      extraHeaders = renderHeaders @headers

instance
  (Accept ctype, KnownNat status, KnownSymbol desc, SwaggerMethod method) =>
  HasSwagger (LowLevelStream method status headers desc ctype)
  where
  toSwagger _ =
    mempty
      & S.paths
        . at "/"
        ?~ ( mempty
               & method
                 ?~ ( mempty
                        & S.produces ?~ S.MimeList [contentType (Proxy @ctype)]
                        & S.responses . S.responses .~ fmap S.Inline responses
                    )
           )
    where
      method = S.swaggerMethod (Proxy @method)
      responses =
        InsOrdHashMap.singleton
          (fromIntegral (natVal (Proxy @status)))
          $ mempty
            & S.description .~ Text.pack (symbolVal (Proxy @desc))

instance RoutesToPaths (LowLevelStream method status headers desc ctype) where
  getRoutes = []
