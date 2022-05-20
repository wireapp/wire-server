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

data LowLevelStream method status desc ctype

instance
  (ReflectMethod method, KnownNat status, Accept ctype) =>
  HasServer (LowLevelStream method status desc ctype) context
  where
  type ServerT (LowLevelStream method status desc ctype) m = m StreamingBody
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
          $ Route . responseStream status [contentHeader]
    where
      method = reflectMethod (Proxy :: Proxy method)
      status = statusFromNat (Proxy :: Proxy status)

instance
  (Accept ctype, KnownNat status, KnownSymbol desc, SwaggerMethod method) =>
  HasSwagger (LowLevelStream method status desc ctype)
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

instance RoutesToPaths (LowLevelStream method status desc ctype) where
  getRoutes = []
