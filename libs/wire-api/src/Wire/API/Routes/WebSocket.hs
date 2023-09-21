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

module Wire.API.Routes.WebSocket where

import Control.Lens
import Control.Monad.Trans.Resource
import Data.HashMap.Strict.InsOrd
import Data.Metrics.Servant
import Data.OpenApi hiding (HasServer)
import Data.Proxy
import Imports
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Servant.OpenApi
import Servant.Server hiding (respond)
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.RouteResult
import Servant.Server.Internal.Router
import Wire.API.Routes.Version

-- | A websocket that relates to a 'PendingConnection'
-- Copied and adapted from: <https://hackage.haskell.org/package/servant-websockets-2.0.0/docs/Servant-API-WebSocket.html#t:WebSocketPending>
data WebSocketPending

instance HasServer WebSocketPending ctx where
  type ServerT WebSocketPending m = PendingConnection -> m ()

  hoistServerWithContext _ _ nat svr = nat . svr

  route Proxy _ app = leafRouter $ \env request respond ->
    runResourceT $
      runDelayed app env request >>= liftIO . go request respond
    where
      go request respond (Route app') =
        websocketsOr defaultConnectionOptions (runApp app') (backupApp respond) request (respond . Route)
      go _ respond (Fail e) = respond $ Fail e
      go _ respond (FailFatal e) = respond $ FailFatal e

      runApp a c = void (runHandler $ a c)

      backupApp respond _ _ =
        respond $
          FailFatal
            ServerError
              { errHTTPCode = 426,
                errReasonPhrase = "Upgrade Required",
                errBody = mempty,
                errHeaders = mempty
              }

type instance SpecialiseToVersion v WebSocketPending = WebSocketPending

instance HasOpenApi WebSocketPending where
  toOpenApi _ =
    mempty
      & paths
        . at "/"
        ?~ ( mempty
               & get
                 ?~ ( mempty
                        & responses . responses .~ resps
                        & externalDocs
                          ?~ ( mempty
                                 & description ?~ "RFC 6455"
                                 & url .~ URL "https://datatracker.ietf.org/doc/html/rfc6455"
                             )
                    )
           )
    where
      resps :: InsOrdHashMap HttpStatusCode (Referenced Data.OpenApi.Response)
      resps =
        mempty
          & at 101 ?~ Inline (mempty & description .~ "Connection upgraded.")
          & at 426 ?~ Inline (mempty & description .~ "Upgrade required.")

instance RoutesToPaths WebSocketPending where
  getRoutes = []
