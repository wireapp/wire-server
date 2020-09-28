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

module Cannon.API.Public
  ( sitemap,
    apiDocs,
  )
where

import Cannon.App
import Cannon.Types
import Cannon.WS
import Data.Id (ClientId, ConnId, UserId)
import Data.Swagger.Build.Api hiding (Response)
import Imports
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.WebSockets
import Network.Wai.Predicate
import Network.Wai.Routing
import Network.Wai.Utilities
import Network.Wai.Utilities.Swagger
import qualified Network.WebSockets as Ws

sitemap :: Routes ApiBuilder Cannon ()
sitemap = do
  get "/await" (continue awaitH) $
    header "Z-User"
      .&. header "Z-Connection"
      .&. opt (query "client")
      .&. request
  document "GET" "await" $ do
    summary "Establish websocket connection"
    parameter Header "Upgrade" (string $ enum ["websocket"]) end
    parameter Header "Connection" (string $ enum ["upgrade"]) end
    parameter Header "Sec-WebSocket-Key" bytes' $
      description "16-bytes base64 encoded nonce"
    parameter Header "Sec-WebSocket-Version" (int32 $ enum [13]) end
    parameter Query "client" string' $ do
      optional
      description "Client ID"
    response 426 "Upgrade required" end

apiDocs :: Routes ApiBuilder Cannon ()
apiDocs = do
  get "/await/api-docs" (continue docsH) $
    accept "application" "json"
      .&. query "base_url"

docsH :: Media "application" "json" ::: Text -> Cannon Response
docsH (_ ::: url) = do
  let doc = mkSwaggerApi url [] sitemap
  return $ json doc

awaitH :: UserId ::: ConnId ::: Maybe ClientId ::: Request -> Cannon Response
awaitH (u ::: a ::: c ::: r) = do
  e <- wsenv
  case websocketsApp wsoptions (wsapp (mkKey u a) c e) r of
    Nothing -> return $ errorRs status426 "request-error" "websocket upgrade required"
    Just rs -> return rs -- ensure all middlewares ignore RawResponse - see Note [Raw Response]
  where
    status426 = mkStatus 426 "Upgrade Required"
    wsoptions = Ws.defaultConnectionOptions
