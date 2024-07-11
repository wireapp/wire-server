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

module Cannon.API.Public
  ( publicAPIServer,
  )
where

import Cannon.App (wsapp)
import Cannon.Options
import Cannon.Types
import Cannon.WS
import Control.Lens
import Control.Monad.IO.Class
import Data.Id
import GHC.Base
import Network.WebSockets.Connection
import Servant
import Wire.API.Routes.Named
import Wire.API.Routes.Public.Cannon

publicAPIServer :: ServerT CannonAPI Cannon
publicAPIServer = Named @"await-notifications" streamData

streamData :: UserId -> ConnId -> Maybe ClientId -> PendingConnection -> Cannon ()
streamData userId connId clientId con = do
  opts <- options
  e <- wsenv
  liftIO $ wsapp (mkKey userId connId) userId clientId e (opts ^. rabbitmq) con
