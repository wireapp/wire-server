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

module Brig.API.Federation (federationSitemap) where

import Brig.API.Error (handleNotFound, throwStd)
import Brig.API.Handler (Handler)
import qualified Brig.API.User as API
import qualified Brig.Data.Client as Data
import Data.Handle (Handle)
import Data.Id (ClientId, UserId)
import Imports
import Servant (ServerT)
import Servant.API.Generic (ToServantApi)
import Servant.Server.Generic (genericServerT)
import qualified Wire.API.Federation.API.Brig as FederationAPIBrig
import Wire.API.User (UserProfile)
import Wire.API.User.Client.Prekey (ClientPrekey)

federationSitemap :: ServerT (ToServantApi FederationAPIBrig.Api) Handler
federationSitemap = genericServerT $ FederationAPIBrig.Api
  getUserByHandle getUsersByIds claimPrekey

getUserByHandle :: Handle -> Handler UserProfile
getUserByHandle handle = do
  maybeOwnerId <- lift $ API.lookupHandle handle
  case maybeOwnerId of
    Nothing -> throwStd handleNotFound
    Just ownerId -> do
      lift (API.lookupProfilesOfLocalUsers Nothing [ownerId]) >>= \case
        [] -> throwStd handleNotFound
        user : _ -> pure user

claimPrekey :: UserId -> ClientId -> Handler (Maybe ClientPrekey)
claimPrekey user client = lift (Data.claimPrekey user client)

getUsersByIds :: [UserId] -> Handler [UserProfile]
getUsersByIds uids =
  lift (API.lookupProfilesOfLocalUsers Nothing uids)

