{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

-- FUTUREWORK: Remove this module all together.
module Brig.Federation.Client where

import Brig.App (AppIO)
import Brig.Types (PrekeyBundle)
import Brig.Types.Client (PubClient)
import qualified Brig.Types.Search as Public
import Brig.Types.User
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Domain
import Data.Handle
import Data.Id (ClientId, UserId)
import Data.Qualified
import Data.Tagged
import qualified Data.Text as T
import Imports
import qualified System.Logger.Class as Log
import Wire.API.Federation.API.Brig as FederatedBrig
import Wire.API.Federation.Client (FederationError (..), executeFederated)
import Wire.API.Message (UserClients)
import Wire.API.User.Client (UserClientPrekeyMap)
import Wire.API.User.Client.Prekey (ClientPrekey)
import Wire.API.UserMap (UserMap)

type FederationAppIO = ExceptT FederationError AppIO

-- FUTUREWORK: Maybe find a way to tranform 'clientRoutes' into a client which
-- only uses 'FederationAppIO' monad, then boilerplate in this module can all be
-- deleted.
getUserHandleInfo :: Qualified Handle -> FederationAppIO (Maybe UserProfile)
getUserHandleInfo (Qualified handle domain) = do
  Log.info $ Log.msg $ T.pack "Brig-federation: handle lookup call on remote backend"
  executeFederated domain $ getUserByHandle clientRoutes handle

getUsersByIds :: Domain -> [UserId] -> FederationAppIO [UserProfile]
getUsersByIds domain uids = do
  Log.info $ Log.msg ("Brig-federation: get users by ids on remote backends" :: ByteString)
  executeFederated domain $ FederatedBrig.getUsersByIds clientRoutes uids

-- FUTUREWORK(federation): Abstract out all the rpc boilerplate and error handling
claimPrekey :: Qualified UserId -> ClientId -> FederationAppIO (Maybe ClientPrekey)
claimPrekey (Qualified user domain) client = do
  Log.info $ Log.msg @Text "Brig-federation: claiming remote prekey"
  executeFederated domain $ FederatedBrig.claimPrekey clientRoutes (user, client)

claimPrekeyBundle :: Qualified UserId -> FederationAppIO PrekeyBundle
claimPrekeyBundle (Qualified user domain) = do
  Log.info $ Log.msg @Text "Brig-federation: claiming remote prekey bundle"
  executeFederated domain $ FederatedBrig.claimPrekeyBundle clientRoutes user

claimMultiPrekeyBundle ::
  Domain ->
  UserClients ->
  FederationAppIO UserClientPrekeyMap
claimMultiPrekeyBundle domain uc = do
  Log.info $ Log.msg @Text "Brig-federation: claiming remote multi-user prekey bundle"
  executeFederated domain $ FederatedBrig.claimMultiPrekeyBundle clientRoutes uc

searchUsers :: Domain -> SearchRequest -> FederationAppIO [Public.Contact]
searchUsers domain searchTerm = do
  Log.warn $ Log.msg $ T.pack "Brig-federation: search call on remote backend"
  executeFederated domain $ FederatedBrig.searchUsers clientRoutes searchTerm

getUserClients :: Domain -> GetUserClients -> FederationAppIO (UserMap (Set PubClient))
getUserClients domain guc = do
  Log.info $ Log.msg @Text "Brig-federation: get users' clients from remote backend"
  executeFederated domain $ FederatedBrig.getUserClients clientRoutes guc

sendConnectionAction ::
  Local UserId ->
  Remote UserId ->
  RemoteConnectionAction ->
  FederationAppIO NewConnectionResponse
sendConnectionAction self (unTagged -> other) action = do
  let req = NewConnectionRequest (lUnqualified self) (qUnqualified other) action
  Log.info $ Log.msg @Text "Brig-federation: sending connection action to remote backend"
  executeFederated (qDomain other) $ FederatedBrig.sendConnectionAction clientRoutes (lDomain self) req
