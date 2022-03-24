{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

-- FUTUREWORK: Remove this module all together.
module Brig.Federation.Client where

import Brig.App
import Brig.Types (PrekeyBundle)
import Brig.Types.Client (PubClient)
import Brig.Types.User
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Except (ExceptT (..), throwE)
import Data.Domain
import Data.Handle
import Data.Id (ClientId, UserId)
import Data.Proxy
import Data.Qualified
import Data.Range (Range)
import qualified Data.Text as T
import GHC.TypeLits
import Imports
import Servant.Client hiding (client)
import Servant.Client.Core
import qualified System.Logger.Class as Log
import Wire.API.Federation.API
import Wire.API.Federation.API.Brig as FederatedBrig
import Wire.API.Federation.Client
import Wire.API.Federation.Error
import Wire.API.Message (UserClients)
import Wire.API.User.Client (UserClientPrekeyMap)
import Wire.API.User.Client.Prekey (ClientPrekey)
import Wire.API.UserMap (UserMap)

type FederationT m = ExceptT FederationError m

type FederationAppIO r = ExceptT FederationError (AppIO r)

getUserHandleInfo :: Remote Handle -> (FederationAppIO r) (Maybe UserProfile)
getUserHandleInfo (qUntagged -> Qualified handle domain) = do
  Log.info $ Log.msg $ T.pack "Brig-federation: handle lookup call on remote backend"
  executeFederated @"get-user-by-handle" domain handle

getUsersByIds :: Domain -> [UserId] -> (FederationAppIO r) [UserProfile]
getUsersByIds domain uids = do
  Log.info $ Log.msg ("Brig-federation: get users by ids on remote backends" :: ByteString)
  executeFederated @"get-users-by-ids" domain uids

claimPrekey ::
  (Monad m, MonadReader Env m, MonadIO m, Log.MonadLogger m) =>
  Qualified UserId ->
  ClientId ->
  FederationT m (Maybe ClientPrekey)
claimPrekey (Qualified user domain) client = do
  lift $ Log.info $ Log.msg @Text "Brig-federation: claiming remote prekey"
  executeFederated @"claim-prekey" domain (user, client)

claimPrekeyBundle :: Qualified UserId -> (FederationAppIO r) PrekeyBundle
claimPrekeyBundle (Qualified user domain) = do
  Log.info $ Log.msg @Text "Brig-federation: claiming remote prekey bundle"
  executeFederated @"claim-prekey-bundle" domain user

claimMultiPrekeyBundle ::
  Domain ->
  UserClients ->
  (FederationAppIO r) UserClientPrekeyMap
claimMultiPrekeyBundle domain uc = do
  Log.info $ Log.msg @Text "Brig-federation: claiming remote multi-user prekey bundle"
  executeFederated @"claim-multi-prekey-bundle" domain uc

searchUsers :: Domain -> SearchRequest -> (FederationAppIO r) SearchResponse
searchUsers domain searchTerm = do
  Log.info $ Log.msg $ T.pack "Brig-federation: search call on remote backend"
  executeFederated @"search-users" domain searchTerm

getUserClients :: Domain -> GetUserClients -> (FederationAppIO r) (UserMap (Set PubClient))
getUserClients domain guc = do
  Log.info $ Log.msg @Text "Brig-federation: get users' clients from remote backend"
  executeFederated @"get-user-clients" domain guc

sendConnectionAction ::
  Local UserId ->
  Remote UserId ->
  RemoteConnectionAction ->
  (FederationAppIO r) NewConnectionResponse
sendConnectionAction self (qUntagged -> other) action = do
  let req = NewConnectionRequest (tUnqualified self) (qUnqualified other) action
  Log.info $ Log.msg @Text "Brig-federation: sending connection action to remote backend"
  executeFederated @"send-connection-action" (qDomain other) req

notifyUserDeleted ::
  Local UserId ->
  Remote (Range 1 1000 [UserId]) ->
  (FederationAppIO r) ()
notifyUserDeleted self remotes = do
  let remoteConnections = tUnqualified remotes
  void $
    executeFederated @"on-user-deleted-connections" (tDomain remotes) $
      UserDeletedConnectionsNotification (tUnqualified self) remoteConnections

runBrigFederatorClient ::
  (MonadReader Env m, MonadIO m) =>
  Domain ->
  FederatorClient 'Brig a ->
  FederationT m a
runBrigFederatorClient targetDomain action = do
  ownDomain <- viewFederationDomain
  endpoint <- view federator >>= maybe (throwE FederationNotConfigured) pure
  let env =
        FederatorClientEnv
          { ceOriginDomain = ownDomain,
            ceTargetDomain = targetDomain,
            ceFederator = endpoint
          }
  liftIO (runFederatorClient env action)
    >>= either (throwE . FederationCallFailure) pure

executeFederated ::
  forall (name :: Symbol) api m.
  (MonadReader Env m, MonadIO m) =>
  ( HasFedEndpoint 'Brig api name,
    HasClient ClientM api,
    HasClient (FederatorClient 'Brig) api
  ) =>
  Domain ->
  Client (FederationT m) api
executeFederated domain =
  hoistClient (Proxy @api) (runBrigFederatorClient @m domain) $
    clientIn (Proxy @api) (Proxy @(FederatorClient 'Brig))
