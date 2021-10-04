{-# LANGUAGE RecordWildCards #-}

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

import qualified Brig.API.Client as API
import Brig.API.Connection.Remote (performRemoteAction)
import Brig.API.Error (clientError)
import Brig.API.Handler (Handler)
import qualified Brig.API.User as API
import Brig.App (qualifyLocal)
import qualified Brig.Data.Connection as Data
import Brig.Types (PrekeyBundle)
import Brig.User.API.Handle
import Data.Domain
import Data.Handle (Handle (..), parseHandle)
import Data.Id (ClientId, UserId)
import Data.Qualified
import Data.Tagged (Tagged (unTagged))
import Imports
import Network.Wai.Utilities.Error ((!>>))
import Servant (ServerT)
import Servant.API.Generic (ToServantApi)
import Servant.Server.Generic (genericServerT)
import Wire.API.Federation.API.Brig hiding (Api (..))
import qualified Wire.API.Federation.API.Brig as Federated
import qualified Wire.API.Federation.API.Brig as FederationAPIBrig
import Wire.API.Message (UserClients)
import Wire.API.Team.LegalHold (LegalholdProtectee (LegalholdPlusFederationNotImplemented))
import Wire.API.User (UserProfile)
import Wire.API.User.Client (PubClient, UserClientPrekeyMap)
import Wire.API.User.Client.Prekey (ClientPrekey)
import Wire.API.User.Search
import Wire.API.UserMap (UserMap)

federationSitemap :: ServerT (ToServantApi Federated.Api) Handler
federationSitemap =
  genericServerT $
    FederationAPIBrig.Api
      { Federated.getUserByHandle = getUserByHandle,
        Federated.getUsersByIds = getUsersByIds,
        Federated.claimPrekey = claimPrekey,
        Federated.claimPrekeyBundle = claimPrekeyBundle,
        Federated.claimMultiPrekeyBundle = claimMultiPrekeyBundle,
        Federated.searchUsers = searchUsers,
        Federated.getUserClients = getUserClients,
        Federated.sendConnectionAction = sendConnectionRequest
      }

sendConnectionRequest :: Domain -> NewConnectionRequest -> Handler NewConnectionResponse
sendConnectionRequest originDomain NewConnectionRequest {..} = do
  self <- qualifyLocal ncrTo
  let other = toRemote $ Qualified ncrFrom originDomain
  mconnection <- lift $ Data.lookupConnection self (unTagged other)
  maction <- lift $ performRemoteAction self other mconnection ncrAction
  pure $ NewConnectionResponse maction

getUserByHandle :: Handle -> Handler (Maybe UserProfile)
getUserByHandle handle = lift $ do
  maybeOwnerId <- API.lookupHandle handle
  case maybeOwnerId of
    Nothing ->
      pure Nothing
    Just ownerId ->
      listToMaybe <$> API.lookupLocalProfiles Nothing [ownerId]

getUsersByIds :: [UserId] -> Handler [UserProfile]
getUsersByIds uids =
  lift (API.lookupLocalProfiles Nothing uids)

claimPrekey :: (UserId, ClientId) -> Handler (Maybe ClientPrekey)
claimPrekey (user, client) = do
  API.claimLocalPrekey LegalholdPlusFederationNotImplemented user client !>> clientError

claimPrekeyBundle :: UserId -> Handler PrekeyBundle
claimPrekeyBundle user =
  API.claimLocalPrekeyBundle LegalholdPlusFederationNotImplemented user !>> clientError

claimMultiPrekeyBundle :: UserClients -> Handler UserClientPrekeyMap
claimMultiPrekeyBundle uc = API.claimLocalMultiPrekeyBundles LegalholdPlusFederationNotImplemented uc !>> clientError

-- | Searching for federated users on a remote backend should
-- only search by exact handle search, not in elasticsearch.
-- (This decision may change in the future)
searchUsers :: SearchRequest -> Handler [Contact]
searchUsers (SearchRequest searchTerm) = do
  let maybeHandle = parseHandle searchTerm
  maybeOwnerId <- maybe (pure Nothing) (lift . API.lookupHandle) maybeHandle
  case maybeOwnerId of
    Nothing -> pure []
    Just foundUser -> lift $ contactFromProfile <$$> API.lookupLocalProfiles Nothing [foundUser]

getUserClients :: GetUserClients -> Handler (UserMap (Set PubClient))
getUserClients (GetUserClients uids) = API.lookupLocalPubClientsBulk uids !>> clientError
