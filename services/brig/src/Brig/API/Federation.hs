{-# LANGUAGE RecordWildCards #-}

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

module Brig.API.Federation (federationSitemap, FederationAPI) where

import qualified Brig.API.Client as API
import Brig.API.Connection.Remote (performRemoteAction)
import Brig.API.Error (clientError)
import Brig.API.Handler (Handler)
import qualified Brig.API.User as API
import Brig.API.Util (lookupSearchPolicy)
import Brig.App (qualifyLocal, wrapClient)
import qualified Brig.Data.Connection as Data
import qualified Brig.Data.User as Data
import Brig.IO.Intra (notify)
import Brig.Types (PrekeyBundle, Relation (Accepted))
import Brig.Types.User.Event
import Brig.User.API.Handle
import qualified Brig.User.Search.SearchIndex as Q
import Data.Domain
import Data.Handle (Handle (..), parseHandle)
import Data.Id (ClientId, UserId)
import Data.List.NonEmpty (nonEmpty)
import Data.List1
import Data.Qualified
import Data.Range
import qualified Gundeck.Types.Push as Push
import Imports
import Network.Wai.Utilities.Error ((!>>))
import Servant (ServerT)
import Servant.API
import UnliftIO.Async (pooledForConcurrentlyN_)
import Wire.API.Federation.API.Brig
import Wire.API.Federation.API.Common
import Wire.API.Message (UserClients)
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.Routes.Named
import Wire.API.Team.LegalHold (LegalholdProtectee (LegalholdPlusFederationNotImplemented))
import Wire.API.User (UserProfile)
import Wire.API.User.Client (PubClient, UserClientPrekeyMap)
import Wire.API.User.Client.Prekey (ClientPrekey)
import Wire.API.User.Search
import Wire.API.UserMap (UserMap)

type FederationAPI = "federation" :> BrigApi

federationSitemap :: ServerT FederationAPI (Handler r)
federationSitemap =
  Named @"get-user-by-handle" getUserByHandle
    :<|> Named @"get-users-by-ids" getUsersByIds
    :<|> Named @"claim-prekey" claimPrekey
    :<|> Named @"claim-prekey-bundle" claimPrekeyBundle
    :<|> Named @"claim-multi-prekey-bundle" claimMultiPrekeyBundle
    :<|> Named @"search-users" searchUsers
    :<|> Named @"get-user-clients" getUserClients
    :<|> Named @"send-connection-action" sendConnectionAction
    :<|> Named @"on-user-deleted-connections" onUserDeleted

sendConnectionAction :: Domain -> NewConnectionRequest -> (Handler r) NewConnectionResponse
sendConnectionAction originDomain NewConnectionRequest {..} = do
  active <- lift $ wrapClient $ Data.isActivated ncrTo
  if active
    then do
      self <- qualifyLocal ncrTo
      let other = toRemoteUnsafe originDomain ncrFrom
      mconnection <- lift $ Data.lookupConnection self (qUntagged other)
      maction <- lift $ performRemoteAction self other mconnection ncrAction
      pure $ NewConnectionResponseOk maction
    else pure NewConnectionResponseUserNotActivated

getUserByHandle :: Domain -> Handle -> (Handler r) (Maybe UserProfile)
getUserByHandle domain handle = do
  searchPolicy <- lookupSearchPolicy domain

  let performHandleLookup =
        case searchPolicy of
          NoSearch -> False
          ExactHandleSearch -> True
          FullSearch -> True
  if not performHandleLookup
    then pure Nothing
    else lift $ do
      maybeOwnerId <- API.lookupHandle handle
      case maybeOwnerId of
        Nothing ->
          pure Nothing
        Just ownerId ->
          listToMaybe <$> API.lookupLocalProfiles Nothing [ownerId]

getUsersByIds :: Domain -> [UserId] -> (Handler r) [UserProfile]
getUsersByIds _ uids =
  lift (API.lookupLocalProfiles Nothing uids)

claimPrekey :: Domain -> (UserId, ClientId) -> (Handler r) (Maybe ClientPrekey)
claimPrekey _ (user, client) = do
  API.claimLocalPrekey LegalholdPlusFederationNotImplemented user client !>> clientError

claimPrekeyBundle :: Domain -> UserId -> (Handler r) PrekeyBundle
claimPrekeyBundle _ user =
  API.claimLocalPrekeyBundle LegalholdPlusFederationNotImplemented user !>> clientError

claimMultiPrekeyBundle :: Domain -> UserClients -> (Handler r) UserClientPrekeyMap
claimMultiPrekeyBundle _ uc = API.claimLocalMultiPrekeyBundles LegalholdPlusFederationNotImplemented uc !>> clientError

-- | Searching for federated users on a remote backend should
-- only search by exact handle search, not in elasticsearch.
-- (This decision may change in the future)
searchUsers :: Domain -> SearchRequest -> (Handler r) SearchResponse
searchUsers domain (SearchRequest searchTerm) = do
  searchPolicy <- lookupSearchPolicy domain

  let searches = case searchPolicy of
        NoSearch -> []
        ExactHandleSearch -> [exactHandleSearch]
        FullSearch -> [exactHandleSearch, fullSearch]

  let maxResults = 15

  contacts <- go [] maxResults searches
  pure $ SearchResponse contacts searchPolicy
  where
    go :: [Contact] -> Int -> [Int -> (Handler r) [Contact]] -> (Handler r) [Contact]
    go contacts _ [] = pure contacts
    go contacts maxResult (search : searches) = do
      contactsNew <- search maxResult
      go (contacts <> contactsNew) (maxResult - length contactsNew) searches

    fullSearch :: Int -> (Handler r) [Contact]
    fullSearch n
      | n > 0 = searchResults <$> Q.searchIndex Nothing Nothing searchTerm n
      | otherwise = pure []

    exactHandleSearch :: Int -> (Handler r) [Contact]
    exactHandleSearch n
      | n > 0 = do
        let maybeHandle = parseHandle searchTerm
        maybeOwnerId <- maybe (pure Nothing) (lift . API.lookupHandle) maybeHandle
        case maybeOwnerId of
          Nothing -> pure []
          Just foundUser -> lift $ contactFromProfile <$$> API.lookupLocalProfiles Nothing [foundUser]
      | otherwise = pure []

getUserClients :: Domain -> GetUserClients -> (Handler r) (UserMap (Set PubClient))
getUserClients _ (GetUserClients uids) = API.lookupLocalPubClientsBulk uids !>> clientError

onUserDeleted :: Domain -> UserDeletedConnectionsNotification -> (Handler r) EmptyResponse
onUserDeleted origDomain udcn = lift $ do
  let deletedUser = toRemoteUnsafe origDomain (udcnUser udcn)
      connections = udcnConnections udcn
      event = pure . UserEvent $ UserDeleted (qUntagged deletedUser)
  acceptedLocals <-
    map csv2From
      . filter (\x -> csv2Status x == Accepted)
      <$> Data.lookupRemoteConnectionStatuses (fromRange connections) (fmap pure deletedUser)
  pooledForConcurrentlyN_ 16 (nonEmpty acceptedLocals) $ \(List1 -> recipients) ->
    notify event (tUnqualified deletedUser) Push.RouteDirect Nothing (pure recipients)
  Data.deleteRemoteConnections deletedUser connections
  pure EmptyResponse
