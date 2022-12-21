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
import Brig.API.Error
import Brig.API.Handler (Handler)
import qualified Brig.API.Internal as Internal
import Brig.API.MLS.KeyPackages
import Brig.API.MLS.Util
import qualified Brig.API.User as API
import Brig.API.Util (lookupSearchPolicy)
import Brig.App
import qualified Brig.Data.Connection as Data
import qualified Brig.Data.User as Data
import Brig.Effects.GalleyProvider (GalleyProvider)
import Brig.IO.Intra (notify)
import Brig.Types.User.Event
import Brig.User.API.Handle
import qualified Brig.User.Search.SearchIndex as Q
import Control.Error.Util
import Control.Monad.Trans.Except
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
import Polysemy
import Servant (ServerT)
import Servant.API
import UnliftIO.Async (pooledForConcurrentlyN_)
import Wire.API.Connection
import Wire.API.Federation.API.Brig
import Wire.API.Federation.API.Common
import Wire.API.Federation.Version
import Wire.API.MLS.KeyPackage
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.Routes.Named
import Wire.API.Team.LegalHold (LegalholdProtectee (LegalholdPlusFederationNotImplemented))
import Wire.API.User (UserProfile)
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.User.Search
import Wire.API.UserMap (UserMap)
import Wire.Sem.Concurrency

type FederationAPI = "federation" :> BrigApi

federationSitemap ::
  Members
    '[ GalleyProvider,
       Concurrency 'Unsafe
     ]
    r =>
  ServerT FederationAPI (Handler r)
federationSitemap =
  Named @"api-version" (\_ _ -> pure versionInfo)
    :<|> Named @"get-user-by-handle" (\d h -> getUserByHandle d h)
    :<|> Named @"get-users-by-ids" (\d us -> getUsersByIds d us)
    :<|> Named @"claim-prekey" claimPrekey
    :<|> Named @"claim-prekey-bundle" claimPrekeyBundle
    :<|> Named @"claim-multi-prekey-bundle" claimMultiPrekeyBundle
    :<|> Named @"search-users" (\d sr -> searchUsers d sr)
    :<|> Named @"get-user-clients" getUserClients
    :<|> Named @"get-mls-clients" getMLSClients
    :<|> Named @"send-connection-action" sendConnectionAction
    :<|> Named @"on-user-deleted-connections" onUserDeleted
    :<|> Named @"claim-key-packages" fedClaimKeyPackages

sendConnectionAction :: Domain -> NewConnectionRequest -> Handler r NewConnectionResponse
sendConnectionAction originDomain NewConnectionRequest {..} = do
  active <- lift $ wrapClient $ Data.isActivated ncrTo
  if active
    then do
      self <- qualifyLocal ncrTo
      let other = toRemoteUnsafe originDomain ncrFrom
      mconnection <- lift . wrapClient $ Data.lookupConnection self (tUntagged other)
      maction <- lift $ performRemoteAction self other mconnection ncrAction
      pure $ NewConnectionResponseOk maction
    else pure NewConnectionResponseUserNotActivated

getUserByHandle ::
  Members
    '[ GalleyProvider
     ]
    r =>
  Domain ->
  Handle ->
  ExceptT Error (AppT r) (Maybe UserProfile)
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
      maybeOwnerId <- wrapClient $ API.lookupHandle handle
      case maybeOwnerId of
        Nothing ->
          pure Nothing
        Just ownerId ->
          listToMaybe <$> API.lookupLocalProfiles Nothing [ownerId]

getUsersByIds ::
  Members
    '[ GalleyProvider
     ]
    r =>
  Domain ->
  [UserId] ->
  ExceptT Error (AppT r) [UserProfile]
getUsersByIds _ uids =
  lift (API.lookupLocalProfiles Nothing uids)

claimPrekey :: Domain -> (UserId, ClientId) -> (Handler r) (Maybe ClientPrekey)
claimPrekey _ (user, client) = do
  API.claimLocalPrekey LegalholdPlusFederationNotImplemented user client !>> clientError

claimPrekeyBundle :: Domain -> UserId -> (Handler r) PrekeyBundle
claimPrekeyBundle _ user =
  API.claimLocalPrekeyBundle LegalholdPlusFederationNotImplemented user !>> clientError

claimMultiPrekeyBundle ::
  Members '[Concurrency 'Unsafe] r =>
  Domain ->
  UserClients ->
  Handler r UserClientPrekeyMap
claimMultiPrekeyBundle _ uc = API.claimLocalMultiPrekeyBundles LegalholdPlusFederationNotImplemented uc !>> clientError

fedClaimKeyPackages :: Domain -> ClaimKeyPackageRequest -> Handler r (Maybe KeyPackageBundle)
fedClaimKeyPackages domain ckpr =
  isMLSEnabled >>= \case
    True -> do
      ltarget <- qualifyLocal (ckprTarget ckpr)
      let rusr = toRemoteUnsafe domain (ckprClaimant ckpr)
      lift . fmap hush . runExceptT $
        claimLocalKeyPackages (tUntagged rusr) Nothing ltarget
    False -> pure Nothing

-- | Searching for federated users on a remote backend should
-- only search by exact handle search, not in elasticsearch.
-- (This decision may change in the future)
searchUsers ::
  forall r.
  Members '[GalleyProvider] r =>
  Domain ->
  SearchRequest ->
  ExceptT Error (AppT r) SearchResponse
searchUsers domain (SearchRequest searchTerm) = do
  searchPolicy <- lift $ lookupSearchPolicy domain

  let searches = case searchPolicy of
        NoSearch -> []
        ExactHandleSearch -> [exactHandleSearch]
        FullSearch -> [exactHandleSearch, fullSearch]

  let maxResults = 15

  contacts <- go [] maxResults searches
  pure $ SearchResponse contacts searchPolicy
  where
    go :: [Contact] -> Int -> [Int -> ExceptT Error (AppT r) [Contact]] -> ExceptT Error (AppT r) [Contact]
    go contacts _ [] = pure contacts
    go contacts maxResult (search : searches) = do
      contactsNew <- search maxResult
      go (contacts <> contactsNew) (maxResult - length contactsNew) searches

    fullSearch :: Int -> ExceptT Error (AppT r) [Contact]
    fullSearch n
      | n > 0 = lift $ searchResults <$> Q.searchIndex Q.FederatedSearch searchTerm n
      | otherwise = pure []

    exactHandleSearch :: Int -> ExceptT Error (AppT r) [Contact]
    exactHandleSearch n
      | n > 0 = do
          let maybeHandle = parseHandle searchTerm
          maybeOwnerId <- maybe (pure Nothing) (wrapHttpClientE . API.lookupHandle) maybeHandle
          case maybeOwnerId of
            Nothing -> pure []
            Just foundUser -> lift $ contactFromProfile <$$> API.lookupLocalProfiles Nothing [foundUser]
      | otherwise = pure []

getUserClients :: Domain -> GetUserClients -> (Handler r) (UserMap (Set PubClient))
getUserClients _ (GetUserClients uids) = API.lookupLocalPubClientsBulk uids !>> clientError

getMLSClients :: Domain -> MLSClientsRequest -> Handler r (Set ClientInfo)
getMLSClients _domain mcr = do
  Internal.getMLSClients (mcrUserId mcr) (mcrSignatureScheme mcr)

onUserDeleted :: Domain -> UserDeletedConnectionsNotification -> (Handler r) EmptyResponse
onUserDeleted origDomain udcn = lift $ do
  let deletedUser = toRemoteUnsafe origDomain (udcnUser udcn)
      connections = udcnConnections udcn
      event = pure . UserEvent $ UserDeleted (tUntagged deletedUser)
  acceptedLocals <-
    map csv2From
      . filter (\x -> csv2Status x == Accepted)
      <$> wrapClient (Data.lookupRemoteConnectionStatuses (fromRange connections) (fmap pure deletedUser))
  wrapHttp $
    pooledForConcurrentlyN_ 16 (nonEmpty acceptedLocals) $ \(List1 -> recipients) ->
      notify event (tUnqualified deletedUser) Push.RouteDirect Nothing (pure recipients)
  wrapClient $ Data.deleteRemoteConnections deletedUser connections
  pure EmptyResponse
