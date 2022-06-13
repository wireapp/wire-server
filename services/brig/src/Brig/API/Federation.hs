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

import Bilge.IO
import Bilge.RPC
import qualified Brig.API.Client as API
import Brig.API.Connection.Remote (performRemoteAction)
import Brig.API.Error
import Brig.API.Handler (Handler)
import qualified Brig.API.Internal as Internal
import Brig.API.MLS.KeyPackages
import qualified Brig.API.User as API
import Brig.API.Util (lookupSearchPolicy)
import Brig.App
import qualified Brig.Data.Connection as Data
import qualified Brig.Data.User as Data
import Brig.IO.Intra (notify)
import Brig.Types (PrekeyBundle, Relation (Accepted))
import Brig.Types.User.Event
import Brig.User.API.Handle
import Brig.User.Search.Index
import qualified Brig.User.Search.SearchIndex as Q
import Cassandra (MonadClient)
import Control.Error.Util
import Control.Monad.Catch (MonadMask)
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
import Servant (ServerT)
import Servant.API
import qualified System.Logger.Class as Log
import UnliftIO.Async (pooledForConcurrentlyN_)
import Wire.API.Federation.API.Brig
import Wire.API.Federation.API.Common
import Wire.API.Federation.Version
import Wire.API.MLS.KeyPackage
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
  Named @"api-version" (\_ _ -> pure versionInfo)
    :<|> Named @"get-user-by-handle" (\d h -> wrapHttpClientE $ getUserByHandle d h)
    :<|> Named @"get-users-by-ids" (\d us -> wrapHttpClientE $ getUsersByIds d us)
    :<|> Named @"claim-prekey" claimPrekey
    :<|> Named @"claim-prekey-bundle" claimPrekeyBundle
    :<|> Named @"claim-multi-prekey-bundle" claimMultiPrekeyBundle
    :<|> Named @"search-users" (\d sr -> wrapHttpClientE $ searchUsers d sr)
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
      mconnection <- lift . wrapClient $ Data.lookupConnection self (qUntagged other)
      maction <- lift $ performRemoteAction self other mconnection ncrAction
      pure $ NewConnectionResponseOk maction
    else pure NewConnectionResponseUserNotActivated

getUserByHandle ::
  ( HasRequestId m,
    Log.MonadLogger m,
    MonadClient m,
    MonadHttp m,
    MonadMask m,
    MonadReader Env m
  ) =>
  Domain ->
  Handle ->
  ExceptT Error m (Maybe UserProfile)
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

getUsersByIds ::
  ( MonadClient m,
    MonadReader Env m,
    Log.MonadLogger m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m
  ) =>
  Domain ->
  [UserId] ->
  ExceptT Error m [UserProfile]
getUsersByIds _ uids =
  lift (API.lookupLocalProfiles Nothing uids)

claimPrekey :: Domain -> (UserId, ClientId) -> (Handler r) (Maybe ClientPrekey)
claimPrekey _ (user, client) = do
  wrapHttpClientE (API.claimLocalPrekey LegalholdPlusFederationNotImplemented user client) !>> clientError

claimPrekeyBundle :: Domain -> UserId -> (Handler r) PrekeyBundle
claimPrekeyBundle _ user =
  API.claimLocalPrekeyBundle LegalholdPlusFederationNotImplemented user !>> clientError

claimMultiPrekeyBundle :: Domain -> UserClients -> (Handler r) UserClientPrekeyMap
claimMultiPrekeyBundle _ uc = API.claimLocalMultiPrekeyBundles LegalholdPlusFederationNotImplemented uc !>> clientError

fedClaimKeyPackages :: Domain -> ClaimKeyPackageRequest -> Handler r (Maybe KeyPackageBundle)
fedClaimKeyPackages domain ckpr = do
  ltarget <- qualifyLocal (ckprTarget ckpr)
  let rusr = toRemoteUnsafe domain (ckprClaimant ckpr)
  lift . fmap hush . runExceptT $
    claimLocalKeyPackages (qUntagged rusr) Nothing ltarget

-- | Searching for federated users on a remote backend should
-- only search by exact handle search, not in elasticsearch.
-- (This decision may change in the future)
searchUsers ::
  forall m.
  ( HasRequestId m,
    Log.MonadLogger m,
    MonadClient m,
    MonadHttp m,
    MonadIndexIO m,
    MonadMask m,
    MonadReader Env m
  ) =>
  Domain ->
  SearchRequest ->
  ExceptT Error m SearchResponse
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
    go :: [Contact] -> Int -> [Int -> ExceptT Error m [Contact]] -> ExceptT Error m [Contact]
    go contacts _ [] = pure contacts
    go contacts maxResult (search : searches) = do
      contactsNew <- search maxResult
      go (contacts <> contactsNew) (maxResult - length contactsNew) searches

    fullSearch :: Int -> ExceptT Error m [Contact]
    fullSearch n
      | n > 0 = lift $ searchResults <$> Q.searchIndex Q.FederatedSearch searchTerm n
      | otherwise = pure []

    exactHandleSearch :: Int -> ExceptT Error m [Contact]
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

getMLSClients :: Domain -> MLSClientsRequest -> Handler r (Set ClientId)
getMLSClients _domain mcr = do
  Internal.getMLSClients (mcrUserId mcr) (mcrSignatureScheme mcr)

onUserDeleted :: Domain -> UserDeletedConnectionsNotification -> (Handler r) EmptyResponse
onUserDeleted origDomain udcn = lift $ do
  let deletedUser = toRemoteUnsafe origDomain (udcnUser udcn)
      connections = udcnConnections udcn
      event = pure . UserEvent $ UserDeleted (qUntagged deletedUser)
  acceptedLocals <-
    map csv2From
      . filter (\x -> csv2Status x == Accepted)
      <$> wrapClient (Data.lookupRemoteConnectionStatuses (fromRange connections) (fmap pure deletedUser))
  wrapHttp $
    pooledForConcurrentlyN_ 16 (nonEmpty acceptedLocals) $ \(List1 -> recipients) ->
      notify event (tUnqualified deletedUser) Push.RouteDirect Nothing (pure recipients)
  wrapClient $ Data.deleteRemoteConnections deletedUser connections
  pure EmptyResponse
