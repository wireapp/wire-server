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

import Brig.API.Client qualified as API
import Brig.API.Connection.Remote (performRemoteAction)
import Brig.API.Error
import Brig.API.Handler (Handler)
import Brig.API.Internal qualified as Internal
import Brig.API.MLS.CipherSuite
import Brig.API.MLS.KeyPackages
import Brig.API.MLS.Util
import Brig.API.User qualified as API
import Brig.App
import Brig.Data.Connection qualified as Data
import Brig.Data.User qualified as Data
import Brig.Effects.FederationConfigStore (FederationConfigStore)
import Brig.Effects.FederationConfigStore qualified as E
import Brig.Effects.GalleyProvider (GalleyProvider)
import Brig.IO.Intra (notify)
import Brig.Options
import Brig.Types.User.Event
import Brig.User.API.Handle
import Brig.User.Search.SearchIndex qualified as Q
import Control.Error.Util
import Control.Lens ((^.))
import Control.Monad.Trans.Except
import Data.Domain
import Data.Handle (Handle (..), parseHandle)
import Data.Id (ClientId, TeamId, UserId)
import Data.List.NonEmpty (nonEmpty)
import Data.Qualified
import Data.Range
import Data.Set (fromList, (\\))
import Gundeck.Types.Push qualified as Push
import Imports hiding ((\\))
import Network.Wai.Utilities.Error ((!>>))
import Polysemy
import Servant (ServerT)
import Servant.API
import Wire.API.Connection
import Wire.API.Federation.API.Brig hiding (searchPolicy)
import Wire.API.Federation.API.Common
import Wire.API.Federation.Endpoint
import Wire.API.Federation.Version
import Wire.API.MLS.KeyPackage
import Wire.API.Routes.FederationDomainConfig as FD
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.Routes.Named
import Wire.API.Team.LegalHold (LegalholdProtectee (LegalholdPlusFederationNotImplemented))
import Wire.API.User (UserProfile)
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.User.Search hiding (searchPolicy)
import Wire.API.UserMap (UserMap)
import Wire.NotificationSubsystem
import Wire.Sem.Concurrency

type FederationAPI = "federation" :> BrigApi

federationSitemap ::
  ( Member GalleyProvider r,
    Member (Concurrency 'Unsafe) r,
    Member FederationConfigStore r,
    Member NotificationSubsystem r
  ) =>
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
    :<|> Named @(Versioned 'V0 "get-mls-clients") getMLSClientsV0
    :<|> Named @"get-mls-clients" getMLSClients
    :<|> Named @"send-connection-action" sendConnectionAction
    :<|> Named @"claim-key-packages" fedClaimKeyPackages
    :<|> Named @"get-not-fully-connected-backends" getFederationStatus
    :<|> Named @"on-user-deleted-connections" onUserDeleted

-- Allow remote domains to send their known remote federation instances, and respond
-- with the subset of those we aren't connected to.
getFederationStatus :: (Member FederationConfigStore r) => Domain -> DomainSet -> Handler r NonConnectedBackends
getFederationStatus _ request = do
  cfg <- ask
  case setFederationStrategy (cfg ^. settings) of
    Just AllowAll -> pure $ NonConnectedBackends mempty
    _ -> do
      fedDomains <- fromList . fmap (.domain) . (.remotes) <$> lift (liftSem $ E.getFederationConfigs)
      pure $ NonConnectedBackends (request.domains \\ fedDomains)

sendConnectionAction ::
  ( Member FederationConfigStore r,
    Member NotificationSubsystem r,
    Member GalleyProvider r
  ) =>
  Domain ->
  NewConnectionRequest ->
  Handler r NewConnectionResponse
sendConnectionAction originDomain NewConnectionRequest {..} = do
  let rTeam = qTagUnsafe (Qualified fromTeam originDomain)
  federates <- lift . liftSem . E.backendFederatesWith $ rTeam
  if federates
    then do
      active <- lift $ wrapClient $ Data.isActivated to
      if active
        then do
          self <- qualifyLocal to
          let other = toRemoteUnsafe originDomain from
          mconnection <- lift . wrapClient $ Data.lookupConnection self (tUntagged other)
          maction <- lift $ performRemoteAction self other mconnection action
          pure $ NewConnectionResponseOk maction
        else pure NewConnectionResponseUserNotActivated
    else pure NewConnectionResponseNotFederating

getUserByHandle ::
  ( Member GalleyProvider r,
    Member FederationConfigStore r
  ) =>
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
  Member GalleyProvider r =>
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
  Member (Concurrency 'Unsafe) r =>
  Domain ->
  UserClients ->
  Handler r UserClientPrekeyMap
claimMultiPrekeyBundle _ uc = API.claimLocalMultiPrekeyBundles LegalholdPlusFederationNotImplemented uc !>> clientError

fedClaimKeyPackages :: Domain -> ClaimKeyPackageRequest -> Handler r (Maybe KeyPackageBundle)
fedClaimKeyPackages domain ckpr =
  isMLSEnabled >>= \case
    True -> do
      suite <- getCipherSuite (Just ckpr.cipherSuite)
      ltarget <- qualifyLocal ckpr.target
      let rusr = toRemoteUnsafe domain ckpr.claimant
      lift . fmap hush . runExceptT $
        claimLocalKeyPackages (tUntagged rusr) Nothing suite ltarget
    False -> pure Nothing

-- | Searching for federated users on a remote backend
searchUsers ::
  forall r.
  ( Member GalleyProvider r,
    Member FederationConfigStore r
  ) =>
  Domain ->
  SearchRequest ->
  ExceptT Error (AppT r) SearchResponse
searchUsers domain (SearchRequest _ mTeam (Just [])) = do
  searchPolicy <- lookupSearchPolicyWithTeam domain mTeam
  pure $ SearchResponse [] searchPolicy
searchUsers domain (SearchRequest searchTerm mTeam mOnlyInTeams) = do
  searchPolicy <- lookupSearchPolicyWithTeam domain mTeam

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
      | n > 0 = lift $ searchResults <$> Q.searchIndex (Q.FederatedSearch mOnlyInTeams) searchTerm n
      | otherwise = pure []

    exactHandleSearch :: Int -> ExceptT Error (AppT r) [Contact]
    exactHandleSearch n
      | n > 0 = do
          let maybeHandle = parseHandle searchTerm
          maybeOwnerId <- maybe (pure Nothing) (wrapHttpClientE . API.lookupHandle) maybeHandle
          case maybeOwnerId of
            Nothing -> pure []
            Just foundUser -> do
              mFoundUserTeamId <- lift $ wrapClient $ Data.lookupUserTeam foundUser
              if isTeamAllowed mOnlyInTeams mFoundUserTeamId
                then lift $ contactFromProfile <$$> API.lookupLocalProfiles Nothing [foundUser]
                else pure []
      | otherwise = pure []

    isTeamAllowed :: Maybe [TeamId] -> Maybe TeamId -> Bool
    isTeamAllowed Nothing _ = True
    isTeamAllowed (Just _) Nothing = False
    isTeamAllowed (Just teams) (Just tid) = tid `elem` teams

getUserClients :: Domain -> GetUserClients -> (Handler r) (UserMap (Set PubClient))
getUserClients _ (GetUserClients uids) = API.lookupLocalPubClientsBulk uids !>> clientError

getMLSClients :: Domain -> MLSClientsRequest -> Handler r (Set ClientInfo)
getMLSClients _domain mcr = do
  Internal.getMLSClients mcr.userId mcr.cipherSuite

getMLSClientsV0 :: Domain -> MLSClientsRequestV0 -> Handler r (Set ClientInfo)
getMLSClientsV0 domain mcr0 = getMLSClients domain (mlsClientsRequestFromV0 mcr0)

onUserDeleted ::
  ( Member (Concurrency 'Unsafe) r,
    Member NotificationSubsystem r
  ) =>
  Domain ->
  UserDeletedConnectionsNotification ->
  (Handler r) EmptyResponse
onUserDeleted origDomain udcn = lift $ do
  let deletedUser = toRemoteUnsafe origDomain udcn.user
      connections = udcn.connections
      event = pure . UserEvent $ UserDeleted (tUntagged deletedUser)
  acceptedLocals <-
    map csv2From
      . filter (\x -> csv2Status x == Accepted)
      <$> wrapClient (Data.lookupRemoteConnectionStatuses (fromRange connections) (fmap pure deletedUser))
  liftSem $
    unsafePooledForConcurrentlyN_ 16 (nonEmpty acceptedLocals) $ \recipients ->
      notify event (tUnqualified deletedUser) Push.RouteDirect Nothing (pure recipients)
  wrapClient $ Data.deleteRemoteConnections deletedUser connections
  pure EmptyResponse

-- | If domain is not configured fall back to `NoSearch`
lookupSearchPolicy :: (Member FederationConfigStore r) => Domain -> (Handler r) FederatedUserSearchPolicy
lookupSearchPolicy domain = do
  mConfig <- lift $ liftSem $ E.getFederationConfig domain
  pure $ maybe NoSearch searchPolicy mConfig

-- | If domain is not configured fall back to `NoSearch`
-- if a team is provided, check if the team is allowed to search
-- if no team is provided, and restriction is set by team, fall back to `NoSearch`
lookupSearchPolicyWithTeam :: (Member FederationConfigStore r) => Domain -> Maybe TeamId -> (Handler r) FederatedUserSearchPolicy
lookupSearchPolicyWithTeam domain mSearcherTeamId =
  lift $
    liftSem $
      E.getFederationConfig domain <&> \case
        Nothing -> NoSearch
        Just (FederationDomainConfig _ sp FederationRestrictionAllowAll) -> sp
        Just (FederationDomainConfig _ sp (FederationRestrictionByTeam teams)) ->
          maybe NoSearch (\tid -> if tid `elem` teams then sp else NoSearch) $ mSearcherTeamId
