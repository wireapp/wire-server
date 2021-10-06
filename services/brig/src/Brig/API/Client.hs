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

-- TODO: Move to Brig.User.Client
module Brig.API.Client
  ( -- * Clients
    addClient,
    updateClient,
    rmClient,
    pubClient,
    legalHoldClientRequested,
    removeLegalHoldClient,
    lookupLocalClient,
    lookupLocalClients,
    lookupPubClient,
    lookupPubClients,
    lookupPubClientsBulk,
    lookupLocalPubClientsBulk,
    Data.lookupPrekeyIds,
    Data.lookupUsersClientIds,

    -- * Prekeys
    claimLocalMultiPrekeyBundles,
    claimLocalPrekeyBundle,
    claimPrekey,
    claimLocalPrekey,
    claimPrekeyBundle,
    claimMultiPrekeyBundles,
    Data.lookupClientIds,
  )
where

import Brig.API.Types
import Brig.App
import qualified Brig.Data.Client as Data
import qualified Brig.Data.User as Data
import Brig.Federation.Client (getUserClients)
import qualified Brig.Federation.Client as Federation
import Brig.IO.Intra (guardLegalhold)
import qualified Brig.IO.Intra as Intra
import qualified Brig.Options as Opt
import Brig.Types
import Brig.Types.Intra
import Brig.Types.Team.LegalHold (LegalHoldClientRequest (..))
import Brig.Types.User.Event
import qualified Brig.User.Auth.Cookie as Auth
import Brig.User.Email
import Control.Error
import Control.Lens (view)
import Data.ByteString.Conversion
import Data.Domain (Domain)
import Data.IP (IP)
import Data.Id (ClientId, ConnId, UserId)
import Data.List.Split (chunksOf)
import Data.Map.Strict (traverseWithKey)
import qualified Data.Map.Strict as Map
import Data.Misc (PlainTextPassword (..))
import Data.Qualified
import qualified Data.Set as Set
import Galley.Types (UserClients (..))
import Imports
import Network.Wai.Utilities
import System.Logger.Class (field, msg, val, (~~))
import qualified System.Logger.Class as Log
import UnliftIO.Async (Concurrently (Concurrently, runConcurrently))
import Wire.API.Federation.API.Brig (GetUserClients (GetUserClients))
import qualified Wire.API.Message as Message
import Wire.API.Team.LegalHold (LegalholdProtectee (..))
import Wire.API.User.Client (ClientCapabilityList (..), QualifiedUserClientPrekeyMap (..), QualifiedUserClients (..), UserClientPrekeyMap, mkQualifiedUserClientPrekeyMap, mkUserClientPrekeyMap)
import qualified Wire.API.User.Client as Client
import Wire.API.UserMap (QualifiedUserMap (QualifiedUserMap, qualifiedUserMap), UserMap (userMap))

lookupLocalClient :: UserId -> ClientId -> AppIO (Maybe Client)
lookupLocalClient = Data.lookupClient

lookupLocalClients :: UserId -> AppIO [Client]
lookupLocalClients = Data.lookupClients

lookupPubClient :: Qualified UserId -> ClientId -> ExceptT ClientError AppIO (Maybe PubClient)
lookupPubClient qid cid = do
  clients <- lookupPubClients qid
  pure $ find ((== cid) . pubClientId) clients

lookupPubClients :: Qualified UserId -> ExceptT ClientError AppIO [PubClient]
lookupPubClients qid@(Qualified uid domain) = do
  getForUser <$> lookupPubClientsBulk [qid]
  where
    getForUser :: QualifiedUserMap (Set PubClient) -> [PubClient]
    getForUser qmap = fromMaybe [] $ do
      um <- userMap <$> Map.lookup domain (qualifiedUserMap qmap)
      Set.toList <$> Map.lookup uid um

lookupPubClientsBulk :: [Qualified UserId] -> ExceptT ClientError AppIO (QualifiedUserMap (Set PubClient))
lookupPubClientsBulk qualifiedUids = do
  loc <- qualifyLocal ()
  let (localUsers, remoteUsers) = partitionQualified loc qualifiedUids
  remoteUserClientMap <-
    traverseWithKey
      (\domain' uids -> getUserClients domain' (GetUserClients uids))
      (indexQualified (fmap qUntagged remoteUsers))
      !>> ClientFederationError
  localUserClientMap <- Map.singleton (lDomain loc) <$> lookupLocalPubClientsBulk localUsers
  pure $ QualifiedUserMap (Map.union localUserClientMap remoteUserClientMap)

lookupLocalPubClientsBulk :: [UserId] -> ExceptT ClientError AppIO (UserMap (Set PubClient))
lookupLocalPubClientsBulk = Data.lookupPubClientsBulk

-- nb. We must ensure that the set of clients known to brig is always
-- a superset of the clients known to galley.
addClient :: UserId -> Maybe ConnId -> Maybe IP -> NewClient -> ExceptT ClientError AppIO Client
addClient u con ip new = do
  acc <- lift (Data.lookupAccount u) >>= maybe (throwE (ClientUserNotFound u)) return
  loc <- maybe (return Nothing) locationOf ip
  maxPermClients <- fromMaybe Opt.defUserMaxPermClients <$> Opt.setUserMaxPermClients <$> view settings
  let caps :: Maybe (Set Client.ClientCapability)
      caps = updlhdev $ newClientCapabilities new
        where
          updlhdev =
            if newClientType new == LegalHoldClientType
              then Just . maybe (Set.singleton lhcaps) (Set.insert lhcaps)
              else id
          lhcaps = Client.ClientSupportsLegalholdImplicitConsent
  (clt, old, count) <- Data.addClient u clientId' new maxPermClients loc caps !>> ClientDataError
  let usr = accountUser acc
  lift $ do
    for_ old $ execDelete u con
    Intra.newClient u (clientId clt)
    Intra.onClientEvent u con (ClientAdded u clt)
    when (clientType clt == LegalHoldClientType) $ Intra.onUserEvent u con (UserLegalHoldEnabled u)
    when (count > 1) $
      for_ (userEmail usr) $
        \email ->
          sendNewClientEmail (userDisplayName usr) email clt (userLocale usr)
  return clt
  where
    clientId' = clientIdFromPrekey (unpackLastPrekey $ newClientLastKey new)

updateClient :: UserId -> ClientId -> UpdateClient -> ExceptT ClientError AppIO ()
updateClient u c r = do
  client <- lift (Data.lookupClient u c) >>= maybe (throwE ClientNotFound) pure
  for_ (updateClientLabel r) $ lift . Data.updateClientLabel u c . Just
  for_ (updateClientCapabilities r) $ \caps' -> do
    let ClientCapabilityList caps = clientCapabilities client
    if caps `Set.isSubsetOf` caps'
      then lift . Data.updateClientCapabilities u c . Just $ caps'
      else throwE ClientCapabilitiesCannotBeRemoved
  let lk = maybeToList (unpackLastPrekey <$> updateClientLastKey r)
  Data.updatePrekeys u c (lk ++ updateClientPrekeys r) !>> ClientDataError

-- nb. We must ensure that the set of clients known to brig is always
-- a superset of the clients known to galley.
rmClient :: UserId -> ConnId -> ClientId -> Maybe PlainTextPassword -> ExceptT ClientError AppIO ()
rmClient u con clt pw =
  maybe (throwE ClientNotFound) fn =<< lift (Data.lookupClient u clt)
  where
    fn client = do
      case clientType client of
        -- Legal hold clients can't be removed
        LegalHoldClientType -> throwE ClientLegalHoldCannotBeRemoved
        -- Temporary clients don't need to re-auth
        TemporaryClientType -> pure ()
        -- All other clients must authenticate
        _ -> Data.reauthenticate u pw !>> ClientDataError . ClientReAuthError
      lift $ execDelete u (Just con) client

claimPrekey :: LegalholdProtectee -> UserId -> Domain -> ClientId -> ExceptT ClientError AppIO (Maybe ClientPrekey)
claimPrekey protectee u d c = do
  isLocalDomain <- (d ==) <$> viewFederationDomain
  if isLocalDomain
    then claimLocalPrekey protectee u c
    else claimRemotePrekey (Qualified u d) c

claimLocalPrekey :: LegalholdProtectee -> UserId -> ClientId -> ExceptT ClientError AppIO (Maybe ClientPrekey)
claimLocalPrekey protectee user client = do
  guardLegalhold protectee (Client.mkUserClients [(user, [client])])
  lift $ do
    prekey <- Data.claimPrekey user client
    when (isNothing prekey) (noPrekeys user client)
    pure prekey

claimRemotePrekey :: Qualified UserId -> ClientId -> ExceptT ClientError AppIO (Maybe ClientPrekey)
claimRemotePrekey quser client = fmapLT ClientFederationError $ Federation.claimPrekey quser client

claimPrekeyBundle :: LegalholdProtectee -> Domain -> UserId -> ExceptT ClientError AppIO PrekeyBundle
claimPrekeyBundle protectee domain uid = do
  isLocalDomain <- (domain ==) <$> viewFederationDomain
  if isLocalDomain
    then claimLocalPrekeyBundle protectee uid
    else claimRemotePrekeyBundle (Qualified uid domain)

claimLocalPrekeyBundle :: LegalholdProtectee -> UserId -> ExceptT ClientError AppIO PrekeyBundle
claimLocalPrekeyBundle protectee u = do
  clients <- map clientId <$> Data.lookupClients u
  guardLegalhold protectee (Client.mkUserClients [(u, clients)])
  PrekeyBundle u . catMaybes <$> lift (mapM (Data.claimPrekey u) clients)

claimRemotePrekeyBundle :: Qualified UserId -> ExceptT ClientError AppIO PrekeyBundle
claimRemotePrekeyBundle quser = do
  Federation.claimPrekeyBundle quser !>> ClientFederationError

claimMultiPrekeyBundles :: LegalholdProtectee -> QualifiedUserClients -> ExceptT ClientError AppIO QualifiedUserClientPrekeyMap
claimMultiPrekeyBundles protectee quc = do
  localDomain <- viewFederationDomain
  fmap (mkQualifiedUserClientPrekeyMap . Map.fromList)
    -- FUTUREWORK(federation): parallelise federator requests here
    . traverse (\(domain, uc) -> (domain,) <$> claim localDomain domain (UserClients uc))
    . Map.assocs
    . qualifiedUserClients
    $ quc
  where
    claim :: Domain -> Domain -> UserClients -> ExceptT ClientError AppIO UserClientPrekeyMap
    claim localDomain domain uc
      | domain == localDomain = claimLocalMultiPrekeyBundles protectee uc
      | otherwise = Federation.claimMultiPrekeyBundle domain uc !>> ClientFederationError

claimLocalMultiPrekeyBundles :: LegalholdProtectee -> UserClients -> ExceptT ClientError AppIO UserClientPrekeyMap
claimLocalMultiPrekeyBundles protectee userClients = do
  guardLegalhold protectee userClients
  lift
    . fmap mkUserClientPrekeyMap
    . foldMap (getChunk . Map.fromList)
    . chunksOf 16
    . Map.toList
    . Message.userClients
    $ userClients
  where
    getChunk :: Map UserId (Set ClientId) -> AppIO (Map UserId (Map ClientId (Maybe Prekey)))
    getChunk =
      runConcurrently . Map.traverseWithKey (\u -> Concurrently . getUserKeys u)
    getUserKeys :: UserId -> Set ClientId -> AppIO (Map ClientId (Maybe Prekey))
    getUserKeys u =
      sequenceA . Map.fromSet (getClientKeys u)
    getClientKeys :: UserId -> ClientId -> AppIO (Maybe Prekey)
    getClientKeys u c = do
      key <- fmap prekeyData <$> Data.claimPrekey u c
      when (isNothing key) $ noPrekeys u c
      return key

-- Utilities

-- | Perform an orderly deletion of an existing client.
execDelete :: UserId -> Maybe ConnId -> Client -> AppIO ()
execDelete u con c = do
  Intra.rmClient u (clientId c)
  for_ (clientCookie c) $ \l -> Auth.revokeCookies u [] [l]
  Intra.onClientEvent u con (ClientRemoved u c)
  Data.rmClient u (clientId c)

-- | Defensive measure when no prekey is found for a
-- requested client: Ensure that the client does indeed
-- not exist, since there must be no client without prekeys,
-- thus repairing any inconsistencies related to distributed
-- (and possibly duplicated) client data.
noPrekeys :: UserId -> ClientId -> AppIO ()
noPrekeys u c = do
  Log.info $
    field "user" (toByteString u)
      ~~ field "client" (toByteString c)
      ~~ msg (val "No prekey found. Ensuring client does not exist.")
  Intra.rmClient u c
  client <- Data.lookupClient u c
  for_ client $ \_ ->
    Log.err $
      field "user" (toByteString u)
        ~~ field "client" (toByteString c)
        ~~ msg (val "Client exists without prekeys.")

pubClient :: Client -> PubClient
pubClient c =
  PubClient
    { pubClientId = clientId c,
      pubClientClass = clientClass c
    }

legalHoldClientRequested :: UserId -> LegalHoldClientRequest -> AppIO ()
legalHoldClientRequested targetUser (LegalHoldClientRequest _requester lastPrekey') =
  Intra.onUserEvent targetUser Nothing lhClientEvent
  where
    clientId :: ClientId
    clientId = clientIdFromPrekey $ unpackLastPrekey lastPrekey'
    eventData :: LegalHoldClientRequestedData
    eventData = LegalHoldClientRequestedData targetUser lastPrekey' clientId
    lhClientEvent :: UserEvent
    lhClientEvent = LegalHoldClientRequested eventData

removeLegalHoldClient :: UserId -> AppIO ()
removeLegalHoldClient uid = do
  clients <- Data.lookupClients uid
  -- Should only be one; but just in case we'll treat it as a list
  let legalHoldClients = filter ((== LegalHoldClientType) . clientType) clients
  -- maybe log if this isn't the case
  forM_ legalHoldClients (execDelete uid Nothing)
  Intra.onUserEvent uid Nothing (UserLegalHoldDisabled uid)
