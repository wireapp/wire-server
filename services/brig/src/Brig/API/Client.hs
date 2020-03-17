-- TODO: Move to Brig.User.Client
module Brig.API.Client
  ( -- * Clients
    addClient,
    updateClient,
    rmClient,
    pubClient,
    legalHoldClientRequested,
    removeLegalHoldClient,
    lookupClient,
    lookupClients,
    Data.lookupPrekeyIds,
    Data.lookupUsersClientIds,

    -- * Prekeys
    claimPrekey,
    claimPrekeyBundle,
    claimMultiPrekeyBundles,
    Data.lookupClientIds,
  )
where

import Brig.API.Error (federationNotImplemented, throwStd)
import Brig.API.Handler (Handler)
import Brig.API.Types
import Brig.API.Util (resolveOpaqueUserId)
import Brig.App
import qualified Brig.Data.Client as Data
import qualified Brig.Data.User as Data
import qualified Brig.IO.Intra as Intra
import qualified Brig.Options as Opt
import Brig.Types
import Brig.Types.Intra
import Brig.Types.Team.LegalHold (LegalHoldClientRequest (..))
import qualified Brig.User.Auth.Cookie as Auth
import Brig.User.Email
import Brig.User.Event
import Control.Error
import Control.Lens (view)
import Data.Bitraversable (bitraverse)
import Data.ByteString.Conversion
import Data.IP (IP)
import qualified Data.Id as Id
import Data.Id (ClientId, ConnId, UserId, makeIdOpaque, makeMappedIdOpaque)
import Data.IdMapping
import Data.List.NonEmpty (nonEmpty)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map
import Data.Misc (PlainTextPassword (..))
import Galley.Types (UserClientMap (..), UserClients (..))
import Imports
import Network.Wai.Utilities
import System.Logger.Class (field, msg, val, (~~))
import qualified System.Logger.Class as Log
import UnliftIO.Async (Concurrently (Concurrently, runConcurrently))

lookupClient :: MappedOrLocalId Id.U -> ClientId -> ExceptT ClientError AppIO (Maybe Client)
lookupClient mappedOrLocalUserId clientId =
  case mappedOrLocalUserId of
    Local u ->
      lift $ lookupLocalClient u clientId
    Mapped IdMapping {idMappingLocal} ->
      -- FUTUREWORK(federation): look up remote clients
      throwE $ ClientUserNotFound (makeMappedIdOpaque idMappingLocal)

lookupLocalClient :: UserId -> ClientId -> AppIO (Maybe Client)
lookupLocalClient = Data.lookupClient

lookupClients :: MappedOrLocalId Id.U -> ExceptT ClientError AppIO [Client]
lookupClients = \case
  Local u ->
    lift $ lookupLocalClients u
  Mapped IdMapping {idMappingLocal} ->
    -- FUTUREWORK(federation): look up remote clients
    throwE $ ClientUserNotFound (makeMappedIdOpaque idMappingLocal)

lookupLocalClients :: UserId -> AppIO [Client]
lookupLocalClients = Data.lookupClients

-- nb. We must ensure that the set of clients known to brig is always
-- a superset of the clients known to galley.
--
-- ClientAdded event to self
-- UserLegalHoldEnabled event to contacts, if client is legalhold
-- ClientRemoved event to self, if removing old clients
addClient :: E -> UserId -> Maybe ConnId -> Maybe IP -> NewClient -> ExceptT ClientError AppIO Client
addClient E u con ip new = do
  acc <- lift (Data.lookupAccount u) >>= maybe (throwE (ClientUserNotFound (makeIdOpaque u))) return
  loc <- maybe (return Nothing) locationOf ip
  maxPermClients <- fromMaybe Opt.defUserMaxPermClients <$> Opt.setUserMaxPermClients <$> view settings
  (clt, old, count) <- Data.addClient u clientId' new maxPermClients loc !>> ClientDataError
  let usr = accountUser acc
  lift $ do
    -- ClientRemoved event to self, if removing old clients
    for_ old $ execDelete E u con
    Intra.newClient u (clientId clt)
    -- ClientAdded event to self
    Intra.onClientEvent E u con (ClientAdded u clt)
    -- UserLegalHoldEnabled event to contacts, if client is legalhold
    when (clientType clt == LegalHoldClientType) $ Intra.onUserEvent E u con (UserLegalHoldEnabled u)
    when (count > 1)
      $ for_ (userEmail usr)
      $ \email ->
        sendNewClientEmail (userName usr) email clt (userLocale usr)
  return clt
  where
    clientId' = clientIdFromPrekey (unpackLastPrekey $ newClientLastKey new)

updateClient :: UserId -> ClientId -> UpdateClient -> ExceptT ClientError AppIO ()
updateClient u c r = do
  ok <- lift $ Data.hasClient u c
  unless ok $
    throwE ClientNotFound
  for_ (updateClientLabel r) $ lift . Data.updateClientLabel u c . Just
  let lk = maybeToList (unpackLastPrekey <$> updateClientLastKey r)
  Data.updatePrekeys u c (lk ++ updateClientPrekeys r) !>> ClientDataError

-- nb. We must ensure that the set of clients known to brig is always
-- a superset of the clients known to galley.
--
-- ClientRemoved event to self
rmClient :: E -> UserId -> ConnId -> ClientId -> Maybe PlainTextPassword -> ExceptT ClientError AppIO ()
rmClient E u con clt pw =
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
      -- ClientRemoved event to self
      lift $ execDelete E u (Just con) client

claimPrekey :: MappedOrLocalId Id.U -> ClientId -> AppIO (Maybe ClientPrekey)
claimPrekey u c = case u of
  Local localUser ->
    claimLocalPrekey localUser c
  Mapped _ ->
    pure Nothing

claimLocalPrekey :: UserId -> ClientId -> AppIO (Maybe ClientPrekey)
claimLocalPrekey u c = do
  prekey <- Data.claimPrekey u c
  case prekey of
    Nothing -> noPrekeys u c >> return Nothing
    pk@(Just _) -> return pk

claimPrekeyBundle :: MappedOrLocalId Id.U -> AppIO PrekeyBundle
claimPrekeyBundle = \case
  Local localUser ->
    claimLocalPrekeyBundle localUser
  Mapped IdMapping {idMappingLocal} ->
    pure $ PrekeyBundle (makeMappedIdOpaque idMappingLocal) []

claimLocalPrekeyBundle :: UserId -> AppIO PrekeyBundle
claimLocalPrekeyBundle u = do
  clients <- map clientId <$> Data.lookupClients u
  PrekeyBundle (makeIdOpaque u) . catMaybes <$> mapM (Data.claimPrekey u) clients

claimMultiPrekeyBundles :: UserClients -> Handler (UserClientMap (Maybe Prekey))
claimMultiPrekeyBundles (UserClients clientMap) = do
  resolved <- traverse (bitraverse resolveOpaqueUserId pure) $ Map.toList clientMap
  let (localUsers, remoteUsers) = partitionEithers $ map localOrRemoteUser resolved
  for_ (nonEmpty remoteUsers) $
    throwStd . federationNotImplemented . fmap fst
  -- FUTUREWORK(federation): claim keys from other backends, merge maps
  lift $ UserClientMap . Map.mapKeys makeIdOpaque <$> claimLocalPrekeyBundles localUsers
  where
    localOrRemoteUser :: (MappedOrLocalId Id.U, a) -> Either (UserId, a) (IdMapping Id.U, a)
    localOrRemoteUser (mappedOrLocal, x) =
      case mappedOrLocal of
        Local localId -> Left (localId, x)
        Mapped mapping -> Right (mapping, x)

claimLocalPrekeyBundles :: [(UserId, Set ClientId)] -> AppIO (Map UserId (Map ClientId (Maybe Prekey)))
claimLocalPrekeyBundles = foldMap getChunk . fmap Map.fromList . chunksOf 16
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
--
-- ClientRemoved event to self
execDelete :: E -> UserId -> Maybe ConnId -> Client -> AppIO ()
execDelete E u con c = do
  Intra.rmClient u (clientId c)
  for_ (clientCookie c) $ \l -> Auth.revokeCookies u [] [l]
  -- ClientRemoved event to self
  Intra.onClientEvent E u con (ClientRemoved u c)
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

-- LegalHoldClientRequested event to contacts
legalHoldClientRequested :: E -> UserId -> LegalHoldClientRequest -> AppIO ()
legalHoldClientRequested E targetUser (LegalHoldClientRequest _requester lastPrekey') =
  -- LegalHoldClientRequested event to contacts
  Intra.onUserEvent E targetUser Nothing lhClientEvent
  where
    clientId :: ClientId
    clientId = clientIdFromPrekey $ unpackLastPrekey lastPrekey'
    eventData :: LegalHoldClientRequestedData
    eventData = LegalHoldClientRequestedData targetUser lastPrekey' clientId
    lhClientEvent :: UserEvent
    lhClientEvent = LegalHoldClientRequested eventData

-- ClientRemoved event to self
-- UserLegalHoldDisabled event to contacts
removeLegalHoldClient :: E -> UserId -> AppIO ()
removeLegalHoldClient E uid = do
  clients <- Data.lookupClients uid
  -- Should only be one; but just in case we'll treat it as a list
  let legalHoldClients = filter ((== LegalHoldClientType) . clientType) clients
  -- maybe log if this isn't the case
  -- ClientRemoved event to self
  forM_ legalHoldClients (execDelete E uid Nothing)
  -- UserLegalHoldDisabled event to contacts
  Intra.onUserEvent E uid Nothing (UserLegalHoldDisabled uid)
