{-# OPTIONS_GHC -Wwarn #-}

module Wire.ClientStore.Cassandra (ClientStoreCassandraEnv (..), interpretClientStoreCassandra) where

import Cassandra as C hiding (Client)
import Cassandra qualified as C
import Cassandra.Settings as C hiding (Client)
import Control.Error (atMay)
import Control.Monad.Random (randomRIO)
import Data.ByteString.Lazy qualified as LBS
import Data.Id
import Data.Json.Util
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Time
import Imports
import Polysemy
import Polysemy.Embed
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import UnliftIO (pooledMapConcurrentlyN)
import Wire.API.MLS.CipherSuite
import Wire.API.User.Auth
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.UserMap
import Wire.ClientStore (ClientStore (..), DuplicateMLSPublicKey (..))
import Wire.ClientStore.DynamoDB
import Wire.Sem.Metrics (Metrics)

data ClientStoreCassandraEnv = ClientStoreCassandraEnv
  { casClient :: ClientState,
    prekeyLocking :: Either (MVar ()) OptimisticLockEnv
  }

interpretClientStoreCassandra ::
  ( Member TinyLog r,
    Member (Final IO) r,
    Member Metrics r
  ) =>
  ClientStoreCassandraEnv -> InterpreterFor ClientStore r
interpretClientStoreCassandra env =
  interpret $
    runInputConst env . \case
      -- Lifecycle
      Upsert uid cid timestamp nc -> upsertImpl uid cid timestamp nc
      Delete uid cid -> deleteImpl uid cid
      UpdateLabel uid cid lbl -> runCasClient $ updateLabelImpl uid cid lbl
      UpdateCapabilities uid cid caps -> runCasClient $ updateCapabilitiesImpl uid cid caps
      UpdateLastActive uid cid timestamp -> runCasClient $ updateLastActiveImpl uid cid timestamp
      -- Lookups
      LookupClient uid cid -> runCasClient $ lookupClientImpl uid cid
      LookupClients uid -> runCasClient $ lookupClientsImpl uid
      LookupClientIds uid -> runCasClient $ lookupClientIdsImpl uid
      LookupClientIdsBulk uids -> runCasClient $ lookupClientIdsBulkImpl uids
      LookupClientsBulk uids -> runCasClient $ lookupClientsBulkImpl uids
      LookupPubClientsBulk uids -> runCasClient $ lookupPubClientsBulkImpl uids
      LookupPrekeyIds uid cid -> runCasClient $ lookupPrekeyIdsImpl uid cid
      -- Proteus
      UpdatePrekeys uid cid prekeys -> runCasClient $ updatePrekeysImpl uid cid prekeys
      ClaimPrekey uid cid -> claimPrekeyImpl uid cid
      -- MLS
      AddMLSPublicKeys uid cid keys -> addMLSPublicKeysImpl uid cid keys
      LookupMLSPublicKey uid cid scheme -> runCasClient $ lookupMLSPublicKeyImpl uid cid scheme

runCasClient :: (Member (Input ClientStoreCassandraEnv) r, Member (Final IO) r) => C.Client a -> Sem r a
runCasClient action = do
  c <- inputs (.casClient)
  embedToFinal . runEmbedded (C.runClient c) . embed $ action

upsertImpl :: (Member (Input ClientStoreCassandraEnv) r, Member (Final IO) r) => UserId -> ClientId -> UTCTimeMillis -> NewClient -> Sem r (Maybe DuplicateMLSPublicKey)
upsertImpl uid newId now c = do
  let keys = unpackLastPrekey (newClientLastKey c) : newClientPrekeys c
  runCasClient $ do
    updatePrekeysImpl uid newId keys
    let prm = (uid, newId, now, newClientType c, newClientLabel c, newClientClass c, newClientCookie c, newClientModel c, C.Set . Set.toList . fromClientCapabilityList <$> newClientCapabilities c)
    retry x5 $ write insertClient (params LocalQuorum prm)
  addMLSPublicKeysImpl uid newId (Map.assocs (newClientMLSPublicKeys c))

lookupClientImpl :: (MonadClient m) => UserId -> ClientId -> m (Maybe Client)
lookupClientImpl u c = do
  keys <- retry x1 (query selectMLSPublicKeys (params LocalQuorum (u, c)))
  fmap (toClient keys)
    <$> retry x1 (query1 selectClient (params LocalQuorum (u, c)))

lookupClientsBulkImpl :: (MonadClient m) => [UserId] -> m (UserMap (Imports.Set Client))
lookupClientsBulkImpl uids = liftClient $ do
  userClientTuples <- pooledMapConcurrentlyN 50 getClientSetWithUser uids
  pure . UserMap $ Map.fromList userClientTuples
  where
    getClientSetWithUser :: (MonadClient m) => UserId -> m (UserId, Imports.Set Client)
    getClientSetWithUser u = fmap ((u,) . Set.fromList) . lookupClientsImpl $ u

lookupPubClientsBulkImpl :: (MonadClient m) => [UserId] -> m (UserMap (Imports.Set PubClient))
lookupPubClientsBulkImpl uids = liftClient $ do
  userClientTuples <- pooledMapConcurrentlyN 50 getClientSetWithUser uids
  pure . UserMap $ Map.fromList userClientTuples
  where
    getClientSetWithUser :: (MonadClient m) => UserId -> m (UserId, Imports.Set PubClient)
    getClientSetWithUser u = (u,) . Set.fromList . map toPubClient <$> executeQuery u

    executeQuery :: (MonadClient m) => UserId -> m [(ClientId, Maybe ClientClass)]
    executeQuery u = retry x1 (query selectPubClients (params LocalQuorum (Identity u)))

lookupClientsImpl :: (MonadClient m) => UserId -> m [Client]
lookupClientsImpl u = do
  keys <-
    (\(cid, ss, Blob b) -> (cid, [(ss, LBS.toStrict b)]))
      <$$> retry x1 (query selectMLSPublicKeysByUser (params LocalQuorum (Identity u)))
  let keyMap = Map.fromListWith (<>) keys
      updateKeys c =
        c
          { clientMLSPublicKeys =
              Map.fromList $ Map.findWithDefault [] c.clientId keyMap
          }
  updateKeys . toClient []
    <$$> retry x1 (query selectClients (params LocalQuorum (Identity u)))

lookupClientIdsImpl :: (MonadClient m) => UserId -> m [ClientId]
lookupClientIdsImpl u =
  map runIdentity
    <$> retry x1 (query selectClientIds (params LocalQuorum (Identity u)))

lookupClientIdsBulkImpl :: (MonadClient m) => [UserId] -> m UserClients
lookupClientIdsBulkImpl us =
  UserClients . Map.fromList <$> (liftClient $ pooledMapConcurrentlyN 16 getClientIds us)
  where
    getClientIds u = (u,) <$> fmap Set.fromList (lookupClientIdsImpl u)

lookupPrekeyIdsImpl :: (MonadClient m) => UserId -> ClientId -> m [PrekeyId]
lookupPrekeyIdsImpl u c =
  map runIdentity
    <$> retry x1 (query selectPrekeyIds (params LocalQuorum (u, c)))

deleteImpl ::
  (Member (Input ClientStoreCassandraEnv) r, Member (Final IO) r) =>
  UserId ->
  ClientId ->
  Sem r ()
deleteImpl u c = do
  runCasClient $ do
    retry x5 $ write removeClient (params LocalQuorum (u, c))
    retry x5 $ write removeClientKeys (params LocalQuorum (u, c))
  inputs (.prekeyLocking) >>= \case
    Left _ -> pure ()
    Right optLockEnv ->
      embedToFinal . runInputConst optLockEnv $ deleteOptLock u c

-- todo "call deleteOptLock"

updateLabelImpl :: (MonadClient m) => UserId -> ClientId -> Maybe Text -> m ()
updateLabelImpl u c l = retry x5 $ write updateClientLabelQuery (params LocalQuorum (l, u, c))

updateCapabilitiesImpl :: (MonadClient m) => UserId -> ClientId -> Maybe ClientCapabilityList -> m ()
updateCapabilitiesImpl u c fs = retry x5 $ write updateClientCapabilitiesQuery (params LocalQuorum (C.Set . Set.toList . fromClientCapabilityList <$> fs, u, c))

-- | If the update fails, which can happen if device does not exist, then ignore the error silently.
updateLastActiveImpl :: (MonadClient m) => UserId -> ClientId -> UTCTime -> m ()
updateLastActiveImpl u c t =
  void . retry x5 $
    trans
      updateClientLastActiveQuery
      (params LocalQuorum (t, u, c))

-- TODO: Add check to upstream callers of this function
updatePrekeysImpl :: (MonadClient m) => UserId -> ClientId -> [UncheckedPrekeyBundle] -> m ()
updatePrekeysImpl u c pks = do
  for_ pks $ \k -> do
    let args = (u, c, prekeyId k, prekeyKey k)
    retry x5 $ write insertClientKey (params LocalQuorum args)

-- claimPrekeyImpl :: UserId -> ClientId -> m (Maybe ClientPrekey)
-- claimPrekeyImpl = todo "implement ClaimPrekey"

claimPrekeyImpl ::
  forall r.
  ( Member (Final IO) r,
    Member (Input ClientStoreCassandraEnv) r,
    Member Metrics r,
    Member TinyLog r
  ) =>
  UserId ->
  ClientId ->
  Sem r (Maybe ClientPrekey)
claimPrekeyImpl u c = do
  cas <- inputs (.casClient)
  inputs (.prekeyLocking) >>= \case
    -- Use random prekey selection strategy
    Left localLock -> embedFinal $ withLocalLock localLock $ do
      prekeys <- C.runClient cas $ retry x1 $ query userPrekeys (params LocalQuorum (u, c))
      prekey <- pickRandomPrekey prekeys
      C.runClient cas $ traverse removeAndReturnPreKey prekey
    -- Use DynamoDB based optimistic locking strategy
    Right optLockEnv -> runInputConst optLockEnv . withOptLock u c . embedFinal . C.runClient cas $ do
      prekey <- retry x1 $ query1 userPrekey (params LocalQuorum (u, c))
      traverse removeAndReturnPreKey prekey
  where
    removeAndReturnPreKey :: (PrekeyId, Text) -> C.Client ClientPrekey
    removeAndReturnPreKey (i, k) = do
      if i /= lastPrekeyId
        then retry x1 $ write removePrekey (params LocalQuorum (u, c, i))
        else pure ()
      -- Log.debug $
      --   field "user" (toByteString u)
      --     . field "client" (toByteString c)
      --     . msg (val "last resort prekey used")
      pure $ ClientPrekey c (UncheckedPrekeyBundle i k)

    pickRandomPrekey :: [(PrekeyId, Text)] -> IO (Maybe (PrekeyId, Text))
    pickRandomPrekey [] = pure Nothing
    -- unless we only have one key left
    pickRandomPrekey [pk] = pure $ Just pk
    -- pick among list of keys, except lastPrekeyId
    pickRandomPrekey pks = do
      let pks' = filter (\k -> fst k /= lastPrekeyId) pks
      ind <- randomRIO (0, length pks' - 1)
      pure $ atMay pks' ind

lookupMLSPublicKeyImpl ::
  (MonadClient m) =>
  UserId ->
  ClientId ->
  SignatureSchemeTag ->
  m (Maybe LByteString)
lookupMLSPublicKeyImpl u c ss =
  (fromBlob . runIdentity) <$$> retry x1 (query1 selectMLSPublicKey (params LocalQuorum (u, c, ss)))

addMLSPublicKeysImpl ::
  (Member (Input ClientStoreCassandraEnv) r, Member (Final IO) r) =>
  UserId ->
  ClientId ->
  [(SignatureSchemeTag, ByteString)] ->
  Sem r (Maybe DuplicateMLSPublicKey)
addMLSPublicKeysImpl u c keys =
  runError (traverse_ (uncurry (addMLSPublicKey u c)) keys) >>= \case
    Left e -> pure $ Just e
    Right () -> pure Nothing

-- TODO: Add checks to callers of this
addMLSPublicKey ::
  (Member (Input ClientStoreCassandraEnv) r, Member (Final IO) r, Member (Error DuplicateMLSPublicKey) r) =>
  UserId ->
  ClientId ->
  SignatureSchemeTag ->
  ByteString ->
  Sem r ()
addMLSPublicKey u c ss pk = do
  rows <-
    runCasClient $
      trans
        insertMLSPublicKeys
        ( params
            LocalQuorum
            (u, c, ss, Blob (LBS.fromStrict pk))
        )
          { serialConsistency = Just LocalSerialConsistency
          }
  case rows of
    [row]
      | C.fromRow 0 row /= Right (Just True) ->
          throw DuplicateMLSPublicKey
    _ -> pure ()

-------------------------------------------------------------------------------
-- Queries

insertClient :: PrepQuery W (UserId, ClientId, UTCTimeMillis, ClientType, Maybe Text, Maybe ClientClass, Maybe CookieLabel, Maybe Text, Maybe (C.Set ClientCapability)) ()
insertClient = "INSERT INTO clients (user, client, tstamp, type, label, class, cookie, model, capabilities) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"

updateClientLabelQuery :: PrepQuery W (Maybe Text, UserId, ClientId) ()
updateClientLabelQuery = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE clients SET label = ? WHERE user = ? AND client = ?"

updateClientCapabilitiesQuery :: PrepQuery W (Maybe (C.Set ClientCapability), UserId, ClientId) ()
updateClientCapabilitiesQuery = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE clients SET capabilities = ? WHERE user = ? AND client = ?"

updateClientLastActiveQuery :: PrepQuery W (UTCTime, UserId, ClientId) Row
updateClientLastActiveQuery = "UPDATE clients SET last_active = ? WHERE user = ? AND client = ? IF EXISTS"

selectClientIds :: PrepQuery R (Identity UserId) (Identity ClientId)
selectClientIds = "SELECT client from clients where user = ?"

selectClients :: PrepQuery R (Identity UserId) (ClientId, ClientType, UTCTimeMillis, Maybe Text, Maybe ClientClass, Maybe CookieLabel, Maybe Text, Maybe (C.Set ClientCapability), Maybe UTCTime)
selectClients = "SELECT client, type, tstamp, label, class, cookie, model, capabilities, last_active from clients where user = ?"

selectPubClients :: PrepQuery R (Identity UserId) (ClientId, Maybe ClientClass)
selectPubClients = "SELECT client, class from clients where user = ?"

selectClient :: PrepQuery R (UserId, ClientId) (ClientId, ClientType, UTCTimeMillis, Maybe Text, Maybe ClientClass, Maybe CookieLabel, Maybe Text, Maybe (C.Set ClientCapability), Maybe UTCTime)
selectClient = "SELECT client, type, tstamp, label, class, cookie, model, capabilities, last_active from clients where user = ? and client = ?"

insertClientKey :: PrepQuery W (UserId, ClientId, PrekeyId, Text) ()
insertClientKey = "INSERT INTO prekeys (user, client, key, data) VALUES (?, ?, ?, ?)"

removeClient :: PrepQuery W (UserId, ClientId) ()
removeClient = "DELETE FROM clients where user = ? and client = ?"

removeClientKeys :: PrepQuery W (UserId, ClientId) ()
removeClientKeys = "DELETE FROM prekeys where user = ? and client = ?"

userPrekey :: PrepQuery R (UserId, ClientId) (PrekeyId, Text)
userPrekey = "SELECT key, data FROM prekeys where user = ? and client = ? LIMIT 1"

userPrekeys :: PrepQuery R (UserId, ClientId) (PrekeyId, Text)
userPrekeys = "SELECT key, data FROM prekeys where user = ? and client = ?"

selectPrekeyIds :: PrepQuery R (UserId, ClientId) (Identity PrekeyId)
selectPrekeyIds = "SELECT key FROM prekeys where user = ? and client = ?"

removePrekey :: PrepQuery W (UserId, ClientId, PrekeyId) ()
removePrekey = "DELETE FROM prekeys where user = ? and client = ? and key = ?"

selectMLSPublicKey :: PrepQuery R (UserId, ClientId, SignatureSchemeTag) (Identity Blob)
selectMLSPublicKey = "SELECT key from mls_public_keys where user = ? and client = ? and sig_scheme = ?"

selectMLSPublicKeys :: PrepQuery R (UserId, ClientId) (SignatureSchemeTag, Blob)
selectMLSPublicKeys = "SELECT sig_scheme, key from mls_public_keys where user = ? and client = ?"

selectMLSPublicKeysByUser :: PrepQuery R (Identity UserId) (ClientId, SignatureSchemeTag, Blob)
selectMLSPublicKeysByUser = "SELECT client, sig_scheme, key from mls_public_keys where user = ?"

insertMLSPublicKeys :: PrepQuery W (UserId, ClientId, SignatureSchemeTag, Blob) Row
insertMLSPublicKeys =
  "INSERT INTO mls_public_keys (user, client, sig_scheme, key) \
  \VALUES (?, ?, ?, ?) IF NOT EXISTS"

-------------------------------------------------------------------------------
-- Conversions

toClient ::
  [(SignatureSchemeTag, Blob)] ->
  ( ClientId,
    ClientType,
    UTCTimeMillis,
    Maybe Text,
    Maybe ClientClass,
    Maybe CookieLabel,
    Maybe Text,
    Maybe (C.Set ClientCapability),
    Maybe UTCTime
  ) ->
  Client
toClient keys (cid, cty, tme, lbl, cls, cok, mdl, cps, lastActive) =
  Client
    { clientId = cid,
      clientType = cty,
      clientTime = tme,
      clientClass = cls,
      clientLabel = lbl,
      clientCookie = cok,
      clientModel = mdl,
      clientCapabilities = ClientCapabilityList $ maybe Set.empty (Set.fromList . C.fromSet) cps,
      clientMLSPublicKeys = fmap (LBS.toStrict . fromBlob) (Map.fromList keys),
      clientLastActive = lastActive
    }

toPubClient :: (ClientId, Maybe ClientClass) -> PubClient
toPubClient = uncurry PubClient
