{-# LANGUAGE LambdaCase #-}

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

module Brig.Data.Client
  ( -- * Clients
    ClientDataError (..),
    AuthError (..),
    ReAuthError (..),
    ReAuthPolicy,
    reAuthForNewClients,
    addClientWithReAuthPolicy,
    addClient,
    rmClient,
    hasClient,
    lookupClient,
    lookupClients,
    lookupPubClientsBulk,
    lookupClientsBulk,
    lookupClientIds,
    lookupUsersClientIds,
    updateClientLabel,
    updateClientCapabilities,
    updateClientLastActive,

    -- * Prekeys
    claimPrekey,
    updatePrekeys,
    lookupPrekeyIds,

    -- * MLS public keys
    addMLSPublicKeys,
    lookupMLSPublicKey,
  )
where

import Amazonka qualified as AWS
import Amazonka.Data.Text qualified as AWS
import Amazonka.DynamoDB qualified as AWS
import Amazonka.DynamoDB.Lens qualified as AWS
import Bilge.Retry (httpHandlers)
import Brig.AWS
import Brig.App
import Brig.Data.User (AuthError (..), ReAuthError (..))
import Brig.Data.User qualified as User
import Brig.Types.Instances ()
import Brig.User.Auth.DB.Instances ()
import Cassandra as C hiding (Client)
import Cassandra.Settings as C hiding (Client)
import Control.Error
import Control.Exception.Lens qualified as EL
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Random (randomRIO)
import Control.Retry
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Conversion (toByteString, toByteString')
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict qualified as HashMap
import Data.Id
import Data.Json.Util (UTCTimeMillis, toUTCTimeMillis)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time.Clock
import Data.UUID qualified as UUID
import Imports
import Prometheus qualified as Prom
import System.CryptoBox (Result (Success))
import System.CryptoBox qualified as CryptoBox
import System.Logger.Class (field, msg, val)
import System.Logger.Class qualified as Log
import UnliftIO (pooledMapConcurrentlyN)
import Wire.API.MLS.CipherSuite
import Wire.API.User.Auth
import Wire.API.User.Client hiding (UpdateClient (..))
import Wire.API.User.Client.Prekey
import Wire.API.UserMap (UserMap (..))

data ClientDataError
  = TooManyClients
  | ClientReAuthError !ReAuthError
  | ClientMissingAuth
  | MalformedPrekeys
  | MLSPublicKeyDuplicate
  | MLSNotEnabled
  | KeyPackageDecodingError
  | InvalidKeyPackageRef

-- | Re-authentication policy.
--
-- For a potential new client, a policy is a function that takes as arguments
-- the number of existing clients of the same type, and whether the client
-- already exists, and returns whether the user should be forced to
-- re-authenticate.
type ReAuthPolicy = Int -> Bool -> Bool

-- | Default re-authentication policy.
--
-- Re-authenticate if there is at least one other client.
reAuthForNewClients :: ReAuthPolicy
reAuthForNewClients count upsert = count > 0 && not upsert

addClient ::
  (MonadClient m, MonadReader Brig.App.Env m) =>
  UserId ->
  ClientId ->
  NewClient ->
  Int ->
  Maybe (Imports.Set ClientCapability) ->
  ExceptT ClientDataError m (Client, [Client], Word)
addClient = addClientWithReAuthPolicy reAuthForNewClients

addClientWithReAuthPolicy ::
  (MonadClient m, MonadReader Brig.App.Env m) =>
  ReAuthPolicy ->
  UserId ->
  ClientId ->
  NewClient ->
  Int ->
  Maybe (Imports.Set ClientCapability) ->
  ExceptT ClientDataError m (Client, [Client], Word)
addClientWithReAuthPolicy reAuthPolicy u newId c maxPermClients cps = do
  clients <- lookupClients u
  let typed = filter ((== newClientType c) . clientType) clients
  let count = length typed
  let upsert = any exists typed
  when (reAuthPolicy count upsert) $
    fmapLT ClientReAuthError $
      User.reauthenticate u (newClientPassword c)
  let capacity = fmap (+ (-count)) limit
  unless (maybe True (> 0) capacity || upsert) $
    throwE TooManyClients
  new <- insert
  let !total = fromIntegral (length clients + if upsert then 0 else 1)
  let old = maybe (filter (not . exists) typed) (const []) limit
  pure (new, old, total)
  where
    limit :: Maybe Int
    limit = case newClientType c of
      PermanentClientType -> Just maxPermClients
      TemporaryClientType -> Nothing
      LegalHoldClientType -> Nothing

    exists :: Client -> Bool
    exists = (==) newId . clientId

    insert :: (MonadClient m, MonadReader Brig.App.Env m) => ExceptT ClientDataError m Client
    insert = do
      -- Is it possible to do this somewhere else? Otherwise we could use `MonadClient` instead
      now <- toUTCTimeMillis <$> (liftIO =<< view currentTime)
      let keys = unpackLastPrekey (newClientLastKey c) : newClientPrekeys c
      updatePrekeys u newId keys
      let mdl = newClientModel c
          prm = (u, newId, now, newClientType c, newClientLabel c, newClientClass c, newClientCookie c, mdl, C.Set . Set.toList <$> cps)
      retry x5 $ write insertClient (params LocalQuorum prm)
      addMLSPublicKeys u newId (Map.assocs (newClientMLSPublicKeys c))
      pure $!
        Client
          { clientId = newId,
            clientType = newClientType c,
            clientTime = now,
            clientClass = newClientClass c,
            clientLabel = newClientLabel c,
            clientCookie = newClientCookie c,
            clientModel = mdl,
            clientCapabilities = ClientCapabilityList (fromMaybe mempty cps),
            clientMLSPublicKeys = mempty,
            clientLastActive = Nothing
          }

lookupClient :: MonadClient m => UserId -> ClientId -> m (Maybe Client)
lookupClient u c = do
  keys <- retry x1 (query selectMLSPublicKeys (params LocalQuorum (u, c)))
  fmap (toClient keys)
    <$> retry x1 (query1 selectClient (params LocalQuorum (u, c)))

lookupClientsBulk :: (MonadClient m) => [UserId] -> m (Map UserId (Imports.Set Client))
lookupClientsBulk uids = liftClient $ do
  userClientTuples <- pooledMapConcurrentlyN 50 getClientSetWithUser uids
  pure $ Map.fromList userClientTuples
  where
    getClientSetWithUser :: MonadClient m => UserId -> m (UserId, Imports.Set Client)
    getClientSetWithUser u = fmap ((u,) . Set.fromList) . lookupClients $ u

lookupPubClientsBulk :: (MonadClient m) => [UserId] -> m (UserMap (Imports.Set PubClient))
lookupPubClientsBulk uids = liftClient $ do
  userClientTuples <- pooledMapConcurrentlyN 50 getClientSetWithUser uids
  pure $ UserMap $ Map.fromList userClientTuples
  where
    getClientSetWithUser :: MonadClient m => UserId -> m (UserId, Imports.Set PubClient)
    getClientSetWithUser u = (u,) . Set.fromList . map toPubClient <$> executeQuery u

    executeQuery :: MonadClient m => UserId -> m [(ClientId, Maybe ClientClass)]
    executeQuery u = retry x1 (query selectPubClients (params LocalQuorum (Identity u)))

lookupClients :: MonadClient m => UserId -> m [Client]
lookupClients u = do
  keys <-
    (\(cid, ss, Blob b) -> (cid, [(ss, LBS.toStrict b)]))
      <$$> retry x1 (query selectMLSPublicKeysByUser (params LocalQuorum (Identity u)))
  let keyMap = Map.fromListWith (<>) keys
      updateKeys c =
        c
          { clientMLSPublicKeys =
              Map.fromList $ Map.findWithDefault [] (clientId c) keyMap
          }
  updateKeys . toClient []
    <$$> retry x1 (query selectClients (params LocalQuorum (Identity u)))

lookupClientIds :: MonadClient m => UserId -> m [ClientId]
lookupClientIds u =
  map runIdentity
    <$> retry x1 (query selectClientIds (params LocalQuorum (Identity u)))

lookupUsersClientIds :: MonadClient m => [UserId] -> m [(UserId, Set.Set ClientId)]
lookupUsersClientIds us =
  liftClient $ pooledMapConcurrentlyN 16 getClientIds us
  where
    getClientIds u = (u,) <$> fmap Set.fromList (lookupClientIds u)

lookupPrekeyIds :: MonadClient m => UserId -> ClientId -> m [PrekeyId]
lookupPrekeyIds u c =
  map runIdentity
    <$> retry x1 (query selectPrekeyIds (params LocalQuorum (u, c)))

hasClient :: MonadClient m => UserId -> ClientId -> m Bool
hasClient u d = isJust <$> retry x1 (query1 checkClient (params LocalQuorum (u, d)))

rmClient ::
  ( MonadClient m,
    MonadReader Brig.App.Env m,
    MonadCatch m
  ) =>
  UserId ->
  ClientId ->
  m ()
rmClient u c = do
  retry x5 $ write removeClient (params LocalQuorum (u, c))
  retry x5 $ write removeClientKeys (params LocalQuorum (u, c))
  unlessM (isJust <$> view randomPrekeyLocalLock) $ deleteOptLock u c

updateClientLabel :: MonadClient m => UserId -> ClientId -> Maybe Text -> m ()
updateClientLabel u c l = retry x5 $ write updateClientLabelQuery (params LocalQuorum (l, u, c))

updateClientCapabilities :: MonadClient m => UserId -> ClientId -> Maybe (Imports.Set ClientCapability) -> m ()
updateClientCapabilities u c fs = retry x5 $ write updateClientCapabilitiesQuery (params LocalQuorum (C.Set . Set.toList <$> fs, u, c))

-- | If the update fails, which can happen if device does not exist, then ignore the error silently.
updateClientLastActive :: MonadClient m => UserId -> ClientId -> UTCTime -> m ()
updateClientLastActive u c t =
  void . retry x5 $
    trans
      updateClientLastActiveQuery
      (params LocalQuorum (t, u, c))

updatePrekeys :: MonadClient m => UserId -> ClientId -> [Prekey] -> ExceptT ClientDataError m ()
updatePrekeys u c pks = do
  plain <- mapM (hoistEither . fmapL (const MalformedPrekeys) . B64.decode . toByteString' . prekeyKey) pks
  binary <- liftIO $ zipWithM check pks plain
  unless (and binary) $
    throwE MalformedPrekeys
  for_ pks $ \k -> do
    let args = (u, c, prekeyId k, prekeyKey k)
    retry x5 $ write insertClientKey (params LocalQuorum args)
  where
    check a b = do
      i <- CryptoBox.isPrekey b
      case i of
        Success n -> pure (CryptoBox.prekeyId n == keyId (prekeyId a))
        _ -> pure False

claimPrekey ::
  ( Log.MonadLogger m,
    MonadMask m,
    MonadClient m,
    MonadReader Brig.App.Env m,
    Prom.MonadMonitor m
  ) =>
  UserId ->
  ClientId ->
  m (Maybe ClientPrekey)
claimPrekey u c =
  view randomPrekeyLocalLock >>= \case
    -- Use random prekey selection strategy
    Just localLock -> withLocalLock localLock $ do
      prekeys <- retry x1 $ query userPrekeys (params LocalQuorum (u, c))
      prekey <- pickRandomPrekey prekeys
      removeAndReturnPreKey prekey
    -- Use DynamoDB based optimistic locking strategy
    Nothing -> withOptLock u c $ do
      prekey <- retry x1 $ query1 userPrekey (params LocalQuorum (u, c))
      removeAndReturnPreKey prekey
  where
    removeAndReturnPreKey :: (MonadClient f, Log.MonadLogger f) => Maybe (PrekeyId, Text) -> f (Maybe ClientPrekey)
    removeAndReturnPreKey (Just (i, k)) = do
      if i /= lastPrekeyId
        then retry x1 $ write removePrekey (params LocalQuorum (u, c, i))
        else
          Log.debug $
            field "user" (toByteString u)
              . field "client" (toByteString c)
              . msg (val "last resort prekey used")
      pure $ Just (ClientPrekey c (Prekey i k))
    removeAndReturnPreKey Nothing = pure Nothing

    pickRandomPrekey :: MonadIO f => [(PrekeyId, Text)] -> f (Maybe (PrekeyId, Text))
    pickRandomPrekey [] = pure Nothing
    -- unless we only have one key left
    pickRandomPrekey [pk] = pure $ Just pk
    -- pick among list of keys, except lastPrekeyId
    pickRandomPrekey pks = do
      let pks' = filter (\k -> fst k /= lastPrekeyId) pks
      ind <- liftIO $ randomRIO (0, length pks' - 1)
      pure $ atMay pks' ind

lookupMLSPublicKey ::
  MonadClient m =>
  UserId ->
  ClientId ->
  SignatureSchemeTag ->
  m (Maybe LByteString)
lookupMLSPublicKey u c ss =
  (fromBlob . runIdentity) <$$> retry x1 (query1 selectMLSPublicKey (params LocalQuorum (u, c, ss)))

addMLSPublicKeys ::
  MonadClient m =>
  UserId ->
  ClientId ->
  [(SignatureSchemeTag, ByteString)] ->
  ExceptT ClientDataError m ()
addMLSPublicKeys u c = traverse_ (uncurry (addMLSPublicKey u c))

addMLSPublicKey ::
  MonadClient m =>
  UserId ->
  ClientId ->
  SignatureSchemeTag ->
  ByteString ->
  ExceptT ClientDataError m ()
addMLSPublicKey u c ss pk = do
  rows <-
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
          throwE MLSPublicKeyDuplicate
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

checkClient :: PrepQuery R (UserId, ClientId) (Identity ClientId)
checkClient = "SELECT client from clients where user = ? and client = ?"

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

-------------------------------------------------------------------------------
-- Best-effort optimistic locking for prekeys via DynamoDB

ddbClient :: Text
ddbClient = "client"

ddbVersion :: Text
ddbVersion = "version"

ddbKey :: UserId -> ClientId -> AWS.AttributeValue
ddbKey u c = AWS.S (UUID.toText (toUUID u) <> "." <> clientToText c)

key :: UserId -> ClientId -> HashMap Text AWS.AttributeValue
key u c = HashMap.singleton ddbClient (ddbKey u c)

deleteOptLock ::
  ( MonadReader Brig.App.Env m,
    MonadCatch m,
    MonadIO m
  ) =>
  UserId ->
  ClientId ->
  m ()
deleteOptLock u c = do
  t <- view (awsEnv . prekeyTable)
  e <- view (awsEnv . amazonkaEnv)
  void $ exec e (AWS.newDeleteItem t & AWS.deleteItem_key .~ key u c)

withOptLock ::
  forall a m.
  ( MonadIO m,
    MonadReader Brig.App.Env m,
    Log.MonadLogger m,
    Prom.MonadMonitor m
  ) =>
  UserId ->
  ClientId ->
  m a ->
  m a
withOptLock u c ma = go (10 :: Int)
  where
    go !n = do
      v <- (version =<<) <$> execDyn pure get
      a <- ma
      r <- execDyn pure (put v)
      case r of
        Nothing | n > 0 -> reportAttemptFailure >> go (n - 1)
        Nothing -> reportFailureAndLogError >> pure a
        Just _ -> pure a
    version :: AWS.GetItemResponse -> Maybe Word32
    version v = conv . HashMap.lookup ddbVersion =<< (view AWS.getItemResponse_item v)
      where
        conv :: Maybe AWS.AttributeValue -> Maybe Word32
        conv = \case
          Just (AWS.N t) -> readMaybe $ Text.unpack t
          _ -> Nothing
    get :: Text -> AWS.GetItem
    get t =
      AWS.newGetItem t
        & AWS.getItem_key .~ key u c
        & AWS.getItem_consistentRead ?~ True
    put :: Maybe Word32 -> Text -> AWS.PutItem
    put v t =
      AWS.newPutItem t
        & AWS.putItem_item .~ item v
        & AWS.putItem_expected ?~ check v
    check :: Maybe Word32 -> HashMap Text AWS.ExpectedAttributeValue
    check Nothing = HashMap.singleton ddbVersion $ AWS.newExpectedAttributeValue & AWS.expectedAttributeValue_comparisonOperator ?~ AWS.ComparisonOperator_NULL
    check (Just v) =
      HashMap.singleton ddbVersion $
        AWS.newExpectedAttributeValue
          & AWS.expectedAttributeValue_comparisonOperator ?~ AWS.ComparisonOperator_EQ
          & AWS.expectedAttributeValue_attributeValueList ?~ [toAttributeValue v]
    item :: Maybe Word32 -> HashMap Text AWS.AttributeValue
    item v =
      HashMap.insert ddbVersion (toAttributeValue (maybe (1 :: Word32) (+ 1) v)) $
        key u c
    toAttributeValue :: Word32 -> AWS.AttributeValue
    toAttributeValue w = AWS.N $ AWS.toText (fromIntegral w :: Int)
    reportAttemptFailure :: m ()
    reportAttemptFailure = Prom.incCounter optimisticLockGrabAttemptFailedCounter
    reportFailureAndLogError :: m ()
    reportFailureAndLogError = do
      Log.err $
        Log.field "user" (toByteString' u)
          . Log.field "client" (toByteString' c)
          . msg (val "PreKeys: Optimistic lock failed")
      Prom.incCounter optimisticLockFailedCounter
    execDyn ::
      forall r x.
      (AWS.AWSRequest r, Typeable r, Typeable (AWS.AWSResponse r)) =>
      (AWS.AWSResponse r -> Maybe x) ->
      (Text -> r) ->
      m (Maybe x)
    execDyn cnv mkCmd = do
      cmd <- mkCmd <$> view (awsEnv . prekeyTable)
      e <- view (awsEnv . amazonkaEnv)
      liftIO $ execDyn' e cnv cmd
      where
        execDyn' ::
          forall y p.
          (AWS.AWSRequest p, Typeable (AWS.AWSResponse p), Typeable p) =>
          AWS.Env ->
          (AWS.AWSResponse p -> Maybe y) ->
          p ->
          IO (Maybe y)
        execDyn' e conv cmd = recovering policy handlers (const run)
          where
            run = execCatch e cmd >>= either handleErr (pure . conv)
            handlers = httpHandlers ++ [const $ EL.handler_ AWS._ConditionalCheckFailedException (pure True)]
            policy = limitRetries 3 <> exponentialBackoff 100000
            handleErr (AWS.ServiceError se) | se ^. AWS.serviceError_code == AWS.ErrorCode "ProvisionedThroughputExceeded" = do
              Prom.incCounter dynProvisionedThroughputExceededCounter
              pure Nothing
            handleErr _ = pure Nothing

withLocalLock :: (MonadMask m, MonadIO m) => MVar () -> m a -> m a
withLocalLock l ma = do
  (takeMVar l *> ma) `finally` putMVar l ()

{-# NOINLINE optimisticLockGrabAttemptFailedCounter #-}
optimisticLockGrabAttemptFailedCounter :: Prom.Counter
optimisticLockGrabAttemptFailedCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "client.opt_lock.optimistic_lock_grab_attempt_failed",
          Prom.metricHelp = "Number of times grab attempts for optimisitic lock on prekeys failed"
        }

{-# NOINLINE optimisticLockFailedCounter #-}
optimisticLockFailedCounter :: Prom.Counter
optimisticLockFailedCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "client.opt_lock.optimistic_lock_failed",
          Prom.metricHelp = "Number of time optimisitic lock on prekeys failed"
        }

{-# NOINLINE dynProvisionedThroughputExceededCounter #-}
dynProvisionedThroughputExceededCounter :: Prom.Counter
dynProvisionedThroughputExceededCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "client.opt_lock.provisioned_throughput_exceeded",
          Prom.metricHelp = "Number of times provisioned throughput on DynamoDB was exceeded"
        }
