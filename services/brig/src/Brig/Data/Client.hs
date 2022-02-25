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
    addClient,
    rmClient,
    hasClient,
    lookupClient,
    lookupClients,
    lookupPubClientsBulk,
    lookupClientsBulk,
    lookupClientIds,
    lookupUsersClientIds,
    Brig.Data.Client.updateClientLabel,
    Brig.Data.Client.updateClientCapabilities,

    -- * Prekeys
    claimPrekey,
    updatePrekeys,
    lookupPrekeyIds,
  )
where

import qualified Amazonka as AWS
import qualified Amazonka.DynamoDB as AWS
import qualified Amazonka.DynamoDB.Lens as AWS
import Bilge.Retry (httpHandlers)
import Brig.AWS
import Brig.App (AppIO, awsEnv, currentTime, metrics, randomPrekeyLocalLock)
import Brig.Data.Instances ()
import Brig.Data.User (AuthError (..), ReAuthError (..))
import qualified Brig.Data.User as User
import Brig.Types
import Brig.Types.Instances ()
import Brig.Types.User.Auth (CookieLabel)
import Brig.User.Auth.DB.Instances ()
import Cassandra as C hiding (Client)
import Control.Error
import qualified Control.Exception.Lens as EL
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Random (randomRIO)
import Control.Retry
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Conversion (toByteString, toByteString')
import qualified Data.HashMap.Strict as HashMap
import Data.Id
import Data.Json.Util (UTCTimeMillis, toUTCTimeMillis)
import qualified Data.Map as Map
import qualified Data.Metrics as Metrics
import Data.Misc
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import Imports
import System.CryptoBox (Result (Success))
import qualified System.CryptoBox as CryptoBox
import System.Logger.Class (field, msg, val)
import qualified System.Logger.Class as Log
import UnliftIO (pooledMapConcurrentlyN)
import Wire.API.User.Client (ClientCapability, ClientCapabilityList (ClientCapabilityList))
import Wire.API.UserMap (UserMap (..))

data ClientDataError
  = TooManyClients
  | ClientReAuthError !ReAuthError
  | ClientMissingAuth
  | MalformedPrekeys

addClient ::
  UserId ->
  ClientId ->
  NewClient ->
  Int ->
  Maybe Location ->
  Maybe (Imports.Set ClientCapability) ->
  ExceptT ClientDataError (AppIO r) (Client, [Client], Word)
addClient u newId c maxPermClients loc cps = do
  clients <- lookupClients u
  let typed = filter ((== newClientType c) . clientType) clients
  let count = length typed
  let upsert = any exists typed
  unless (count == 0 || upsert) $
    fmapLT ClientReAuthError $
      User.reauthenticate u (newClientPassword c)
  let capacity = fmap (+ (- count)) limit
  unless (maybe True (> 0) capacity || upsert) $
    throwE TooManyClients
  new <- insert
  let !total = fromIntegral (length clients + if upsert then 0 else 1)
  let old = maybe (filter (not . exists) typed) (const []) limit
  return (new, old, total)
  where
    limit :: Maybe Int
    limit = case newClientType c of
      PermanentClientType -> Just maxPermClients
      TemporaryClientType -> Nothing
      LegalHoldClientType -> Nothing

    exists :: Client -> Bool
    exists = (==) newId . clientId

    insert :: MonadClient m => ExceptT ClientDataError m Client
    insert = do
      -- Is it possible to do this somewhere else? Otherwise we could use `MonadClient` instead
      now <- toUTCTimeMillis <$> (liftIO =<< view currentTime)
      let keys = unpackLastPrekey (newClientLastKey c) : newClientPrekeys c
      updatePrekeys u newId keys
      let lat = Latitude . view latitude <$> loc
          lon = Longitude . view longitude <$> loc
          mdl = newClientModel c
          prm = (u, newId, now, newClientType c, newClientLabel c, newClientClass c, newClientCookie c, lat, lon, mdl, C.Set . Set.toList <$> cps)
      retry x5 $ write insertClient (params LocalQuorum prm)
      return $! Client newId (newClientType c) now (newClientClass c) (newClientLabel c) (newClientCookie c) loc mdl (ClientCapabilityList $ fromMaybe mempty cps)

lookupClient :: MonadClient m => UserId -> ClientId -> m (Maybe Client)
lookupClient u c =
  fmap toClient
    <$> retry x1 (query1 selectClient (params LocalQuorum (u, c)))

lookupClientsBulk :: (MonadClient m) => [UserId] -> m (Map UserId (Imports.Set Client))
lookupClientsBulk uids = liftClient $ do
  userClientTuples <- pooledMapConcurrentlyN 50 getClientSetWithUser uids
  pure $ Map.fromList userClientTuples
  where
    getClientSetWithUser :: MonadClient m => UserId -> m (UserId, Imports.Set Client)
    getClientSetWithUser u = (u,) . Set.fromList <$> executeQuery u

    executeQuery :: MonadClient m => UserId -> m [Client]
    executeQuery u = toClient <$$> retry x1 (query selectClients (params LocalQuorum (Identity u)))

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
lookupClients u =
  map toClient
    <$> retry x1 (query selectClients (params LocalQuorum (Identity u)))

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

rmClient :: UserId -> ClientId -> (AppIO r) ()
rmClient u c = do
  retry x5 $ write removeClient (params LocalQuorum (u, c))
  retry x5 $ write removeClientKeys (params LocalQuorum (u, c))
  unlessM (isJust <$> view randomPrekeyLocalLock) $ deleteOptLock u c

updateClientLabel :: MonadClient m => UserId -> ClientId -> Maybe Text -> m ()
updateClientLabel u c l = retry x5 $ write updateClientLabelQuery (params LocalQuorum (l, u, c))

updateClientCapabilities :: MonadClient m => UserId -> ClientId -> Maybe (Imports.Set ClientCapability) -> m ()
updateClientCapabilities u c fs = retry x5 $ write updateClientCapabilitiesQuery (params LocalQuorum (C.Set . Set.toList <$> fs, u, c))

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
        Success n -> return (CryptoBox.prekeyId n == keyId (prekeyId a))
        _ -> return False

claimPrekey :: UserId -> ClientId -> (AppIO r) (Maybe ClientPrekey)
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
    removeAndReturnPreKey :: Maybe (PrekeyId, Text) -> (AppIO r) (Maybe ClientPrekey)
    removeAndReturnPreKey (Just (i, k)) = do
      if i /= lastPrekeyId
        then retry x1 $ write removePrekey (params LocalQuorum (u, c, i))
        else
          Log.debug $
            field "user" (toByteString u)
              . field "client" (toByteString c)
              . msg (val "last resort prekey used")
      return $ Just (ClientPrekey c (Prekey i k))
    removeAndReturnPreKey Nothing = return Nothing

    pickRandomPrekey :: [(PrekeyId, Text)] -> (AppIO r) (Maybe (PrekeyId, Text))
    pickRandomPrekey [] = return Nothing
    -- unless we only have one key left
    pickRandomPrekey [pk] = return $ Just pk
    -- pick among list of keys, except lastPrekeyId
    pickRandomPrekey pks = do
      let pks' = filter (\k -> fst k /= lastPrekeyId) pks
      ind <- liftIO $ randomRIO (0, length pks' - 1)
      return $ atMay pks' ind

-------------------------------------------------------------------------------
-- Queries

insertClient :: PrepQuery W (UserId, ClientId, UTCTimeMillis, ClientType, Maybe Text, Maybe ClientClass, Maybe CookieLabel, Maybe Latitude, Maybe Longitude, Maybe Text, Maybe (C.Set ClientCapability)) ()
insertClient = "INSERT INTO clients (user, client, tstamp, type, label, class, cookie, lat, lon, model, capabilities) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

updateClientLabelQuery :: PrepQuery W (Maybe Text, UserId, ClientId) ()
updateClientLabelQuery = "UPDATE clients SET label = ? WHERE user = ? AND client = ?"

updateClientCapabilitiesQuery :: PrepQuery W (Maybe (C.Set ClientCapability), UserId, ClientId) ()
updateClientCapabilitiesQuery = "UPDATE clients SET capabilities = ? WHERE user = ? AND client = ?"

selectClientIds :: PrepQuery R (Identity UserId) (Identity ClientId)
selectClientIds = "SELECT client from clients where user = ?"

selectClients :: PrepQuery R (Identity UserId) (ClientId, ClientType, UTCTimeMillis, Maybe Text, Maybe ClientClass, Maybe CookieLabel, Maybe Latitude, Maybe Longitude, Maybe Text, Maybe (C.Set ClientCapability))
selectClients = "SELECT client, type, tstamp, label, class, cookie, lat, lon, model, capabilities from clients where user = ?"

selectPubClients :: PrepQuery R (Identity UserId) (ClientId, Maybe ClientClass)
selectPubClients = "SELECT client, class from clients where user = ?"

selectClient :: PrepQuery R (UserId, ClientId) (ClientId, ClientType, UTCTimeMillis, Maybe Text, Maybe ClientClass, Maybe CookieLabel, Maybe Latitude, Maybe Longitude, Maybe Text, Maybe (C.Set ClientCapability))
selectClient = "SELECT client, type, tstamp, label, class, cookie, lat, lon, model, capabilities from clients where user = ? and client = ?"

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

-------------------------------------------------------------------------------
-- Conversions

toClient :: (ClientId, ClientType, UTCTimeMillis, Maybe Text, Maybe ClientClass, Maybe CookieLabel, Maybe Latitude, Maybe Longitude, Maybe Text, Maybe (C.Set ClientCapability)) -> Client
toClient (cid, cty, tme, lbl, cls, cok, lat, lon, mdl, cps) =
  Client
    { clientId = cid,
      clientType = cty,
      clientTime = tme,
      clientClass = cls,
      clientLabel = lbl,
      clientCookie = cok,
      clientLocation = location <$> lat <*> lon,
      clientModel = mdl,
      clientCapabilities = ClientCapabilityList $ maybe Set.empty (Set.fromList . C.fromSet) cps
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
ddbKey u c = AWS.S (UUID.toText (toUUID u) <> "." <> client c)

key :: UserId -> ClientId -> HashMap Text AWS.AttributeValue
key u c = HashMap.singleton ddbClient (ddbKey u c)

deleteOptLock :: UserId -> ClientId -> (AppIO r) ()
deleteOptLock u c = do
  t <- view (awsEnv . prekeyTable)
  e <- view (awsEnv . amazonkaEnv)
  void $ exec e (AWS.newDeleteItem t & AWS.deleteItem_key .~ key u c)

withOptLock :: forall effs a. UserId -> ClientId -> (AppIO effs) a -> (AppIO effs) a
withOptLock u c ma = go (10 :: Int)
  where
    go !n = do
      v <- (version =<<) <$> execDyn return get
      a <- ma
      r <- execDyn return (put v)
      case r of
        Nothing | n > 0 -> reportAttemptFailure >> go (n - 1)
        Nothing -> reportFailureAndLogError >> return a
        Just _ -> return a
    version :: AWS.GetItemResponse -> Maybe Word32
    version v = conv =<< HashMap.lookup ddbVersion (view AWS.getItemResponse_item v)
      where
        conv :: AWS.AttributeValue -> Maybe Word32
        conv = \case
          AWS.N t -> readMaybe $ Text.unpack t
          _ -> Nothing
    get :: Text -> AWS.GetItem
    get t =
      AWS.newGetItem t & AWS.getItem_key .~ key u c
        & AWS.getItem_consistentRead ?~ True
    put :: Maybe Word32 -> Text -> AWS.PutItem
    put v t =
      AWS.newPutItem t & AWS.putItem_item .~ item v
        & AWS.putItem_expected ?~ check v
    check :: Maybe Word32 -> HashMap Text AWS.ExpectedAttributeValue
    check Nothing = HashMap.singleton ddbVersion $ AWS.newExpectedAttributeValue & AWS.expectedAttributeValue_comparisonOperator ?~ AWS.ComparisonOperator_NULL
    check (Just v) =
      HashMap.singleton ddbVersion $
        AWS.newExpectedAttributeValue & AWS.expectedAttributeValue_comparisonOperator ?~ AWS.ComparisonOperator_EQ
          & AWS.expectedAttributeValue_attributeValueList ?~ [toAttributeValue v]
    item :: Maybe Word32 -> HashMap Text AWS.AttributeValue
    item v =
      HashMap.insert ddbVersion (toAttributeValue (maybe (1 :: Word32) (+ 1) v)) $
        key u c
    toAttributeValue :: Word32 -> AWS.AttributeValue
    toAttributeValue w = AWS.N $ AWS.toText (fromIntegral w :: Int)
    reportAttemptFailure :: (AppIO effs) ()
    reportAttemptFailure =
      Metrics.counterIncr (Metrics.path "client.opt_lock.optimistic_lock_grab_attempt_failed") =<< view metrics
    reportFailureAndLogError :: (AppIO effs) ()
    reportFailureAndLogError = do
      Log.err $
        Log.field "user" (toByteString' u)
          . Log.field "client" (toByteString' c)
          . msg (val "PreKeys: Optimistic lock failed")
      Metrics.counterIncr (Metrics.path "client.opt_lock.optimistic_lock_failed") =<< view metrics
    execDyn :: forall r x. (AWS.AWSRequest r) => (AWS.AWSResponse r -> Maybe x) -> (Text -> r) -> (AppIO effs) (Maybe x)
    execDyn cnv mkCmd = do
      cmd <- mkCmd <$> view (awsEnv . prekeyTable)
      e <- view (awsEnv . amazonkaEnv)
      m <- view metrics
      liftIO $ execDyn' e m cnv cmd
      where
        execDyn' ::
          forall y p.
          AWS.AWSRequest p =>
          AWS.Env ->
          Metrics.Metrics ->
          (AWS.AWSResponse p -> Maybe y) ->
          p ->
          IO (Maybe y)
        execDyn' e m conv cmd = recovering policy handlers (const run)
          where
            run = execCatch e cmd >>= either handleErr (return . conv)
            handlers = httpHandlers ++ [const $ EL.handler_ AWS._ConditionalCheckFailedException (pure True)]
            policy = limitRetries 3 <> exponentialBackoff 100000
            handleErr (AWS.ServiceError se) | se ^. AWS.serviceCode == AWS.ErrorCode "ProvisionedThroughputExceeded" = do
              Metrics.counterIncr (Metrics.path "client.opt_lock.provisioned_throughput_exceeded") m
              return Nothing
            handleErr _ = return Nothing

withLocalLock :: MVar () -> (AppIO r) a -> (AppIO r) a
withLocalLock l ma = do
  (takeMVar l *> ma) `finally` putMVar l ()
