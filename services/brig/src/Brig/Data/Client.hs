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
    lookupClientIds,
    lookupUsersClientIds,
    Brig.Data.Client.updateClientLabel,

    -- * Prekeys
    claimPrekey,
    updatePrekeys,
    lookupPrekeyIds,
  )
where

import Bilge.Retry (httpHandlers)
import Brig.AWS
import Brig.App (AppIO, awsEnv, currentTime, metrics)
import Brig.Data.Instances ()
import Brig.Data.User (AuthError (..), ReAuthError (..))
import qualified Brig.Data.User as User
import Brig.Types
import Brig.Types.Instances ()
import Brig.Types.User.Auth (CookieLabel)
import Brig.User.Auth.DB.Instances ()
import Cassandra hiding (Client)
import Control.Error
import qualified Control.Exception.Lens as EL
import Control.Lens
import Control.Monad.Catch
import Control.Retry
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Conversion (toByteString, toByteString')
import qualified Data.HashMap.Strict as Map
import Data.Id
import Data.Json.Util (UTCTimeMillis, toUTCTimeMillis)
import Data.List.Split (chunksOf)
import qualified Data.Metrics as Metrics
import Data.Misc
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import Imports
import qualified Network.AWS as AWS
import qualified Network.AWS.Data as AWS
import qualified Network.AWS.DynamoDB as AWS
import System.CryptoBox (Result (Success))
import qualified System.CryptoBox as CryptoBox
import System.Logger.Class (field, msg, val)
import qualified System.Logger.Class as Log
import UnliftIO (mapConcurrently)

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
  ExceptT ClientDataError AppIO (Client, [Client], Word)
addClient u newId c maxPermClients loc = do
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
    limit = case newClientType c of
      PermanentClientType -> Just maxPermClients
      TemporaryClientType -> Nothing
      LegalHoldClientType -> Nothing
    exists = (==) newId . clientId
    insert = do
      -- Is it possible to do this somewhere else? Otherwise we could use `MonadClient` instead
      now <- toUTCTimeMillis <$> (liftIO =<< view currentTime)
      let keys = unpackLastPrekey (newClientLastKey c) : newClientPrekeys c
      updatePrekeys u newId keys
      let lat = Latitude . view latitude <$> loc
          lon = Longitude . view longitude <$> loc
          mdl = newClientModel c
          prm = (u, newId, now, newClientType c, newClientLabel c, newClientClass c, newClientCookie c, lat, lon, mdl)
      retry x5 $ write insertClient (params Quorum prm)
      return $! Client newId (newClientType c) now (newClientClass c) (newClientLabel c) (newClientCookie c) loc mdl

lookupClient :: MonadClient m => UserId -> ClientId -> m (Maybe Client)
lookupClient u c =
  fmap toClient
    <$> retry x1 (query1 selectClient (params Quorum (u, c)))

lookupClients :: MonadClient m => UserId -> m [Client]
lookupClients u =
  map toClient
    <$> retry x1 (query selectClients (params Quorum (Identity u)))

lookupClientIds :: MonadClient m => UserId -> m [ClientId]
lookupClientIds u =
  map runIdentity
    <$> retry x1 (query selectClientIds (params Quorum (Identity u)))

lookupUsersClientIds :: MonadClient m => [UserId] -> m [(UserId, Set.Set ClientId)]
lookupUsersClientIds us = liftClient $ do
  -- Limit concurrency to 16 parallel queries
  clts <- mapM (mapConcurrently getClientIds) (chunksOf 16 us)
  return (concat clts)
  where
    getClientIds u = (u,) <$> fmap Set.fromList (lookupClientIds u)

lookupPrekeyIds :: MonadClient m => UserId -> ClientId -> m [PrekeyId]
lookupPrekeyIds u c =
  map runIdentity
    <$> retry x1 (query selectPrekeyIds (params Quorum (u, c)))

hasClient :: MonadClient m => UserId -> ClientId -> m Bool
hasClient u d = isJust <$> retry x1 (query1 checkClient (params Quorum (u, d)))

rmClient :: UserId -> ClientId -> AppIO ()
rmClient u c = do
  retry x5 $ write removeClient (params Quorum (u, c))
  retry x5 $ write removeClientKeys (params Quorum (u, c))
  deleteOptLock u c

updateClientLabel :: MonadClient m => UserId -> ClientId -> Maybe Text -> m ()
updateClientLabel u c l = retry x5 $ write updateClientLabelQuery (params Quorum (l, u, c))

updatePrekeys :: MonadClient m => UserId -> ClientId -> [Prekey] -> ExceptT ClientDataError m ()
updatePrekeys u c pks = do
  plain <- mapM (hoistEither . fmapL (const MalformedPrekeys) . B64.decode . toByteString' . prekeyKey) pks
  binary <- liftIO $ zipWithM check pks plain
  unless (and binary) $
    throwE MalformedPrekeys
  for_ pks $ \k -> do
    let args = (u, c, prekeyId k, prekeyKey k)
    retry x5 $ write insertClientKey (params Quorum args)
  where
    check a b = do
      i <- CryptoBox.isPrekey b
      case i of
        Success n -> return (CryptoBox.prekeyId n == keyId (prekeyId a))
        _ -> return False

claimPrekey :: UserId -> ClientId -> AppIO (Maybe ClientPrekey)
claimPrekey u c = withOptLock u c $ do
  prekey <- retry x1 $ query1 userPrekeys (params Quorum (u, c))
  case prekey of
    Just (i, k) -> do
      if i /= lastPrekeyId
        then retry x1 $ write removePrekey (params Quorum (u, c, i))
        else
          Log.debug $
            field "user" (toByteString u)
              . field "client" (toByteString c)
              . msg (val "last resort prekey used")
      return $ Just (ClientPrekey c (Prekey i k))
    Nothing -> return Nothing

-------------------------------------------------------------------------------
-- Queries

insertClient :: PrepQuery W (UserId, ClientId, UTCTimeMillis, ClientType, Maybe Text, Maybe ClientClass, Maybe CookieLabel, Maybe Latitude, Maybe Longitude, Maybe Text) ()
insertClient = "INSERT INTO clients (user, client, tstamp, type, label, class, cookie, lat, lon, model) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

updateClientLabelQuery :: PrepQuery W (Maybe Text, UserId, ClientId) ()
updateClientLabelQuery = "UPDATE clients SET label = ? WHERE user = ? AND client = ?"

selectClientIds :: PrepQuery R (Identity UserId) (Identity ClientId)
selectClientIds = "SELECT client from clients where user = ?"

selectClients :: PrepQuery R (Identity UserId) (ClientId, ClientType, UTCTimeMillis, Maybe Text, Maybe ClientClass, Maybe CookieLabel, Maybe Latitude, Maybe Longitude, Maybe Text)
selectClients = "SELECT client, type, tstamp, label, class, cookie, lat, lon, model from clients where user = ?"

selectClient :: PrepQuery R (UserId, ClientId) (ClientId, ClientType, UTCTimeMillis, Maybe Text, Maybe ClientClass, Maybe CookieLabel, Maybe Latitude, Maybe Longitude, Maybe Text)
selectClient = "SELECT client, type, tstamp, label, class, cookie, lat, lon, model from clients where user = ? and client = ?"

insertClientKey :: PrepQuery W (UserId, ClientId, PrekeyId, Text) ()
insertClientKey = "INSERT INTO prekeys (user, client, key, data) VALUES (?, ?, ?, ?)"

removeClient :: PrepQuery W (UserId, ClientId) ()
removeClient = "DELETE FROM clients where user = ? and client = ?"

removeClientKeys :: PrepQuery W (UserId, ClientId) ()
removeClientKeys = "DELETE FROM prekeys where user = ? and client = ?"

userPrekeys :: PrepQuery R (UserId, ClientId) (PrekeyId, Text)
userPrekeys = "SELECT key, data FROM prekeys where user = ? and client = ? LIMIT 1"

selectPrekeyIds :: PrepQuery R (UserId, ClientId) (Identity PrekeyId)
selectPrekeyIds = "SELECT key FROM prekeys where user = ? and client = ?"

removePrekey :: PrepQuery W (UserId, ClientId, PrekeyId) ()
removePrekey = "DELETE FROM prekeys where user = ? and client = ? and key = ?"

checkClient :: PrepQuery R (UserId, ClientId) (Identity ClientId)
checkClient = "SELECT client from clients where user = ? and client = ?"

-------------------------------------------------------------------------------
-- Conversions

toClient :: (ClientId, ClientType, UTCTimeMillis, Maybe Text, Maybe ClientClass, Maybe CookieLabel, Maybe Latitude, Maybe Longitude, Maybe Text) -> Client
toClient (cid, cty, tme, lbl, cls, cok, lat, lon, mdl) =
  Client
    { clientId = cid,
      clientType = cty,
      clientTime = tme,
      clientClass = cls,
      clientLabel = lbl,
      clientCookie = cok,
      clientLocation = location <$> lat <*> lon,
      clientModel = mdl
    }

-------------------------------------------------------------------------------
-- Best-effort optimistic locking for prekeys via DynamoDB

ddbClient :: Text
ddbClient = "client"

ddbVersion :: Text
ddbVersion = "version"

ddbKey :: UserId -> ClientId -> AWS.AttributeValue
ddbKey u c = AWS.attributeValue & AWS.avS ?~ UUID.toText (toUUID u) <> "." <> client c

key :: UserId -> ClientId -> Map.HashMap Text AWS.AttributeValue
key u c = Map.singleton ddbClient (ddbKey u c)

deleteOptLock :: UserId -> ClientId -> AppIO ()
deleteOptLock u c = do
  t <- view (awsEnv . prekeyTable)
  e <- view (awsEnv . amazonkaEnv)
  void $ exec e (AWS.deleteItem t & AWS.diKey .~ (key u c))

withOptLock :: UserId -> ClientId -> AppIO a -> AppIO a
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
    version v = conv =<< Map.lookup ddbVersion (view AWS.girsItem v)
      where
        conv :: AWS.AttributeValue -> Maybe Word32
        conv = readMaybe . Text.unpack <=< view AWS.avN
    get :: Text -> AWS.GetItem
    get t =
      AWS.getItem t & AWS.giKey .~ (key u c)
        & AWS.giConsistentRead ?~ True
    put :: Maybe Word32 -> Text -> AWS.PutItem
    put v t =
      AWS.putItem t & AWS.piItem .~ item v
        & AWS.piExpected .~ check v
    check :: Maybe Word32 -> Map.HashMap Text AWS.ExpectedAttributeValue
    check Nothing = Map.singleton ddbVersion $ AWS.expectedAttributeValue & AWS.eavComparisonOperator ?~ AWS.Null
    check (Just v) =
      Map.singleton ddbVersion $
        AWS.expectedAttributeValue & AWS.eavComparisonOperator ?~ AWS.EQ'
          & AWS.eavAttributeValueList .~ [toAttributeValue v]
    item :: Maybe Word32 -> Map.HashMap Text AWS.AttributeValue
    item v =
      Map.insert ddbVersion (toAttributeValue (maybe (1 :: Word32) (+ 1) v)) $
        key u c
    toAttributeValue :: Word32 -> AWS.AttributeValue
    toAttributeValue w = AWS.attributeValue & AWS.avN ?~ AWS.toText (fromIntegral w :: Int)
    reportAttemptFailure :: AppIO ()
    reportAttemptFailure =
      Metrics.counterIncr (Metrics.path "client.opt_lock.optimistic_lock_grab_attempt_failed") =<< view metrics
    reportFailureAndLogError :: AppIO ()
    reportFailureAndLogError = do
      Log.err $
        Log.field "user" (toByteString' u)
          . Log.field "client" (toByteString' c)
          . msg (val "PreKeys: Optimistic lock failed")
      Metrics.counterIncr (Metrics.path "client.opt_lock.optimistic_lock_failed") =<< view metrics
    execDyn :: (AWS.AWSRequest r) => (AWS.Rs r -> Maybe a) -> (Text -> r) -> AppIO (Maybe a)
    execDyn cnv mkCmd = do
      cmd <- mkCmd <$> view (awsEnv . prekeyTable)
      e <- view (awsEnv . amazonkaEnv)
      m <- view metrics
      execDyn' e m cnv cmd
      where
        execDyn' ::
          (AWS.AWSRequest r, MonadUnliftIO m, MonadMask m, MonadIO m, Typeable m) =>
          AWS.Env ->
          Metrics.Metrics ->
          (AWS.Rs r -> Maybe a) ->
          r ->
          m (Maybe a)
        execDyn' e m conv cmd = recovering policy handlers (const run)
          where
            run = execCatch e cmd >>= either handleErr (return . conv)
            handlers = httpHandlers ++ [const $ EL.handler_ AWS._ConditionalCheckFailedException (pure True)]
            policy = limitRetries 3 <> exponentialBackoff 100000
            handleErr (AWS.ServiceError se) | se ^. AWS.serviceCode == AWS.ErrorCode "ProvisionedThroughputExceeded" = do
              Metrics.counterIncr (Metrics.path "client.opt_lock.provisioned_throughput_exceeded") m
              return Nothing
            handleErr _ = return Nothing
