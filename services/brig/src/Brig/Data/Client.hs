{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

module Brig.Data.Client
    ( -- * Clients
      ClientDataError (..)
    , AuthError       (..)
    , ReAuthError     (..)
    , addClient
    , rmClient
    , hasClient
    , lookupClient
    , lookupClients
    , lookupClientIds
    , lookupUsersClientIds
    , Brig.Data.Client.updateClientLabel

      -- * Prekeys
    , claimPrekey
    , updatePrekeys
    , lookupPrekeyIds
    ) where

import Aws.Core (Transaction, ServiceConfiguration)
import Bilge.Retry (httpHandlers)
import Brig.App (AppIO, awsConfig, currentTime, amazonkaEnv)
import Brig.AwsAmazonka
import Brig.User.Auth.DB.Instances ()
import Brig.Data.Instances ()
import Brig.Data.User (AuthError (..), ReAuthError (..))
import Brig.Types
import Brig.Types.User.Auth (CookieLabel)
import Cassandra hiding (Client)
import Control.Concurrent.Async.Lifted.Safe (mapConcurrently)
import Control.Error
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Retry
import Data.ByteString.Conversion (toByteString, toByteString')
import Data.Foldable (for_)
import Data.Id
import Data.List.Split (chunksOf)
import Data.Misc
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock
import Data.Typeable
import Data.Word
import Network.AWS (AWSRequest, Rs, Region (..))
import Network.AWS.Data
import Safe (readMay)
import System.CryptoBox (Result (Success))
import System.Logger.Class (field, msg, val)

import qualified Aws.DynamoDb           as Ddb
import qualified Brig.Aws               as Aws
import qualified Brig.Data.User         as User
import qualified Control.Exception.Lens as EL
import qualified Data.ByteString.Base64 as B64
import qualified Data.HashMap.Strict    as Map
import qualified Data.Map.Lazy          as LazyMap
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import qualified Data.UUID              as UUID
import qualified Network.AWS            as Amazonka
import qualified Network.AWS.Data       as Amazonka
import qualified Network.AWS.DynamoDB   as AmazonkaDdb
import qualified Network.AWS.Env        as AWS
import qualified System.CryptoBox       as CryptoBox
import qualified System.Logger.Class    as Log

maxPermClients :: Int
maxPermClients = 7

data ClientDataError
    = TooManyClients
    | ClientReAuthError !ReAuthError
    | ClientMissingAuth
    | MalformedPrekeys

addClient :: (MonadClient m)
          => UserId
          -> ClientId
          -> NewClient a
          -> Maybe Location
          -> ExceptT ClientDataError m (Client, [Client], Word)
addClient u newId c loc = do
    clients <- lookupClients u
    let typed  = filter ((== newClientType c) . clientType) clients
    let count  = length typed
    let upsert = any exists typed
    unless (count == 0 || upsert) $
        fmapLT ClientReAuthError $
            User.reauthenticate u (newClientPassword c)
    let capacity = fmap (+(-count)) limit
    unless (maybe True (> 0) capacity || upsert) $
        throwE TooManyClients
    new <- insert
    let !total = fromIntegral (length clients + if upsert then 0 else 1)
    let old = maybe (filter (not . exists) typed) (const []) limit
    return (new, old, total)
  where
    limit = case newClientType c of
        PermanentClient -> Just maxPermClients
        TemporaryClient -> Nothing

    exists = (==) newId . clientId

    insert = do
        now <- liftIO getCurrentTime -- TODO: Is this a performance concern? liftIO =<< view currentTime
        let keys = unpackLastPrekey (newClientLastKey c) : newClientPrekeys c
        updatePrekeys u newId keys
        let lat = Latitude . view latitude <$> loc
            lon = Longitude . view longitude <$> loc
            mdl = newClientModel c
            prm = (u, newId, now, newClientType c, newClientLabel c, newClientClass c, newClientCookie c, lat, lon, mdl)
        retry x5 $ write insertClient (params Quorum prm)
        return $! Client newId (newClientType c) now (newClientClass c) (newClientLabel c) (newClientCookie c) loc mdl

lookupClient :: MonadClient m => UserId -> ClientId -> m (Maybe Client)
lookupClient u c = fmap toClient <$>
    retry x1 (query1 selectClient (params Quorum (u, c)))

lookupClients :: MonadClient m => UserId -> m [Client]
lookupClients u = map toClient <$>
    retry x1 (query selectClients (params Quorum (Identity u)))

lookupClientIds :: MonadClient m => UserId -> m [ClientId]
lookupClientIds u = map runIdentity <$>
    retry x1 (query selectClientIds (params Quorum (Identity u)))

lookupUsersClientIds :: MonadClient m => [UserId] -> m [(UserId, Set.Set ClientId)]
lookupUsersClientIds us = liftClient $ do
    -- Limit concurrency to 16 parallel queries
    clts <- mapM (mapConcurrently getClientIds) (chunksOf 16 us)
    return (concat clts)
  where
    getClientIds u = (u,) <$> fmap Set.fromList (lookupClientIds u)

lookupPrekeyIds :: MonadClient m => UserId -> ClientId -> m [PrekeyId]
lookupPrekeyIds u c = map runIdentity <$>
    retry x1 (query selectPrekeyIds (params Quorum (u, c)))

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
    plain  <- mapM (hoistEither . fmapL (const MalformedPrekeys) . B64.decode . toByteString' . prekeyKey) pks
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
            _         -> return False

claimPrekey :: UserId -> ClientId -> AppIO (Maybe ClientPrekey)
claimPrekey u c = withOptLock u c $ do
    prekey <- retry x1 $ query1 userPrekeys (params Quorum (u, c))
    case prekey of
        Just (i, k) -> do
            if i /= lastPrekeyId
                then retry x1 $ write removePrekey (params Quorum (u, c, i))
                else Log.warn $ field "user" (toByteString u)
                              . field "client" (toByteString c)
                              . msg (val "last resort prekey used")
            return $ Just (ClientPrekey c (Prekey i k))
        Nothing -> return Nothing

-------------------------------------------------------------------------------
-- Queries

insertClient :: PrepQuery W (UserId, ClientId, UTCTime, ClientType, Maybe Text, Maybe ClientClass, Maybe CookieLabel, Maybe Latitude, Maybe Longitude, Maybe Text) ()
insertClient = "INSERT INTO clients (user, client, tstamp, type, label, class, cookie, lat, lon, model) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

updateClientLabelQuery :: PrepQuery W (Maybe Text, UserId, ClientId) ()
updateClientLabelQuery = "UPDATE clients SET label = ? WHERE user = ? AND client = ?"

selectClientIds :: PrepQuery R (Identity UserId) (Identity ClientId)
selectClientIds = "SELECT client from clients where user = ?"

selectClients :: PrepQuery R (Identity UserId) (ClientId, ClientType, UTCTime, Maybe Text, Maybe ClientClass, Maybe CookieLabel, Maybe Latitude, Maybe Longitude, Maybe Text)
selectClients = "SELECT client, type, tstamp, label, class, cookie, lat, lon, model from clients where user = ?"

selectClient :: PrepQuery R (UserId, ClientId) (ClientId, ClientType, UTCTime, Maybe Text, Maybe ClientClass, Maybe CookieLabel, Maybe Latitude, Maybe Longitude, Maybe Text)
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

toClient :: (ClientId, ClientType, UTCTime, Maybe Text, Maybe ClientClass, Maybe CookieLabel, Maybe Latitude, Maybe Longitude, Maybe Text) -> Client
toClient (cid, cty, tme, lbl, cls, cok, lat, lon, mdl) = Client
    { clientId       = cid
    , clientType     = cty
    , clientTime     = tme
    , clientClass    = cls
    , clientLabel    = lbl
    , clientCookie   = cok
    , clientLocation = location <$> lat <*> lon
    , clientModel    = mdl
    }

-------------------------------------------------------------------------------
-- Best-effort optimistic locking for prekeys via DynamoDB

ddbClient :: Text
ddbClient = "client"

ddbVersion :: Text
ddbVersion = "version"

-- ddbKey' :: UserId -> ClientId -> Ddb.DValue
-- ddbKey' u c = Ddb.DString (UUID.toText (toUUID u) <> "." <> client c)

-- withOptLock' :: UserId -> ClientId -> AppIO a -> AppIO a
-- withOptLock' u c ma = go (10 :: Int)
--   where
--     go !n = do
--         v <- (version =<<) <$> exec get
--         a <- ma
--         r <- exec (put v)
--         case r of
--             Nothing | n > 0 -> go (n - 1)
--             Nothing         -> logFailure >> return a
--             Just _          -> return a

--     key = ddbKey u c

--     version = Ddb.girItem >=> LazyMap.lookup ddbVersion >=> Ddb.fromValue

--     get (Aws.PreKeyTable t) = (Ddb.getItem t (Ddb.hk ddbClient key))
--         { Ddb.giAttrs      = Nothing
--         , Ddb.giConsistent = True
--         , Ddb.giRetCons    = Ddb.RCNone
--         }

--     put v (Aws.PreKeyTable t) = (Ddb.putItem t (item v))
--         { Ddb.piExpect  = Ddb.Conditions Ddb.CondAnd [check v]
--         , Ddb.piReturn  = Ddb.URNone
--         , Ddb.piRetCons = Ddb.RCNone
--         , Ddb.piRetMet  = Ddb.RICMNone
--         }

--     item v = Ddb.item
--         [ Ddb.attr ddbClient key
--         , Ddb.attr ddbVersion (maybe (1 :: Word32) (+1) v)
--         ]

--     check (Just v) = Ddb.Condition ddbVersion (Ddb.DEq (Ddb.toValue v))
--     check Nothing  = Ddb.Condition ddbVersion Ddb.IsNull

--     logFailure = Log.err (msg (val "PreKeys: Optimistic lock failed"))

-- deleteOptLock' :: UserId -> ClientId -> AppIO ()
-- deleteOptLock' u c = void (exec delete)
--   where
--     delete (Aws.PreKeyTable t) = Ddb.deleteItem t (Ddb.hk ddbClient (ddbKey u c))

-- exec' :: (Transaction r a, ServiceConfiguration r ~ Ddb.DdbConfiguration)
--      => (Aws.PreKeyTable -> r)
--      -> AppIO (Maybe a)
-- exec' mkCmd = do
--     cmd <- mkCmd <$> view (error "old awsConfig.Aws.ddbPreKeyTable")
--     rs  <- Aws.tryDynamo cmd
--     case snd <$> rs of
--         Left (Aws.DynamoErrorResponse e)
--             | Ddb.ddbErrCode e == Ddb.ConditionalCheckFailedException
--             -> return Nothing
--         Left ex -> do
--           Log.err $ field "error" (show ex)
--                   . msg (val "PreKeys: DynamoDB error")
--           return Nothing
--         Right a -> return (Just a)

-- Amazonka

-- execAmazonka' :: (MonadMask m, Amazonka.MonadAWS m, Typeable m) => AWS.Env -> (Rs r -> a) -> r -> m a
-- execAmazonka' e conv cmd = recovering policy cond (const fn)
--   where
--     cond = httpHandlers ++ [const $ EL.handler_ AmazonkaDdb._ConditionalCheckFailedException (pure True)]
--     policy = limitRetries 100 <> exponentialBackoff 100000

--     fn = do
--         r <- execute (error "e") execAWSSafe cmd
--         return $ conv r

-- execAmazonka :: (AWSRequest r) => (Rs r -> a) -> (Text -> r) -> AppIO a
-- execAmazonka conv mkCmd = do
--     cmd <- mkCmd <$> view (amazonkaEnv.dynamoPrekeyTable)
--     e <- view (amazonkaEnv.amazonkaAwsEnv)
--     execAmazonka' e conv cmd

-- execAmazonka2' :: (AWSRequest r, MonadMask m, MonadIO m, Typeable m, MonadBaseControl IO m) => AWS.Env -> r -> m (Maybe a)
-- execAmazonka2' e cmd = recovering policy cond (const fn)
--   where
--     cond = httpHandlers ++ [const $ EL.handler_ AmazonkaDdb._ConditionalCheckFailedException (pure True)]
--     policy = limitRetries 100 <> exponentialBackoff 100000

--     fn = do
--         r <- toA <$> execAWS2 e cmd
--         case r of
--             Left _  -> return Nothing
--             Right x -> return x
    
--     toA :: Either Amazonka.Error (Rs r) -> Either Amazonka.Error (Maybe a)
--     toA = undefined

-- execAmazonka2 :: (AWSRequest r) => (Text -> r) -> AppIO (Maybe a)
-- execAmazonka2 mkCmd = do
--     cmd <- mkCmd <$> view (amazonkaEnv.dynamoPrekeyTable)
--     e <- view (amazonkaEnv.amazonkaAwsEnv)
--     execAmazonka2' e cmd

deleteOptLock :: UserId -> ClientId -> AppIO ()
deleteOptLock u c = do
    t <- view (amazonkaEnv.dynamoPrekeyTable)
    e <- view (amazonkaEnv.amazonkaAwsEnv)
    void $ execAWS e (AmazonkaDdb.deleteItem t & AmazonkaDdb.diKey .~ item)
  where
    item :: Map.HashMap Text AmazonkaDdb.AttributeValue
    item = Map.singleton ddbClient (ddbKey u c)

ddbKey :: UserId -> ClientId -> AmazonkaDdb.AttributeValue
ddbKey u c = AmazonkaDdb.attributeValue & AmazonkaDdb.avS .~ Just (UUID.toText (toUUID u) <> "." <> client c)

withOptLock :: UserId -> ClientId -> AppIO a -> AppIO a
withOptLock u c ma = go (10 :: Int)
  where
    go !n = do
        v <- (version =<<) <$> execAmazonka return get
        a <- ma
        r <- execAmazonka return (put v)
        case r of
            Nothing | n > 0 -> go (n - 1)
            Nothing         -> logFailure >> return a
            Just _          -> return a

    key :: Map.HashMap Text AmazonkaDdb.AttributeValue
    key = Map.singleton ddbClient (ddbKey u c)

    version :: AmazonkaDdb.GetItemResponse -> Maybe Word32
    version r = do
        let v = Map.lookup ddbVersion (view AmazonkaDdb.girsItem r)
        join (conv <$> v)
      where
        conv :: AmazonkaDdb.AttributeValue -> Maybe Word32
        conv = maybe Nothing (readMay . Text.unpack) . view AmazonkaDdb.avN

    get :: Text -> AmazonkaDdb.GetItem
    get t = AmazonkaDdb.getItem t & AmazonkaDdb.giKey .~ key
                                  & AmazonkaDdb.giConsistentRead ?~ True

    put :: Maybe Word32 -> Text -> AmazonkaDdb.PutItem
    put v t = AmazonkaDdb.putItem t & AmazonkaDdb.piItem .~ item v
                                    & AmazonkaDdb.piExpected .~ check v

    check :: Maybe Word32 -> Map.HashMap Text AmazonkaDdb.ExpectedAttributeValue
    check Nothing  = Map.singleton ddbVersion $ AmazonkaDdb.expectedAttributeValue & AmazonkaDdb.eavComparisonOperator ?~ AmazonkaDdb.Null
    check (Just v) = Map.singleton ddbVersion $ AmazonkaDdb.expectedAttributeValue & AmazonkaDdb.eavComparisonOperator ?~ AmazonkaDdb.EQ'
                                                                                   & AmazonkaDdb.eavAttributeValueList .~ [toAttributeValue v]

    item :: Maybe Word32 -> Map.HashMap Text AmazonkaDdb.AttributeValue
    item v = Map.insert ddbVersion (toAttributeValue (maybe (1 :: Word32) (+1) v))
           $ key

    toAttributeValue :: Word32 -> AmazonkaDdb.AttributeValue
    toAttributeValue w = AmazonkaDdb.attributeValue & AmazonkaDdb.avN ?~ toText (fromIntegral w :: Int)

    logFailure :: AppIO ()
    logFailure = Log.err (msg (val "PreKeys: Optimistic lock failed"))

execAmazonka :: (AWSRequest r) => (Rs r -> Maybe a) -> (Text -> r) -> AppIO (Maybe a)
execAmazonka conv mkCmd = do
    cmd <- mkCmd <$> view (amazonkaEnv.dynamoPrekeyTable)
    e <- view (amazonkaEnv.amazonkaAwsEnv)
    execAmazonkaAux e conv cmd
  where
    execAmazonkaAux :: (AWSRequest r, MonadMask m, MonadIO m, Typeable m, MonadBaseControl IO m) => AWS.Env -> (Rs r -> Maybe a) -> r -> m (Maybe a)
    execAmazonkaAux e conv cmd = recovering policy cond (const fn)
      where
        cond = httpHandlers ++ [const $ EL.handler_ AmazonkaDdb._ConditionalCheckFailedException (pure True)]
        policy = limitRetries 3 <> exponentialBackoff 100000

        fn = do
            r <- execAWS2 e cmd
            case r of
                Left _  -> return Nothing
                Right x -> return (conv x)
