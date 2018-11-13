{-# LANGUAGE OverloadedStrings #-}

-- TODO: Move to Brig.User.Client
module Brig.API.Client
    ( -- * Clients
      addClient
    , updateClient
    , rmClient
    , pubClient
    , Data.lookupClient
    , Data.lookupClients
    , Data.lookupPrekeyIds
    , Data.lookupUsersClientIds

      -- * Prekeys
    , claimPrekey
    , claimPrekeyBundle
    , claimMultiPrekeyBundles
    , Data.lookupClientIds
    ) where

import Imports
import Brig.App
import Brig.API.Types
import Brig.Types
import Brig.Types.Intra
import Brig.User.Email
import Brig.User.Event
import Control.Concurrent.Async (mapConcurrently)
import Control.Error
import Data.ByteString.Conversion
import Data.Hashable (hash)
import Data.Id (UserId, ClientId, newClientId, ConnId)
import Data.IP (IP)
import Data.List.Split (chunksOf)
import Data.Misc (PlainTextPassword (..))
import Galley.Types (UserClients (..), UserClientMap (..))
import Gundeck.Types.Push.V2 (SignalingKeys)
import Network.Wai.Utilities
import System.Logger.Class (msg, val, field, (~~))

import qualified Brig.Data.Client    as Data
import qualified Brig.Data.User      as Data
import qualified Brig.IO.Intra       as Intra
import qualified Data.Map.Strict     as Map
import qualified System.Logger.Class as Log

import qualified Brig.User.Auth.Cookie as Auth

-- nb. We must ensure that the set of clients known to brig is always
-- a superset of the clients known to galley.
addClient :: UserId -> ConnId -> Maybe IP -> NewClient SignalingKeys -> ExceptT ClientError AppIO Client
addClient u con ip new = do
    acc <- lift (Data.lookupAccount u) >>= maybe (throwE (ClientUserNotFound u)) return
    loc <- maybe (return Nothing) locationOf ip
    (clt, old, count) <- Data.addClient u newId new loc !>> ClientDataError
    let usr = accountUser acc
    lift $ do
        for_ old $ execDelete u (Just con)
        Intra.newClient u (clientId clt) (newClientSigKeys new)
        Intra.onClientEvent u (Just con) (ClientAdded u clt)
        when (count > 1) $
            for_ (userEmail usr) $ \email ->
                sendNewClientEmail (userName usr) email clt (userLocale usr)
    return clt
  where
    newId = let prekey = unpackLastPrekey (newClientLastKey new)
                hashCode = hash (prekeyKey prekey)
            in newClientId (fromIntegral hashCode)

updateClient :: UserId -> ClientId -> UpdateClient SignalingKeys -> ExceptT ClientError AppIO ()
updateClient u c r = do
    ok <- lift $ Data.hasClient u c
    unless ok $
        throwE ClientNotFound
    for_ (updateClientLabel r) $ lift . Data.updateClientLabel u c . Just
    let lk = maybeToList (unpackLastPrekey <$> updateClientLastKey r)
    Data.updatePrekeys u c (lk ++ updateClientPrekeys r) !>> ClientDataError
    for_ (updateClientSigKeys r) $ lift . Intra.updateSignalingKeys u c

-- nb. We must ensure that the set of clients known to brig is always
-- a superset of the clients known to galley.
rmClient :: UserId -> ConnId -> ClientId -> Maybe PlainTextPassword -> ExceptT ClientError AppIO ()
rmClient u con clt pw =
    maybe (throwE ClientNotFound) fn =<< lift (Data.lookupClient u clt)
  where
    fn client = do
        unless (clientType client == TemporaryClient) $
            Data.reauthenticate u pw !>> ClientDataError . ClientReAuthError
        lift $ execDelete u (Just con) client

claimPrekey :: UserId -> ClientId -> AppIO (Maybe ClientPrekey)
claimPrekey u c = do
    prekey <- Data.claimPrekey u c
    case prekey of
        Nothing     -> noPrekeys u c >> return Nothing
        pk@(Just _) -> return pk

claimPrekeyBundle :: UserId -> AppIO PrekeyBundle
claimPrekeyBundle u = do
    clients <- map clientId <$> Data.lookupClients u
    PrekeyBundle u . catMaybes <$> mapM (Data.claimPrekey u) clients

claimMultiPrekeyBundles :: UserClients -> AppIO (UserClientMap (Maybe Prekey))
claimMultiPrekeyBundles (UserClients x) = do
    e <- ask
    m <- liftIO $ forM chunks (mapConcurrently $ runAppT e . outer)
    return $ UserClientMap (Map.fromList (concat m))
  where
    chunks = chunksOf 16 (Map.toList x)

    outer (u, c) = do
        keymap <- foldrM (inner u) Map.empty c
        return (u, keymap)

    inner u c m = do
        key <- fmap prekeyData <$> Data.claimPrekey u c
        when (isNothing key) $ noPrekeys u c
        return (Map.insert c key m)

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
    Log.info $ field "user" (toByteString u)
        ~~ field "client" (toByteString c)
        ~~ msg (val "No prekey found. Ensuring client does not exist.")
    Intra.rmClient u c
    client <- Data.lookupClient u c
    for_ client $ \_ ->
        Log.err $ field "user" (toByteString u)
            ~~ field "client" (toByteString c)
            ~~ msg (val "Client exists without prekeys.")

pubClient :: Client -> PubClient
pubClient c = PubClient
    { pubClientId    = clientId c
    , pubClientClass = clientClass c
    }
