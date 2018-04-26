{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections       #-}

module API.User.Client (tests) where

import API.User.Util
import Bilge hiding (accept, timeout)
import Bilge.Assert
import Brig.Types
import Brig.Types.User.Auth hiding (user)
import Control.Concurrent.Async.Lifted.Safe (mapConcurrently)
import Control.Lens ((^?), preview)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Conversion
import Data.Function (on)
import Data.Id hiding (client)
import Data.List (sort, sortBy, nub)
import Data.Maybe
import Gundeck.Types.Notification
import Gundeck.Types.Push.V2
import Test.Tasty hiding (Timeout)
import Test.Tasty.Cannon hiding (Cannon)
import Test.Tasty.HUnit
import Util

import qualified Brig.AWS                    as AWS
import qualified Brig.Options                as Opt
import qualified Data.List1                  as List1
import qualified Data.Vector                 as Vec
import qualified Network.Wai.Utilities.Error as Error
import qualified Test.Tasty.Cannon           as WS

tests :: ConnectionLimit -> Opt.Timeout -> Maybe Opt.Opts -> Manager -> Brig -> Cannon -> Galley -> Maybe AWS.Env -> TestTree
tests _cl _at _conf p b c g _localAWS = testGroup "client"
    [ test p "get /users/:user/prekeys - 200"         $ testGetUserPrekeys b
    , test p "get /users/:user/prekeys/:client - 200" $ testGetClientPrekey b
    , test p "post /clients - 201"                    $ testAddGetClient b c
    , test p "post /clients - 403"                    $ testClientReauthentication b
    , test p "get /clients - 200"                     $ testListClients b
    , test p "get /clients/:client/prekeys - 200"     $ testListPrekeyIds b
    , test p "post /clients - 400"                    $ testTooManyClients b
    , test p "delete /clients/:client - 200"          $ testRemoveClient b c
    , test p "put /clients/:client - 200"             $ testUpdateClient b
    , test p "post /clients - 200 multiple temporary" $ testAddMultipleTemporary b g
    , test p "client/prekeys/race"                    $ testPreKeyRace b
    ]

testAddGetClient :: Brig -> Cannon -> Http ()
testAddGetClient brig cannon = do
    u <- randomUser brig
    let rq = addClientReq brig u (defNewClient TemporaryClient [somePrekeys !! 0] (someLastPrekeys !! 0))
           . header "X-Forwarded-For" "127.0.0.1" -- Fake IP to test IpAddr parsing.
    c <- WS.bracketR cannon (userId u) $ \ws -> do
        Just c <- decodeBody <$> (post rq <!! do
            const 201  === statusCode
            const True === isJust . getHeader "Location")
        void . liftIO $ WS.assertMatch (5 # Second) ws $ \n -> do
            let j = Object $ List1.head (ntfPayload n)
            let etype = j ^? key "type" . _String
            let eclient = j ^? key "client"
            etype @?= Just "user.client-add"
            fmap fromJSON eclient @?= Just (Success c)
        return c
    getClient brig (userId u) (clientId c) !!! do
        const 200      === statusCode
        const (Just c) === decodeBody

testClientReauthentication :: Brig -> Http ()
testClientReauthentication brig = do
    let (pk1, lk1) = (somePrekeys !! 0, someLastPrekeys !! 0)
    let (pk2, lk2) = (somePrekeys !! 1, someLastPrekeys !! 1)
    let (pk3, lk3) = (somePrekeys !! 2, someLastPrekeys !! 2)

    let payload1 = (defNewClient PermanentClient [pk1] lk1)
                 { newClientPassword = Nothing }
    let payload2 = (defNewClient PermanentClient [pk2] lk2)
                 { newClientPassword = Nothing }
    let payload3 = (defNewClient TemporaryClient [pk3] lk3)
                 { newClientPassword = Nothing }

    -- User with password
    u <- randomUser brig
    -- The first client never requires authentication
    Just c <- decodeBody <$> (addClient brig u payload1 <!! const 201 === statusCode)
    -- Adding a second client requires reauthentication, if a password is set.
    addClient brig u payload2 !!! do
        const 403 === statusCode
        const (Just "missing-auth") === (fmap Error.label . decodeBody)
    -- Removing a client requires reauthentication, if a password is set.
    deleteClient brig (userId u) (clientId c) Nothing !!! const 403 === statusCode

    -- User without a password
    u2 <- createAnonUser "Mr. X" brig
    Just c2 <- decodeBody <$> (addClient brig u2 payload1 <!! const 201 === statusCode)
    Just c3 <- decodeBody <$> (addClient brig u2 payload2 <!! const 201 === statusCode)
    deleteClient brig (userId u2) (clientId c2) Nothing !!! const 200 === statusCode
    deleteClient brig (userId u2) (clientId c3) Nothing !!! const 200 === statusCode

    -- Temporary client can always be deleted without a password
    Just c4 <- decodeBody <$> addClient brig u payload3
    deleteClient brig (userId u) (clientId c4) Nothing !!! const 200 === statusCode
    Just c5 <- decodeBody <$> addClient brig u2 payload3
    deleteClient brig (userId u2) (clientId c5) Nothing !!! const 200 === statusCode

testListClients :: Brig -> Http ()
testListClients brig = do
    u  <- randomUser brig
    let (pk1, lk1) = (somePrekeys !! 0, (someLastPrekeys !! 0))
    let (pk2, lk2) = (somePrekeys !! 1, (someLastPrekeys !! 1))
    let (pk3, lk3) = (somePrekeys !! 2, (someLastPrekeys !! 2))
    c1 <- decodeBody <$> addClient brig u (defNewClient PermanentClient [pk1] lk1)
    c2 <- decodeBody <$> addClient brig u (defNewClient PermanentClient [pk2] lk2)
    c3 <- decodeBody <$> addClient brig u (defNewClient TemporaryClient [pk3] lk3)
    let cs = sortBy (compare `on` clientId) $ catMaybes [c1, c2, c3]
    get ( brig
        . path "clients"
        . zUser (userId u)
        ) !!! do
            const 200       === statusCode
            const (Just cs) === decodeBody

testListPrekeyIds :: Brig -> Http ()
testListPrekeyIds brig = do
    u <- randomUser brig
    let new = defNewClient PermanentClient [somePrekeys !! 0] (someLastPrekeys !! 0)
    Just c <- decodeBody <$> addClient brig u new
    let pks = [PrekeyId 1, lastPrekeyId]
    get ( brig
        . paths ["clients", toByteString' (clientId c), "prekeys"]
        . zUser (userId u)
        ) !!! do
            const 200        === statusCode
            const (Just pks) === fmap sort . decodeBody

testGetUserPrekeys :: Brig -> Http ()
testGetUserPrekeys brig = do
    u <- randomUser brig
    let new = defNewClient TemporaryClient [somePrekeys !! 0] (someLastPrekeys !! 0)
    Just c <- decodeBody <$> addClient brig u new
    let cpk = ClientPrekey (clientId c) (somePrekeys !! 0)
    get (brig . paths ["users", toByteString' (userId u), "prekeys"]) !!! do
        const 200 === statusCode
        const (Just $ PrekeyBundle (userId u) [cpk]) === decodeBody

    -- prekeys are deleted when retrieved, except the last one
    let lpk = ClientPrekey (clientId c) (unpackLastPrekey (someLastPrekeys !! 0))
    replicateM_ 2 $ get (brig . paths ["users", toByteString' (userId u), "prekeys"]) !!! do
        const 200 === statusCode
        const (Just $ PrekeyBundle (userId u) [lpk]) === decodeBody

testGetClientPrekey :: Brig -> Http ()
testGetClientPrekey brig = do
    u <- randomUser brig
    let new = defNewClient TemporaryClient [somePrekeys !! 0] (someLastPrekeys !! 0)
    Just c <- decodeBody <$> addClient brig u new
    get (brig . paths ["users", toByteString' (userId u), "prekeys", toByteString' (clientId c)]) !!! do
        const 200 === statusCode
        const (Just $ ClientPrekey (clientId c) (somePrekeys !! 0)) === decodeBody

testTooManyClients :: Brig -> Http ()
testTooManyClients brig = do
    u <- randomUser brig

    -- There is only one temporary client, adding a new one
    -- replaces the previous one.
    forM_ [0..(9 :: Int)] $ \i ->
        let pk = somePrekeys !! i
            lk = someLastPrekeys !! i
        in addClient brig u (defNewClient TemporaryClient [pk] lk) !!! const 201 === statusCode

    -- But there can be only up to 7 permanent clients
    forM_ [10..(16 :: Int)] $ \i ->
        let pk = somePrekeys !! i
            lk = someLastPrekeys !! i
        in addClient brig u (defNewClient PermanentClient [pk] lk) !!! const 201 === statusCode

    addClient brig u (defNewClient PermanentClient [somePrekeys !! 17] (someLastPrekeys !! 17)) !!! do
        const 403 === statusCode
        const (Just "too-many-clients") === fmap Error.label . decodeBody

testRemoveClient :: Brig -> Cannon -> Http ()
testRemoveClient brig cannon = do
    u <- randomUser brig
    let Just email = userEmail u

    -- Permanent client with attached cookie
    login brig (defEmailLogin email) PersistentCookie
        !!! const 200 === statusCode
    numCookies <- countCookies brig (userId u) defCookieLabel
    liftIO $ Just 1 @=? numCookies
    Just c <- decodeBody <$> addClient brig u (client PermanentClient (someLastPrekeys !! 10))
    -- Missing password
    deleteClient brig (userId u) (clientId c) Nothing !!! const 403 === statusCode

    -- Success
    WS.bracketR cannon (userId u) $ \ws -> do
        deleteClient brig (userId u) (clientId c) (Just defPassword)
            !!! const 200 === statusCode
        void . liftIO $ WS.assertMatch (5 # Second) ws $ \n -> do
            let j = Object $ List1.head (ntfPayload n)
            let etype = j ^? key "type" . _String
            let eclient = j ^? key "client" . key "id" . _String
            etype @?= Just "user.client-remove"
            fmap ClientId eclient @?= Just (clientId c)

    -- Not found on retry
    deleteClient brig (userId u) (clientId c) Nothing !!! const 404 === statusCode

    -- Prekeys are gone
    getPreKey brig (userId u) (clientId c) !!! const 404 === statusCode

    -- Cookies are gone
    numCookies' <- countCookies brig (userId u) defCookieLabel
    liftIO $ Just 0 @=? numCookies'
  where
    client ty lk = (defNewClient ty [somePrekeys !! 0] lk)
        { newClientLabel  = Just "Nexus 5x"
        , newClientCookie = Just defCookieLabel
        }

testUpdateClient :: Brig -> Http ()
testUpdateClient brig = do
    u <- randomUser brig
    let clt = (defNewClient TemporaryClient [somePrekeys !! 0] (someLastPrekeys !! 0))
                { newClientClass = Just PhoneClient
                , newClientModel = Just "featurephone"
                }
    Just c <- decodeBody <$> addClient brig u clt
    get (brig . paths ["users", toByteString' (userId u), "prekeys", toByteString' (clientId c)]) !!! do
        const 200 === statusCode
        const (Just $ ClientPrekey (clientId c) (somePrekeys !! 0)) === decodeBody

    getClient brig (userId u) (clientId c) !!! do
        const 200                   === statusCode
        const (Just "Test Device")  === (clientLabel <=< decodeBody)
        const (Just PhoneClient)    === (clientClass <=< decodeBody)
        const (Just "featurephone") === (clientModel <=< decodeBody)

    let newPrekey = somePrekeys !! 2
    let update    = UpdateClient [newPrekey] Nothing (Nothing :: Maybe SignalingKeys) (Just "label")

    put ( brig
        . paths ["clients", toByteString' (clientId c)]
        . zUser (userId u)
        . contentJson
        . body (RequestBodyLBS $ encode update)
        ) !!! const 200 === statusCode

    get (brig . paths ["users", toByteString' (userId u), "prekeys", toByteString' (clientId c)]) !!! do
        const 200 === statusCode
        const (Just $ ClientPrekey (clientId c) newPrekey) === decodeBody

    -- check if label has been updated
    getClient brig (userId u) (clientId c) !!! do
        const 200            === statusCode
        const (Just "label") === (clientLabel <=< decodeBody)

    -- via `/users/:user/clients/:client`, only `id` and `class` are visible:
    get (brig . paths ["users", toByteString' (userId u), "clients", toByteString' (clientId c)]) !!! do
        const 200                 === statusCode
        const (Just $ clientId c) === (fmap pubClientId . decodeBody)
        const (Just PhoneClient)  === (pubClientClass <=< decodeBody)
        const Nothing             === (preview (key "label") <=< asValue)

    let update' = UpdateClient [] Nothing (Nothing :: Maybe SignalingKeys) Nothing

    -- empty update should be a no-op
    put ( brig
        . paths ["clients", toByteString' (clientId c)]
        . zUser (userId u)
        . contentJson
        . body (RequestBodyLBS $ encode update')
        ) !!! const 200 === statusCode

    -- check if label is still present
    getClient brig (userId u) (clientId c) !!! do
        const 200            === statusCode
        const (Just "label") === (clientLabel <=< decodeBody)

-- Legacy (galley)
testAddMultipleTemporary :: Brig -> Galley -> Http ()
testAddMultipleTemporary brig galley = do
    u <- randomUser brig

    let clt1 = (defNewClient TemporaryClient [somePrekeys !! 0] (someLastPrekeys !! 0))
                { newClientClass = Just PhoneClient
                , newClientModel = Just "featurephone1"
                }
    _ <- addClient brig u clt1
    brigClients1   <- numOfBrigClients (userId u)
    galleyClients1 <- numOfGalleyClients (userId u)
    liftIO $ assertEqual "Too many clients found" (Just 1) brigClients1
    liftIO $ assertEqual "Too many clients found" (Just 1) galleyClients1

    let clt2 = (defNewClient TemporaryClient [somePrekeys !! 1] (someLastPrekeys !! 1))
                { newClientClass = Just PhoneClient
                , newClientModel = Just "featurephone2"
                }
    _ <- addClient brig u clt2

    brigClients2   <- numOfBrigClients (userId u)
    galleyClients2 <- numOfGalleyClients (userId u)
    liftIO $ assertEqual "Too many clients found" (Just 1) brigClients2
    liftIO $ assertEqual "Too many clients found" (Just 1) galleyClients2
  where
    numOfBrigClients u = do
        r <- get $ brig
                 . path "clients"
                 . zUser u
        return $ Vec.length <$> (preview _Array =<< asValue r)

    numOfGalleyClients u = do
        r <- get $ galley
                 . path "i/test/clients"
                 . zUser u
        return $ Vec.length <$> (preview _Array =<< asValue r)

testPreKeyRace :: Brig -> Http ()
testPreKeyRace brig = do
    u <- randomUser brig
    let pks = map (\i -> somePrekeys !! i) [1..10]
    Just c <- decodeBody <$> addClient brig u (defNewClient PermanentClient pks (someLastPrekeys !! 0))
    pks' <- flip mapConcurrently pks $ \_ -> do
        rs <- getPreKey brig (userId u) (clientId c) <!! const 200 === statusCode
        return $ prekeyId . prekeyData <$> decodeBody rs
    -- We should not hand out regular prekeys more than once (i.e. at most once).
    let actual = catMaybes pks'
    liftIO $ assertEqual "insufficient prekeys" (length pks) (length actual)
    let regular = filter (/= lastPrekeyId) actual
    liftIO $ assertEqual "duplicate prekeys" (length regular) (length (nub regular))
    deleteClient brig (userId u) (clientId c) (Just defPassword) !!! const 200 === statusCode
