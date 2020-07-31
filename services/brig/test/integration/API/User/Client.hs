{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

module API.User.Client
  ( tests,
  )
where

import API.User.Util
import Bilge hiding (accept, head, timeout)
import Bilge.Assert
import qualified Brig.Options as Opt
import Brig.Types
import Brig.Types.User.Auth hiding (user)
import Control.Lens hiding ((#))
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Conversion
import Data.Id hiding (client)
import qualified Data.List1 as List1
import qualified Data.Vector as Vec
import Gundeck.Types.Notification
import Imports
import qualified Network.Wai.Utilities.Error as Error
import Test.Tasty hiding (Timeout)
import Test.Tasty.Cannon hiding (Cannon)
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import UnliftIO (mapConcurrently)
import Util

tests :: ConnectionLimit -> Opt.Timeout -> Opt.Opts -> Manager -> Brig -> Cannon -> Galley -> TestTree
tests _cl _at opts p b c g =
  testGroup
    "client"
    [ test p "delete /clients/:client 403 - can't delete legalhold clients" $
        testCan'tDeleteLegalHoldClient b,
      test p "post /clients 400 - can't add legalhold clients manually" $
        testCan'tAddLegalHoldClient b,
      test p "get /users/:uid/prekeys - 200" $ testGetUserPrekeys b,
      test p "get /users/:uid/prekeys/:client - 200" $ testGetClientPrekey b,
      test p "post /clients - 201 (pwd)" $ testAddGetClient True b c,
      test p "post /clients - 201 (no pwd)" $ testAddGetClient False b c,
      test p "post /clients - 403" $ testClientReauthentication b,
      test p "get /clients - 200" $ testListClients b,
      test p "get /clients/:client/prekeys - 200" $ testListPrekeyIds b,
      test p "post /clients - 400" $ testTooManyClients opts b,
      test p "delete /clients/:client - 200 (pwd)" $ testRemoveClient True b c,
      test p "delete /clients/:client - 200 (no pwd)" $ testRemoveClient False b c,
      test p "put /clients/:client - 200" $ testUpdateClient b,
      test p "post /clients - 200 multiple temporary" $ testAddMultipleTemporary b g,
      test p "client/prekeys/race" $ testPreKeyRace b
    ]

testAddGetClient :: Bool -> Brig -> Cannon -> Http ()
testAddGetClient hasPwd brig cannon = do
  uid <- userId <$> randomUser' hasPwd brig
  let rq =
        addClientReq brig uid (defNewClient TemporaryClientType [somePrekeys !! 0] (someLastPrekeys !! 0))
          . header "X-Forwarded-For" "127.0.0.1" -- Fake IP to test IpAddr parsing.
  c <- WS.bracketR cannon uid $ \ws -> do
    c <-
      responseJsonError
        =<< ( post rq <!! do
                const 201 === statusCode
                const True === isJust . getHeader "Location"
            )
    void . liftIO . WS.assertMatch (5 # Second) ws $ \n -> do
      let j = Object $ List1.head (ntfPayload n)
      let etype = j ^? key "type" . _String
      let eclient = j ^? key "client"
      etype @?= Just "user.client-add"
      fmap fromJSON eclient @?= Just (Success c)
    return c
  getClient brig uid (clientId c) !!! do
    const 200 === statusCode
    const (Just c) === responseJsonMaybe

testClientReauthentication :: Brig -> Http ()
testClientReauthentication brig = do
  let (pk1, lk1) = (somePrekeys !! 0, someLastPrekeys !! 0)
  let (pk2, lk2) = (somePrekeys !! 1, someLastPrekeys !! 1)
  let (pk3, lk3) = (somePrekeys !! 2, someLastPrekeys !! 2)
  let payload1 =
        (defNewClient PermanentClientType [pk1] lk1)
          { newClientPassword = Nothing
          }
  let payload2 =
        (defNewClient PermanentClientType [pk2] lk2)
          { newClientPassword = Nothing
          }
  let payload3 =
        (defNewClient TemporaryClientType [pk3] lk3)
          { newClientPassword = Nothing
          }
  -- User with password
  uid <- userId <$> randomUser brig
  -- The first client never requires authentication
  c <- responseJsonError =<< (addClient brig uid payload1 <!! const 201 === statusCode)
  -- Adding a second client requires reauthentication, if a password is set.
  addClient brig uid payload2 !!! do
    const 403 === statusCode
    const (Just "missing-auth") === (fmap Error.label . responseJsonMaybe)
  -- Removing a client requires reauthentication, if a password is set.
  deleteClient brig uid (clientId c) Nothing !!! const 403 === statusCode
  -- User without a password
  uid2 <- userId <$> createAnonUser "Mr. X" brig
  c2 <- responseJsonError =<< (addClient brig uid2 payload1 <!! const 201 === statusCode)
  c3 <- responseJsonError =<< (addClient brig uid2 payload2 <!! const 201 === statusCode)
  deleteClient brig uid2 (clientId c2) Nothing !!! const 200 === statusCode
  deleteClient brig uid2 (clientId c3) Nothing !!! const 200 === statusCode
  -- Temporary client can always be deleted without a password
  c4 <- responseJsonError =<< addClient brig uid payload3
  deleteClient brig uid (clientId c4) Nothing !!! const 200 === statusCode
  c5 <- responseJsonError =<< addClient brig uid2 payload3
  deleteClient brig uid2 (clientId c5) Nothing !!! const 200 === statusCode

testListClients :: Brig -> Http ()
testListClients brig = do
  uid <- userId <$> randomUser brig
  let (pk1, lk1) = (somePrekeys !! 0, (someLastPrekeys !! 0))
  let (pk2, lk2) = (somePrekeys !! 1, (someLastPrekeys !! 1))
  let (pk3, lk3) = (somePrekeys !! 2, (someLastPrekeys !! 2))
  c1 <- responseJsonMaybe <$> addClient brig uid (defNewClient PermanentClientType [pk1] lk1)
  c2 <- responseJsonMaybe <$> addClient brig uid (defNewClient PermanentClientType [pk2] lk2)
  c3 <- responseJsonMaybe <$> addClient brig uid (defNewClient TemporaryClientType [pk3] lk3)
  let cs = sortBy (compare `on` clientId) $ catMaybes [c1, c2, c3]
  get
    ( brig
        . path "clients"
        . zUser uid
    )
    !!! do
      const 200 === statusCode
      const (Just cs) === responseJsonMaybe

testListPrekeyIds :: Brig -> Http ()
testListPrekeyIds brig = do
  uid <- userId <$> randomUser brig
  let new = defNewClient PermanentClientType [somePrekeys !! 0] (someLastPrekeys !! 0)
  c <- responseJsonError =<< addClient brig uid new
  let pks = [PrekeyId 1, lastPrekeyId]
  get
    ( brig
        . paths ["clients", toByteString' (clientId c), "prekeys"]
        . zUser uid
    )
    !!! do
      const 200 === statusCode
      const (Just pks) === fmap sort . responseJsonMaybe

testGetUserPrekeys :: Brig -> Http ()
testGetUserPrekeys brig = do
  uid <- userId <$> randomUser brig
  let new = defNewClient TemporaryClientType [somePrekeys !! 0] (someLastPrekeys !! 0)
  c <- responseJsonError =<< addClient brig uid new
  let cpk = ClientPrekey (clientId c) (somePrekeys !! 0)
  get (brig . paths ["users", toByteString' uid, "prekeys"]) !!! do
    const 200 === statusCode
    const (Just $ PrekeyBundle (makeIdOpaque uid) [cpk]) === responseJsonMaybe
  -- prekeys are deleted when retrieved, except the last one
  let lpk = ClientPrekey (clientId c) (unpackLastPrekey (someLastPrekeys !! 0))
  replicateM_ 2 $
    get (brig . paths ["users", toByteString' uid, "prekeys"]) !!! do
      const 200 === statusCode
      const (Just $ PrekeyBundle (makeIdOpaque uid) [lpk]) === responseJsonMaybe

testGetClientPrekey :: Brig -> Http ()
testGetClientPrekey brig = do
  uid <- userId <$> randomUser brig
  let new = defNewClient TemporaryClientType [somePrekeys !! 0] (someLastPrekeys !! 0)
  c <- responseJsonError =<< addClient brig uid new
  get (brig . paths ["users", toByteString' uid, "prekeys", toByteString' (clientId c)]) !!! do
    const 200 === statusCode
    const (Just $ ClientPrekey (clientId c) (somePrekeys !! 0)) === responseJsonMaybe

testTooManyClients :: Opt.Opts -> Brig -> Http ()
testTooManyClients opts brig = do
  uid <- userId <$> randomUser brig
  -- We can always change the permanent client limit
  let newOpts = opts & Opt.optionSettings . Opt.userMaxPermClients .~ Just 1
  withSettingsOverrides newOpts $ do
    -- There is only one temporary client, adding a new one
    -- replaces the previous one.
    forM_ [0 .. (3 :: Int)] $ \i ->
      let pk = somePrekeys !! i
          lk = someLastPrekeys !! i
       in addClient brig uid (defNewClient TemporaryClientType [pk] lk) !!! const 201 === statusCode
    -- We can't add more permanent clients than configured
    addClient brig uid (defNewClient PermanentClientType [somePrekeys !! 10] (someLastPrekeys !! 10)) !!! do
      const 201 === statusCode
    addClient brig uid (defNewClient PermanentClientType [somePrekeys !! 11] (someLastPrekeys !! 11)) !!! do
      const 403 === statusCode
      const (Just "too-many-clients") === fmap Error.label . responseJsonMaybe

testRemoveClient :: Bool -> Brig -> Cannon -> Http ()
testRemoveClient hasPwd brig cannon = do
  u <- randomUser' hasPwd brig
  let uid = userId u
  let Just email = userEmail u
  -- Permanent client with attached cookie
  when hasPwd $ do
    login brig (defEmailLogin email) PersistentCookie
      !!! const 200 === statusCode
    numCookies <- countCookies brig uid defCookieLabel
    liftIO $ Just 1 @=? numCookies
  c <- responseJsonError =<< addClient brig uid (client PermanentClientType (someLastPrekeys !! 10))
  when hasPwd $ do
    -- Missing password
    deleteClient brig uid (clientId c) Nothing !!! const 403 === statusCode
  -- Success
  WS.bracketR cannon uid $ \ws -> do
    deleteClient brig uid (clientId c) (if hasPwd then Just defPassword else Nothing)
      !!! const 200 === statusCode
    void . liftIO . WS.assertMatch (5 # Second) ws $ \n -> do
      let j = Object $ List1.head (ntfPayload n)
      let etype = j ^? key "type" . _String
      let eclient = j ^? key "client" . key "id" . _String
      etype @?= Just "user.client-remove"
      fmap ClientId eclient @?= Just (clientId c)
  -- Not found on retry
  deleteClient brig uid (clientId c) Nothing !!! const 404 === statusCode
  -- Prekeys are gone
  getPreKey brig uid (clientId c) !!! const 404 === statusCode
  -- Cookies are gone
  numCookies' <- countCookies brig (userId u) defCookieLabel
  liftIO $ Just 0 @=? numCookies'
  where
    client ty lk =
      (defNewClient ty [somePrekeys !! 0] lk)
        { newClientLabel = Just "Nexus 5x",
          newClientCookie = Just defCookieLabel
        }

testUpdateClient :: Brig -> Http ()
testUpdateClient brig = do
  uid <- userId <$> randomUser brig
  let clt =
        (defNewClient TemporaryClientType [somePrekeys !! 0] (someLastPrekeys !! 0))
          { newClientClass = Just PhoneClient,
            newClientModel = Just "featurephone"
          }
  c <- responseJsonError =<< addClient brig uid clt
  get (brig . paths ["users", toByteString' uid, "prekeys", toByteString' (clientId c)]) !!! do
    const 200 === statusCode
    const (Just $ ClientPrekey (clientId c) (somePrekeys !! 0)) === responseJsonMaybe
  getClient brig uid (clientId c) !!! do
    const 200 === statusCode
    const (Just "Test Device") === (clientLabel <=< responseJsonMaybe)
    const (Just PhoneClient) === (clientClass <=< responseJsonMaybe)
    const (Just "featurephone") === (clientModel <=< responseJsonMaybe)
  let newPrekey = somePrekeys !! 2
  let update = UpdateClient [newPrekey] Nothing (Just "label")
  put
    ( brig
        . paths ["clients", toByteString' (clientId c)]
        . zUser uid
        . contentJson
        . body (RequestBodyLBS $ encode update)
    )
    !!! const 200
    === statusCode
  get (brig . paths ["users", toByteString' uid, "prekeys", toByteString' (clientId c)]) !!! do
    const 200 === statusCode
    const (Just $ ClientPrekey (clientId c) newPrekey) === responseJsonMaybe
  -- check if label has been updated
  getClient brig uid (clientId c) !!! do
    const 200 === statusCode
    const (Just "label") === (clientLabel <=< responseJsonMaybe)
  -- via `/users/:uid/clients/:client`, only `id` and `class` are visible:
  get (brig . paths ["users", toByteString' uid, "clients", toByteString' (clientId c)]) !!! do
    const 200 === statusCode
    const (Just $ clientId c) === (fmap pubClientId . responseJsonMaybe)
    const (Just PhoneClient) === (pubClientClass <=< responseJsonMaybe)
    const Nothing === (preview (key "label") <=< responseJsonMaybe @Value)
  let update' = UpdateClient [] Nothing Nothing
  -- empty update should be a no-op
  put
    ( brig
        . paths ["clients", toByteString' (clientId c)]
        . zUser uid
        . contentJson
        . body (RequestBodyLBS $ encode update')
    )
    !!! const 200
    === statusCode
  -- check if label is still present
  getClient brig uid (clientId c) !!! do
    const 200 === statusCode
    const (Just "label") === (clientLabel <=< responseJsonMaybe)

-- Legacy (galley)
testAddMultipleTemporary :: Brig -> Galley -> Http ()
testAddMultipleTemporary brig galley = do
  uid <- userId <$> randomUser brig
  let clt1 =
        (defNewClient TemporaryClientType [somePrekeys !! 0] (someLastPrekeys !! 0))
          { newClientClass = Just PhoneClient,
            newClientModel = Just "featurephone1"
          }
  _ <- addClient brig uid clt1
  brigClients1 <- numOfBrigClients uid
  galleyClients1 <- numOfGalleyClients uid
  liftIO $ assertEqual "Too many clients found" (Just 1) brigClients1
  liftIO $ assertEqual "Too many clients found" (Just 1) galleyClients1
  let clt2 =
        (defNewClient TemporaryClientType [somePrekeys !! 1] (someLastPrekeys !! 1))
          { newClientClass = Just PhoneClient,
            newClientModel = Just "featurephone2"
          }
  _ <- addClient brig uid clt2
  brigClients2 <- numOfBrigClients uid
  galleyClients2 <- numOfGalleyClients uid
  liftIO $ assertEqual "Too many clients found" (Just 1) brigClients2
  liftIO $ assertEqual "Too many clients found" (Just 1) galleyClients2
  where
    numOfBrigClients u = do
      r <-
        get $
          brig
            . path "clients"
            . zUser u
      return $ Vec.length <$> (preview _Array =<< responseJsonMaybe @Value r)
    numOfGalleyClients u = do
      r <-
        get $
          galley
            . path "i/test/clients"
            . zUser u
      return $ Vec.length <$> (preview _Array =<< responseJsonMaybe @Value r)

testPreKeyRace :: Brig -> Http ()
testPreKeyRace brig = do
  uid <- userId <$> randomUser brig
  let pks = map (\i -> somePrekeys !! i) [1 .. 10]
  c <- responseJsonError =<< addClient brig uid (defNewClient PermanentClientType pks (someLastPrekeys !! 0))
  pks' <- flip mapConcurrently pks $ \_ -> do
    rs <- getPreKey brig uid (clientId c) <!! const 200 === statusCode
    return $ prekeyId . prekeyData <$> responseJsonMaybe rs
  -- We should not hand out regular prekeys more than once (i.e. at most once).
  let actual = catMaybes pks'
  liftIO $ assertEqual "insufficient prekeys" (length pks) (length actual)
  let regular = filter (/= lastPrekeyId) actual
  liftIO $ assertEqual "duplicate prekeys" (length regular) (length (nub regular))
  deleteClient brig uid (clientId c) (Just defPassword) !!! const 200 === statusCode

testCan'tDeleteLegalHoldClient :: Brig -> Http ()
testCan'tDeleteLegalHoldClient brig = do
  let hasPassword = False
  user <- randomUser' hasPassword brig
  let uid = userId user
  let pk = head somePrekeys
  let lk = head someLastPrekeys
  resp <-
    addClientInternal brig uid (defNewClient LegalHoldClientType [pk] lk)
      <!! const 201 === statusCode
  lhClientId <- clientId <$> responseJsonError resp
  deleteClient brig uid lhClientId Nothing !!! const 400 === statusCode

testCan'tAddLegalHoldClient :: Brig -> Http ()
testCan'tAddLegalHoldClient brig = do
  let hasPassword = False
  user <- randomUser' hasPassword brig
  let uid = userId user
  let pk = head somePrekeys
  let lk = head someLastPrekeys
  -- Regular users cannot add legalhold clients
  addClient brig uid (defNewClient LegalHoldClientType [pk] lk) !!! const 400 === statusCode
