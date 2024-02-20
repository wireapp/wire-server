{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

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

module API.User.Client
  ( tests,
  )
where

import API.Team.Util qualified as Util
import API.User.Util
import API.User.Util qualified as Util
import Bilge hiding (accept, head, timeout)
import Bilge.Assert
import Brig.Code qualified as Code
import Brig.Options qualified as Opt
import Brig.Options qualified as Opts
import Cassandra qualified as DB
import Control.Lens hiding (Wrapped, (#))
import Crypto.JWT hiding (Ed25519, header, params)
import Data.Aeson hiding (json)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as M
import Data.Aeson.Lens
import Data.ByteString.Conversion
import Data.Coerce (coerce)
import Data.Default
import Data.Domain (Domain (..))
import Data.Handle
import Data.Id
import Data.List1 qualified as List1
import Data.Map qualified as Map
import Data.Nonce (isValidBase64UrlEncodedUUID)
import Data.Qualified (Qualified (..))
import Data.Range (unsafeRange)
import Data.Set qualified as Set
import Data.Text.Ascii (AsciiChars (validate), encodeBase64UrlUnpadded, toText)
import Data.Text.Encoding qualified as T
import Data.Time (addUTCTime)
import Data.Time.Clock.POSIX
import Data.UUID (toByteString)
import Data.UUID qualified as UUID
import Data.Vector qualified as Vec
import Imports
import Network.Wai.Utilities.Error qualified as Error
import System.Logger qualified as Log
import Test.QuickCheck (arbitrary, generate)
import Test.Tasty hiding (Timeout)
import Test.Tasty.Cannon hiding (Cannon)
import Test.Tasty.Cannon qualified as WS
import Test.Tasty.HUnit
import UnliftIO (mapConcurrently)
import Util
import Wire.API.Internal.Notification
import Wire.API.MLS.CipherSuite
import Wire.API.Team.Feature qualified as Public
import Wire.API.User
import Wire.API.User qualified as Public
import Wire.API.User.Auth
import Wire.API.User.Client
import Wire.API.User.Client.DPoPAccessToken
import Wire.API.User.Client.Prekey
import Wire.API.UserMap (QualifiedUserMap (..), UserMap (..), WrappedQualifiedUserMap)
import Wire.API.Wrapped (Wrapped (..))

tests :: ConnectionLimit -> Opt.Timeout -> Opt.Opts -> Manager -> DB.ClientState -> Nginz -> Brig -> Cannon -> Galley -> TestTree
tests _cl _at opts p db n b c g =
  testGroup
    "client"
    [ test p "get /users/:uid/clients - 200" $ testGetUserClientsUnqualified opts b,
      test p "get /users/<localdomain>/:uid/clients - 200" $ testGetUserClientsQualified opts b,
      test p "get /users/:uid/prekeys - 200" $ testGetUserPrekeys b,
      test p "get /users/<localdomain>/:uid/prekeys - 200" $ testGetUserPrekeysQualified b opts,
      test p "get /users/:domain/:uid/prekeys - 422" $ testGetUserPrekeysInvalidDomain b,
      test p "get /users/:uid/prekeys/:client - 200" $ testGetClientPrekey b,
      test p "get /users/<localdomain>/:uid/prekeys/:client - 200" $ testGetClientPrekeyQualified b opts,
      test p "post /users/prekeys" $ testMultiUserGetPrekeys b,
      test p "post /users/list-prekeys" $ testMultiUserGetPrekeysQualified b opts,
      test p "post /users/list-prekeys@v4" $ testMultiUserGetPrekeysQualifiedV4 b opts,
      test p "post /users/list-clients - 200" $ testListClientsBulk opts b,
      test p "post /users/list-clients/v2 - 200" $ testListClientsBulkV2 opts b,
      test p "post /users/list-prekeys - clients without prekeys" $ testClientsWithoutPrekeys b c db opts,
      test p "post /users/list-prekeys@v4 - clients without prekeys" $ testClientsWithoutPrekeysV4 b c db opts,
      test p "post /users/list-prekeys@v4 - clients without prekeys fail to list" $ testClientsWithoutPrekeysFailToListV4 b c db opts,
      test p "post /clients - 201 (pwd)" $ testAddGetClient def {addWithPassword = True} b c,
      test p "post /clients - 201 (no pwd)" $ testAddGetClient def {addWithPassword = False} b c,
      testGroup
        "post /clients - verification code"
        [ test p "success" $ testAddGetClientVerificationCode db b g,
          test p "missing code" $ testAddGetClientMissingCode b g,
          test p "wrong code" $ testAddGetClientWrongCode b g,
          test p "expired code" $ testAddGetClientCodeExpired db opts b g
        ],
      test p "post /clients - 201 (with mls keys)" $ testAddGetClient def {addWithMLSKeys = True} b c,
      test p "post /clients - 403" $ testClientReauthentication b,
      test p "get /clients - 200" $ testListClients b,
      test p "get /clients/:client/prekeys - 200" $ testListPrekeyIds b,
      test p "post /clients - 400" $ testTooManyClients opts b,
      test p "client/prekeys not empty" $ testPrekeysNotEmptyRandomPrekeys opts b,
      test p "lastprekeys not bogus" $ testRegularPrekeysCannotBeSentAsLastPrekeys b,
      test p "lastprekeys not bogus during update" $ testRegularPrekeysCannotBeSentAsLastPrekeysDuringUpdate b,
      test p "delete /clients/:client - 200 (pwd)" $ testRemoveClient True b c,
      test p "delete /clients/:client - 200 (no pwd)" $ testRemoveClient False b c,
      test p "delete /clients/:client - 400 (short pwd)" $ testRemoveClientShortPwd b,
      test p "delete /clients/:client - 403 (incorrect pwd)" $ testRemoveClientIncorrectPwd b,
      test p "put /clients/:client - 200" $ testUpdateClient opts b,
      test p "put /clients/:client - 200 (mls keys)" $ testMLSPublicKeyUpdate b,
      test p "get /clients/:client - 404" $ testMissingClient b,
      test p "get /clients/:client - 200" $ testMLSClient b,
      test p "post /clients - 200 multiple temporary" $ testAddMultipleTemporary b g c,
      test p "client/prekeys/race" $ testPreKeyRace b,
      test p "get/head nonce/clients" $ testNewNonce b,
      testGroup
        "post /clients/:cid/access-token"
        [ test p "success" $ testCreateAccessToken opts n b,
          test p "proof missing" $ testCreateAccessTokenMissingProof b,
          test p "no nonce" $ testCreateAccessTokenNoNonce b
        ]
    ]

testAddGetClientVerificationCode :: DB.ClientState -> Brig -> Galley -> Http ()
testAddGetClientVerificationCode db brig galley = do
  (u, tid) <- Util.createUserWithTeam' brig
  let uid = userId u
  let Just email = userEmail u
  let checkLoginSucceeds b = login brig b PersistentCookie !!! const 200 === statusCode
  let addClient' :: Maybe Code.Value -> Http Client
      addClient' codeValue = responseJsonError =<< addClient brig uid (defNewClientWithVerificationCode codeValue PermanentClientType [head somePrekeys] (head someLastPrekeys))

  Util.setTeamFeatureLockStatus @Public.SndFactorPasswordChallengeConfig galley tid Public.LockStatusUnlocked
  Util.setTeamSndFactorPasswordChallenge galley tid Public.FeatureStatusEnabled
  Util.generateVerificationCode brig (Public.SendVerificationCode Public.Login email)
  k <- Code.mkKey (Code.ForEmail email)
  codeValue <- Code.codeValue <$$> lookupCode db k Code.AccountLogin
  checkLoginSucceeds $
    PasswordLogin $
      PasswordLoginData (LoginByEmail email) defPassword (Just defCookieLabel) codeValue
  c <- addClient' codeValue
  getClient brig uid (clientId c) !!! do
    const 200 === statusCode
    const (Just c) === responseJsonMaybe

-- @SF.Channel @TSFI.RESTfulAPI @S2
--
-- Test that device cannot be added with missing second factor email verification code when this feature is enabled
testAddGetClientMissingCode :: Brig -> Galley -> Http ()
testAddGetClientMissingCode brig galley = do
  (u, tid) <- Util.createUserWithTeam' brig
  let uid = userId u
  let Just email = userEmail u
  let addClient' codeValue = addClient brig uid (defNewClientWithVerificationCode codeValue PermanentClientType [head somePrekeys] (head someLastPrekeys))

  Util.setTeamFeatureLockStatus @Public.SndFactorPasswordChallengeConfig galley tid Public.LockStatusUnlocked
  Util.setTeamSndFactorPasswordChallenge galley tid Public.FeatureStatusEnabled
  Util.generateVerificationCode brig (Public.SendVerificationCode Public.Login email)
  addClient' Nothing !!! do
    const 403 === statusCode
    const (Just "code-authentication-required") === fmap Error.label . responseJsonMaybe

-- @END

-- @SF.Channel @TSFI.RESTfulAPI @S2
--
-- Test that device cannot be added with wrong second factor email verification code when this feature is enabled
testAddGetClientWrongCode :: Brig -> Galley -> Http ()
testAddGetClientWrongCode brig galley = do
  (u, tid) <- Util.createUserWithTeam' brig
  let uid = userId u
  let Just email = userEmail u
  let addClient' codeValue = addClient brig uid (defNewClientWithVerificationCode codeValue PermanentClientType [head somePrekeys] (head someLastPrekeys))

  Util.setTeamFeatureLockStatus @Public.SndFactorPasswordChallengeConfig galley tid Public.LockStatusUnlocked
  Util.setTeamSndFactorPasswordChallenge galley tid Public.FeatureStatusEnabled
  Util.generateVerificationCode brig (Public.SendVerificationCode Public.Login email)
  let wrongCode = Code.Value $ unsafeRange (fromRight undefined (validate "123456"))
  addClient' (Just wrongCode) !!! do
    const 403 === statusCode
    const (Just "code-authentication-failed") === fmap Error.label . responseJsonMaybe

-- @END

-- @SF.Channel @TSFI.RESTfulAPI @S2
--
-- Test that device cannot be added with expired second factor email verification code when this feature is enabled
testAddGetClientCodeExpired :: DB.ClientState -> Opt.Opts -> Brig -> Galley -> Http ()
testAddGetClientCodeExpired db opts brig galley = do
  (u, tid) <- Util.createUserWithTeam' brig
  let uid = userId u
  let Just email = userEmail u
  let checkLoginSucceeds b = login brig b PersistentCookie !!! const 200 === statusCode
  let addClient' codeValue = addClient brig uid (defNewClientWithVerificationCode codeValue PermanentClientType [head somePrekeys] (head someLastPrekeys))

  Util.setTeamFeatureLockStatus @Public.SndFactorPasswordChallengeConfig galley tid Public.LockStatusUnlocked
  Util.setTeamSndFactorPasswordChallenge galley tid Public.FeatureStatusEnabled
  Util.generateVerificationCode brig (Public.SendVerificationCode Public.Login email)
  k <- Code.mkKey (Code.ForEmail email)
  codeValue <- Code.codeValue <$$> lookupCode db k Code.AccountLogin
  checkLoginSucceeds $
    PasswordLogin $
      PasswordLoginData (LoginByEmail email) defPassword (Just defCookieLabel) codeValue
  let verificationTimeout = round (Opt.setVerificationTimeout (Opt.optSettings opts))
  threadDelay $ ((verificationTimeout + 1) * 1000_000)
  addClient' codeValue !!! do
    const 403 === statusCode
    const (Just "code-authentication-failed") === fmap Error.label . responseJsonMaybe

-- @END

data AddGetClient = AddGetClient
  { addWithPassword :: Bool,
    addWithMLSKeys :: Bool
  }

instance Default AddGetClient where
  def = AddGetClient True False

testAddGetClient :: AddGetClient -> Brig -> Cannon -> Http ()
testAddGetClient params brig cannon = do
  uid <- userId <$> randomUser' (addWithPassword params) brig
  let new =
        (defNewClient TemporaryClientType [somePrekeys !! 0] (someLastPrekeys !! 0))
          { newClientMLSPublicKeys = keys
          }
      keys
        | addWithMLSKeys params = Map.fromList [(Ed25519, "aGVsbG8gd29ybGQ=")]
        | otherwise = mempty
  let rq =
        addClientReq brig uid new
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
    pure c
  liftIO $ clientMLSPublicKeys c @?= keys
  getClient brig uid (clientId c) !!! do
    const 200 === statusCode
    const (Just c) === responseJsonMaybe

testGetUserClientsUnqualified :: Opt.Opts -> Brig -> Http ()
testGetUserClientsUnqualified _opts brig = do
  uid1 <- userId <$> randomUser brig
  let (pk11, lk11) = (somePrekeys !! 0, someLastPrekeys !! 0)
  let (pk12, lk12) = (somePrekeys !! 1, someLastPrekeys !! 1)
  let (pk13, lk13) = (somePrekeys !! 2, someLastPrekeys !! 2)
  _c11 :: Client <- responseJsonError =<< addClient brig uid1 (defNewClient PermanentClientType [pk11] lk11)
  _c12 :: Client <- responseJsonError =<< addClient brig uid1 (defNewClient PermanentClientType [pk12] lk12)
  _c13 :: Client <- responseJsonError =<< addClient brig uid1 (defNewClient TemporaryClientType [pk13] lk13)
  getUserClientsUnqualified brig uid1 !!! do
    const 200 === statusCode
    assertTrue_ $ \res -> do
      let clients :: [PubClient] = responseJsonUnsafe res
       in length clients == 3

testGetUserClientsQualified :: Opt.Opts -> Brig -> Http ()
testGetUserClientsQualified opts brig = do
  uid1 <- userId <$> randomUser brig
  uid2 <- userId <$> randomUser brig
  let (pk11, lk11) = (somePrekeys !! 0, someLastPrekeys !! 0)
  let (pk12, lk12) = (somePrekeys !! 1, someLastPrekeys !! 1)
  let (pk13, lk13) = (somePrekeys !! 2, someLastPrekeys !! 2)
  _c11 :: Client <- responseJsonError =<< addClient brig uid1 (defNewClient PermanentClientType [pk11] lk11)
  _c12 :: Client <- responseJsonError =<< addClient brig uid1 (defNewClient PermanentClientType [pk12] lk12)
  _c13 :: Client <- responseJsonError =<< addClient brig uid1 (defNewClient TemporaryClientType [pk13] lk13)
  let localdomain = opts ^. Opt.optionSettings & Opt.setFederationDomain
  getUserClientsQualified brig uid2 localdomain uid1 !!! do
    const 200 === statusCode
    assertTrue_ $ \res -> do
      let clients :: [PubClient] = responseJsonUnsafe res
       in length clients == 3

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
  let (pk1, lk1) = (somePrekeys !! 0, someLastPrekeys !! 0)
  let (pk2, lk2) = (somePrekeys !! 1, someLastPrekeys !! 1)
  let (pk3, lk3) = (somePrekeys !! 2, someLastPrekeys !! 2)
  c1 <- responseJsonError =<< addClient brig uid (defNewClient PermanentClientType [pk1] lk1)
  c2 <- responseJsonError =<< addClient brig uid (defNewClient PermanentClientType [pk2] lk2)
  c3 <- responseJsonError =<< addClient brig uid (defNewClient TemporaryClientType [pk3] lk3)

  let pks = Map.fromList [(Ed25519, "random")]
  void $ putClient brig uid (clientId c1) pks
  let c1' = c1 {clientMLSPublicKeys = pks}
  let clients = sortBy (compare `on` clientId) [c1', c2, c3]

  get
    ( brig
        . path "clients"
        . zUser uid
    )
    !!! do
      const 200 === statusCode
      const (Just clients) === responseJsonMaybe

testMLSClient :: Brig -> Http ()
testMLSClient brig = do
  uid <- userId <$> randomUser brig
  let (pk1, lk1) = (somePrekeys !! 0, someLastPrekeys !! 0)
  let (pk2, lk2) = (somePrekeys !! 1, someLastPrekeys !! 1)
  -- An MLS client
  c1 <- responseJsonError =<< addClient brig uid (defNewClient PermanentClientType [pk1] lk1)
  -- Non-MLS client
  c2 <- responseJsonError =<< addClient brig uid (defNewClient PermanentClientType [pk2] lk2)

  let pks = Map.fromList [(Ed25519, "random")]
  void $ putClient brig uid (clientId c1) pks

  -- Assert that adding MLS public keys to one client does not affect the other
  -- client
  getClient brig uid (clientId c2) !!! do
    const 200 === statusCode
    const (Just c2) === responseJsonMaybe

testListClientsBulk :: Opt.Opts -> Brig -> Http ()
testListClientsBulk opts brig = do
  uid1 <- userId <$> randomUser brig
  let (pk11, lk11) = (somePrekeys !! 0, someLastPrekeys !! 0)
  let (pk12, lk12) = (somePrekeys !! 1, someLastPrekeys !! 1)
  let (pk13, lk13) = (somePrekeys !! 2, someLastPrekeys !! 2)
  c11 <- responseJsonError =<< addClient brig uid1 (defNewClient PermanentClientType [pk11] lk11)
  c12 <- responseJsonError =<< addClient brig uid1 (defNewClient PermanentClientType [pk12] lk12)
  c13 <- responseJsonError =<< addClient brig uid1 (defNewClient TemporaryClientType [pk13] lk13)

  uid2 <- userId <$> randomUser brig
  let (pk21, lk21) = (somePrekeys !! 3, someLastPrekeys !! 3)
  let (pk22, lk22) = (somePrekeys !! 4, someLastPrekeys !! 4)
  c21 <- responseJsonError =<< addClient brig uid2 (defNewClient PermanentClientType [pk21] lk21)
  c22 <- responseJsonError =<< addClient brig uid2 (defNewClient PermanentClientType [pk22] lk22)

  let domain = Opt.setFederationDomain $ Opt.optSettings opts
  uid3 <- userId <$> randomUser brig
  let mkPubClient cl = PubClient (clientId cl) (clientClass cl)
  let expectedResponse :: QualifiedUserMap (Set PubClient) =
        QualifiedUserMap $
          Map.singleton
            domain
            ( UserMap $
                Map.fromList
                  [ (uid1, Set.fromList $ mkPubClient <$> [c11, c12, c13]),
                    (uid2, Set.fromList $ mkPubClient <$> [c21, c22])
                  ]
            )
  post
    ( apiVersion "v1"
        . brig
        . paths ["users", "list-clients"]
        . zUser uid3
        . contentJson
        . body (RequestBodyLBS $ encode [Qualified uid1 domain, Qualified uid2 domain])
    )
    !!! do
      const 200 === statusCode
      const (Just expectedResponse) === responseJsonMaybe

testClientsWithoutPrekeys :: Brig -> Cannon -> DB.ClientState -> Opt.Opts -> Http ()
testClientsWithoutPrekeys brig cannon db opts = do
  uid1 <- userId <$> randomUser brig
  let (pk11, lk11) = (somePrekeys !! 0, someLastPrekeys !! 0)
  c11 <- responseJsonError =<< addClient brig uid1 (defNewClient PermanentClientType [pk11] lk11)
  let (pk12, lk12) = (somePrekeys !! 1, someLastPrekeys !! 1)
  c12 <- responseJsonError =<< addClient brig uid1 (defNewClient PermanentClientType [pk12] lk12)

  -- Simulating loss of all prekeys from c11 (due e.g. DB problems, business
  -- logic prevents this from happening)
  let removeClientKeys :: DB.PrepQuery DB.W (UserId, ClientId) ()
      removeClientKeys = "DELETE FROM prekeys where user = ? and client = ?"
  liftIO $
    DB.runClient db $
      DB.write removeClientKeys (DB.params DB.LocalQuorum (uid1, clientId c11))

  uid2 <- userId <$> randomUser brig

  let domain = opts ^. Opt.optionSettings & Opt.setFederationDomain

  let userClients =
        QualifiedUserClients $
          Map.singleton domain $
            Map.singleton uid1 $
              Set.fromList [clientId c11, clientId c12]

  WS.bracketR cannon uid1 $ \ws -> do
    getClient brig uid1 (clientId c11) !!! do
      const 200 === statusCode

    post
      ( apiVersion "v3"
          . brig
          . paths ["users", "list-prekeys"]
          . contentJson
          . body (RequestBodyLBS $ encode userClients)
          . zUser uid2
      )
      !!! do
        const 200 === statusCode
        const
          ( Right $
              expectedClientMap
                domain
                uid1
                [ (clientId c11, Nothing),
                  (clientId c12, Just pk12)
                ]
          )
          === responseJsonEither

    getClient brig uid1 (clientId c11) !!! do
      const 404 === statusCode

    liftIO . WS.assertMatch (5 # Second) ws $ \n -> do
      let ob = Object $ List1.head (ntfPayload n)
      ob ^? key "type" . _String
        @?= Just "user.client-remove"
      ( fromByteString . T.encodeUtf8
          =<< (ob ^? key "client" . key "id" . _String)
        )
        @?= Just (clientId c11)

  post
    ( apiVersion "v3"
        . brig
        . paths ["users", "list-prekeys"]
        . contentJson
        . body (RequestBodyLBS $ encode userClients)
        . zUser uid2
    )
    !!! do
      const 200 === statusCode
      const
        ( Right $
            expectedClientMap
              domain
              uid1
              [ (clientId c11, Nothing),
                (clientId c12, Just (unpackLastPrekey lk12))
              ]
        )
        === responseJsonEither
  where
    expectedClientMap :: Domain -> UserId -> [(ClientId, Maybe Prekey)] -> QualifiedUserClientPrekeyMap
    expectedClientMap domain u xs =
      mkQualifiedUserClientPrekeyMap $
        Map.singleton domain $
          mkUserClientPrekeyMap $
            Map.singleton u $
              Map.fromList xs

testClientsWithoutPrekeysV4 :: Brig -> Cannon -> DB.ClientState -> Opt.Opts -> Http ()
testClientsWithoutPrekeysV4 brig cannon db opts = do
  uid1 <- userId <$> randomUser brig
  let (pk11, lk11) = (somePrekeys !! 0, someLastPrekeys !! 0)
  c11 <- responseJsonError =<< addClient brig uid1 (defNewClient PermanentClientType [pk11] lk11)
  let (pk12, lk12) = (somePrekeys !! 1, someLastPrekeys !! 1)
  c12 <- responseJsonError =<< addClient brig uid1 (defNewClient PermanentClientType [pk12] lk12)

  -- Simulating loss of all prekeys from c11 (due e.g. DB problems, business
  -- logic prevents this from happening)
  let removeClientKeys :: DB.PrepQuery DB.W (UserId, ClientId) ()
      removeClientKeys = "DELETE FROM prekeys where user = ? and client = ?"
  liftIO $
    DB.runClient db $
      DB.write removeClientKeys (DB.params DB.LocalQuorum (uid1, clientId c11))

  uid2 <- userId <$> randomUser brig

  let domain = opts ^. Opt.optionSettings & Opt.setFederationDomain

  let userClients =
        QualifiedUserClients $
          Map.singleton domain $
            Map.singleton uid1 $
              Set.fromList [clientId c11, clientId c12]

  WS.bracketR cannon uid1 $ \ws -> do
    getClient brig uid1 (clientId c11) !!! do
      const 200 === statusCode

    post
      ( brig
          . paths ["users", "list-prekeys"]
          . contentJson
          . body (RequestBodyLBS $ encode userClients)
          . zUser uid2
      )
      !!! do
        const 200 === statusCode
        const
          ( Right $
              expectedClientMapClientsWithoutPrekeys
                domain
                uid1
                [ (clientId c11, Nothing),
                  (clientId c12, Just pk12)
                ]
                Nothing
          )
          === responseJsonEither

    getClient brig uid1 (clientId c11) !!! do
      const 404 === statusCode

    liftIO . WS.assertMatch (5 # Second) ws $ \n -> do
      let ob = Object $ List1.head (ntfPayload n)
      ob ^? key "type" . _String
        @?= Just "user.client-remove"
      (fromByteString . T.encodeUtf8 =<< (ob ^? key "client" . key "id" . _String))
        @?= Just (clientId c11)

  post
    ( brig
        . paths ["users", "list-prekeys"]
        . contentJson
        . body (RequestBodyLBS $ encode userClients)
        . zUser uid2
    )
    !!! do
      const 200 === statusCode
      const
        ( Right $
            expectedClientMapClientsWithoutPrekeys
              domain
              uid1
              [ (clientId c11, Nothing),
                (clientId c12, Just (unpackLastPrekey lk12))
              ]
              Nothing
        )
        === responseJsonEither

expectedClientMapClientsWithoutPrekeys :: Domain -> UserId -> [(ClientId, Maybe Prekey)] -> Maybe [Qualified UserId] -> QualifiedUserClientPrekeyMapV4
expectedClientMapClientsWithoutPrekeys domain u xs failed =
  QualifiedUserClientPrekeyMapV4
    { qualifiedUserClientPrekeys =
        coerce $
          mkQualifiedUserClientPrekeyMap $
            Map.singleton domain $
              mkUserClientPrekeyMap $
                Map.singleton u $
                  Map.fromList xs,
      failedToList = failed
    }

testClientsWithoutPrekeysFailToListV4 :: Brig -> Cannon -> DB.ClientState -> Opt.Opts -> Http ()
testClientsWithoutPrekeysFailToListV4 brig cannon db opts = do
  uid1 <- userId <$> randomUser brig
  let (pk11, lk11) = (somePrekeys !! 0, someLastPrekeys !! 0)
  c11 <- responseJsonError =<< addClient brig uid1 (defNewClient PermanentClientType [pk11] lk11)
  let (pk12, lk12) = (somePrekeys !! 1, someLastPrekeys !! 1)
  c12 <- responseJsonError =<< addClient brig uid1 (defNewClient PermanentClientType [pk12] lk12)

  -- Simulating loss of all prekeys from c11 (due e.g. DB problems, business
  -- logic prevents this from happening)
  let removeClientKeys :: DB.PrepQuery DB.W (UserId, ClientId) ()
      removeClientKeys = "DELETE FROM prekeys where user = ? and client = ?"
  liftIO $
    DB.runClient db $
      DB.write removeClientKeys (DB.params DB.LocalQuorum (uid1, clientId c11))

  uid2 <- fakeRemoteUser

  let domain = opts ^. Opt.optionSettings & Opt.setFederationDomain

  let userClients1 =
        QualifiedUserClients $
          Map.singleton domain $
            Map.singleton uid1 $
              Set.fromList [clientId c11, clientId c12]
      userClients2 =
        QualifiedUserClients $
          Map.fromList
            [ ( qDomain uid2,
                Map.singleton (qUnqualified uid2) mempty
              )
            ]

  WS.bracketR cannon uid1 $ \ws -> do
    getClient brig uid1 (clientId c11) !!! do
      const 200 === statusCode

    post
      ( brig
          . paths ["users", "list-prekeys"]
          . contentJson
          . body (RequestBodyLBS $ encode userClients1)
          . zUser (qUnqualified uid2)
      )
      !!! do
        const 200 === statusCode
        const
          ( Right $
              expectedClientMapClientsWithoutPrekeys
                domain
                uid1
                [ (clientId c11, Nothing),
                  (clientId c12, Just pk12)
                ]
                Nothing
          )
          === responseJsonEither

    getClient brig uid1 (clientId c11) !!! do
      const 404 === statusCode

    liftIO . WS.assertMatch (5 # Second) ws $ \n -> do
      let ob = Object $ List1.head (ntfPayload n)
      ob ^? key "type" . _String
        @?= Just "user.client-remove"
      (fromByteString . T.encodeUtf8 =<< (ob ^? key "client" . key "id" . _String))
        @?= Just (clientId c11)

  post
    ( brig
        . paths ["users", "list-prekeys"]
        . contentJson
        . body (RequestBodyLBS $ encode userClients2)
        . zUser (qUnqualified uid2)
    )
    !!! do
      const 200 === statusCode
      const
        ( Right $
            QualifiedUserClientPrekeyMapV4
              { qualifiedUserClientPrekeys = QualifiedUserClientMap Map.empty,
                failedToList = pure [uid2]
              }
        )
        === responseJsonEither

testListClientsBulkV2 :: Opt.Opts -> Brig -> Http ()
testListClientsBulkV2 opts brig = do
  uid1 <- userId <$> randomUser brig
  let (pk11, lk11) = (somePrekeys !! 0, someLastPrekeys !! 0)
  let (pk12, lk12) = (somePrekeys !! 1, someLastPrekeys !! 1)
  let (pk13, lk13) = (somePrekeys !! 2, someLastPrekeys !! 2)
  c11 <- responseJsonError =<< addClient brig uid1 (defNewClient PermanentClientType [pk11] lk11)
  c12 <- responseJsonError =<< addClient brig uid1 (defNewClient PermanentClientType [pk12] lk12)
  c13 <- responseJsonError =<< addClient brig uid1 (defNewClient TemporaryClientType [pk13] lk13)

  uid2 <- userId <$> randomUser brig
  let (pk21, lk21) = (somePrekeys !! 3, someLastPrekeys !! 3)
  let (pk22, lk22) = (somePrekeys !! 4, someLastPrekeys !! 4)
  c21 <- responseJsonError =<< addClient brig uid2 (defNewClient PermanentClientType [pk21] lk21)
  c22 <- responseJsonError =<< addClient brig uid2 (defNewClient PermanentClientType [pk22] lk22)

  let domain = Opt.setFederationDomain $ Opt.optSettings opts
  uid3 <- userId <$> randomUser brig
  let mkPubClient cl = PubClient (clientId cl) (clientClass cl)
  let expectedResponse :: WrappedQualifiedUserMap (Set PubClient) =
        Wrapped . QualifiedUserMap $
          Map.singleton
            domain
            ( UserMap $
                Map.fromList
                  [ (uid1, Set.fromList $ mkPubClient <$> [c11, c12, c13]),
                    (uid2, Set.fromList $ mkPubClient <$> [c21, c22])
                  ]
            )
  post
    ( apiVersion "v1"
        . brig
        . paths ["users", "list-clients", "v2"]
        . zUser uid3
        . contentJson
        . body (RequestBodyLBS $ encode (LimitedQualifiedUserIdList @20 (unsafeRange [Qualified uid1 domain, Qualified uid2 domain])))
    )
    !!! do
      const 200 === statusCode
      const (Just expectedResponse) === responseJsonMaybe

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

generateClients :: Int -> Brig -> Http [(UserId, Client, ClientPrekey, ClientPrekey)]
generateClients n brig = do
  for [1 .. n] $ \i -> do
    uid <- userId <$> randomUser brig
    let new = defNewClient TemporaryClientType [somePrekeys !! i] (someLastPrekeys !! i)
    c <- responseJsonError =<< addClient brig uid new
    let cpk = ClientPrekey (clientId c) (somePrekeys !! i)
    let lpk = ClientPrekey (clientId c) (unpackLastPrekey (someLastPrekeys !! i))
    pure (uid, c, lpk, cpk)

testGetUserPrekeys :: Brig -> Http ()
testGetUserPrekeys brig = do
  [(uid, _c, lpk, cpk)] <- generateClients 1 brig
  get (apiVersion "v1" . brig . paths ["users", toByteString' uid, "prekeys"] . zUser uid) !!! do
    const 200 === statusCode
    const (Just $ PrekeyBundle uid [cpk]) === responseJsonMaybe
  -- prekeys are deleted when retrieved, except the last one
  replicateM_ 2 $
    get (apiVersion "v1" . brig . paths ["users", toByteString' uid, "prekeys"] . zUser uid) !!! do
      const 200 === statusCode
      const (Just $ PrekeyBundle uid [lpk]) === responseJsonMaybe

testGetUserPrekeysQualified :: Brig -> Opt.Opts -> Http ()
testGetUserPrekeysQualified brig opts = do
  let domain = opts ^. Opt.optionSettings & Opt.setFederationDomain
  [(uid, _c, _lpk, cpk)] <- generateClients 1 brig
  get (brig . paths ["users", toByteString' domain, toByteString' uid, "prekeys"] . zUser uid) !!! do
    const 200 === statusCode
    const (Just $ PrekeyBundle uid [cpk]) === responseJsonMaybe

testGetUserPrekeysInvalidDomain :: Brig -> Http ()
testGetUserPrekeysInvalidDomain brig = do
  [(uid, _c, _lpk, _)] <- generateClients 1 brig
  get (brig . paths ["users", "invalid.example.com", toByteString' uid, "prekeys"] . zUser uid) !!! do
    const 422 === statusCode

testGetClientPrekey :: Brig -> Http ()
testGetClientPrekey brig = do
  [(uid, c, _lpk, cpk)] <- generateClients 1 brig
  get (apiVersion "v1" . brig . paths ["users", toByteString' uid, "prekeys", toByteString' (clientId c)] . zUser uid) !!! do
    const 200 === statusCode
    const (Just $ cpk) === responseJsonMaybe

testGetClientPrekeyQualified :: Brig -> Opt.Opts -> Http ()
testGetClientPrekeyQualified brig opts = do
  let domain = opts ^. Opt.optionSettings & Opt.setFederationDomain
  [(uid, c, _lpk, cpk)] <- generateClients 1 brig
  get (brig . paths ["users", toByteString' domain, toByteString' uid, "prekeys", toByteString' (clientId c)] . zUser uid) !!! do
    const 200 === statusCode
    const (Just $ cpk) === responseJsonMaybe

testMultiUserGetPrekeys :: Brig -> Http ()
testMultiUserGetPrekeys brig = do
  xs <- generateClients 3 brig
  let userClients =
        UserClients $
          Map.fromList $
            xs <&> \(uid, c, _lpk, _cpk) ->
              (uid, Set.fromList [clientId c])

  let expectedUserClientMap =
        mkUserClientPrekeyMap $
          Map.fromList $
            xs <&> \(uid, c, _lpk, cpk) ->
              (uid, Map.singleton (clientId c) (Just (prekeyData cpk)))

  uid <- userId <$> randomUser brig

  post
    ( apiVersion "v1"
        . brig
        . paths ["users", "prekeys"]
        . contentJson
        . body (RequestBodyLBS $ encode userClients)
        . zUser uid
    )
    !!! do
      const 200 === statusCode
      const (Right $ expectedUserClientMap) === responseJsonEither

testMultiUserGetPrekeysQualified :: Brig -> Opt.Opts -> Http ()
testMultiUserGetPrekeysQualified brig opts = do
  let domain = opts ^. Opt.optionSettings & Opt.setFederationDomain

  xs <- generateClients 3 brig
  let userClients =
        QualifiedUserClients $
          Map.singleton domain $
            Map.fromList $
              xs <&> \(uid, c, _lpk, _cpk) ->
                (uid, Set.fromList [clientId c])

  uid <- userId <$> randomUser brig

  let expectedUserClientMap =
        mkQualifiedUserClientPrekeyMap $
          Map.singleton domain $
            mkUserClientPrekeyMap $
              Map.fromList $
                xs <&> \(uid', c, _lpk, cpk) ->
                  (uid', Map.singleton (clientId c) (Just (prekeyData cpk)))

  post
    ( apiVersion "v2"
        . brig
        . paths ["users", "list-prekeys"]
        . contentJson
        . body (RequestBodyLBS $ encode userClients)
        . zUser uid
    )
    !!! do
      const 200 === statusCode
      const (Right $ expectedUserClientMap) === responseJsonEither

testMultiUserGetPrekeysQualifiedV4 :: Brig -> Opt.Opts -> Http ()
testMultiUserGetPrekeysQualifiedV4 brig opts = do
  let domain = opts ^. Opt.optionSettings & Opt.setFederationDomain

  xs <- generateClients 3 brig
  let userClients =
        QualifiedUserClients $
          Map.singleton domain $
            Map.fromList $
              xs <&> \(uid, c, _lpk, _cpk) ->
                (uid, Set.fromList [clientId c])

  uid <- userId <$> randomUser brig

  let expectedUserClientMap =
        QualifiedUserClientPrekeyMapV4
          { qualifiedUserClientPrekeys =
              coerce $
                mkQualifiedUserClientPrekeyMap $
                  Map.singleton domain $
                    mkUserClientPrekeyMap $
                      Map.fromList $
                        xs <&> \(uid', c, _lpk, cpk) ->
                          (uid', Map.singleton (clientId c) (Just (prekeyData cpk))),
            failedToList = Nothing
          }

  post
    ( brig
        . paths ["users", "list-prekeys"]
        . contentJson
        . body (RequestBodyLBS $ encode userClients)
        . zUser uid
    )
    !!! do
      const 200 === statusCode
      const (Right $ expectedUserClientMap) === responseJsonEither

-- The testTooManyClients test conforms to the following testing standards:
-- @SF.Provisioning @TSFI.RESTfulAPI @S2
--
-- The test validates the upper bound on the number of permanent clients per
-- user. It does so by trying to create one permanent client more than allowed.
-- The expected outcome is that all the clients up to the limit are successfully
-- created, but the one over the limit is not (error `404 too-many-clients`).
testTooManyClients :: Opt.Opts -> Brig -> Http ()
testTooManyClients opts brig = do
  uid <- userId <$> randomUser brig
  -- We can always change the permanent client limit
  let newOpts = opts & Opt.optionSettings . Opt.userMaxPermClients ?~ 1
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
      const (Just "application/json") === getHeader "Content-Type"

-- Ensure that the list of prekeys for a user does not become empty, and the
-- last resort prekey keeps being returned if it's the only key left.
-- Test with featureFlag randomPrekeys=true
testPrekeysNotEmptyRandomPrekeys :: Opt.Opts -> Brig -> Http ()
testPrekeysNotEmptyRandomPrekeys opts brig = do
  -- Run the test for randomPrekeys (not dynamoDB locking)
  let newOpts = opts {Opt.randomPrekeys = Just True}
  ensurePrekeysNotEmpty newOpts brig

ensurePrekeysNotEmpty :: Opt.Opts -> Brig -> Http ()
ensurePrekeysNotEmpty opts brig = withSettingsOverrides opts $ do
  lgr <- Log.new Log.defSettings
  uid <- userId <$> randomUser brig
  -- Create a client with 1 regular prekey and 1 last resort prekey
  c <- responseJsonError =<< addClient brig uid (defNewClient PermanentClientType [somePrekeys !! 10] (someLastPrekeys !! 10))
  -- Claim the first regular one
  _rs1 <- getPreKey brig uid uid (clientId c) <!! const 200 === statusCode
  -- Claim again; this should give the last resort one
  rs2 <- getPreKey brig uid uid (clientId c) <!! const 200 === statusCode
  let pId2 = prekeyId . prekeyData <$> responseJsonMaybe rs2
  liftIO $ assertEqual "last prekey rs2" (Just lastPrekeyId) pId2
  liftIO $ Log.warn lgr (Log.msg (Log.val "First claim of last resort successful, claim again..."))
  -- Claim again; this should (again) give the last resort one
  rs3 <- getPreKey brig uid uid (clientId c) <!! const 200 === statusCode
  let pId3 = prekeyId . prekeyData <$> responseJsonMaybe rs3
  liftIO $ assertEqual "last prekey rs3" (Just lastPrekeyId) pId3

testRegularPrekeysCannotBeSentAsLastPrekeys :: Brig -> Http ()
testRegularPrekeysCannotBeSentAsLastPrekeys brig = do
  uid <- userId <$> randomUser brig
  -- The parser should reject a normal prekey in the lastPrekey field
  addClient brig uid (defNewClient PermanentClientType [head somePrekeys] fakeLastPrekey) !!! const 400 === statusCode

testRegularPrekeysCannotBeSentAsLastPrekeysDuringUpdate :: Brig -> Http ()
testRegularPrekeysCannotBeSentAsLastPrekeysDuringUpdate brig = do
  uid <- userId <$> randomUser brig
  c <- responseJsonError =<< addClient brig uid (defNewClient PermanentClientType [head somePrekeys] (someLastPrekeys !! 11)) <!! const 201 === statusCode
  let newPrekey = somePrekeys !! 2
  let update =
        defUpdateClient
          { updateClientPrekeys = [newPrekey],
            updateClientLastKey = Just fakeLastPrekey,
            updateClientLabel = Just "label"
          }
  -- The parser should reject a normal prekey in the lastPrekey field
  put
    ( brig
        . paths ["clients", toByteString' (clientId c)]
        . zUser uid
        . contentJson
        . body (RequestBodyLBS $ encode update)
    )
    !!! const 400
      === statusCode

-- @END

-- The testRemoveClient test conforms to the following testing standards:
-- @SF.Provisioning @TSFI.RESTfulAPI @S2
--
-- This test validates creating and deleting a client. A client is created and
-- consequently deleted. Deleting a second time yields response 404 not found.
-- Prekeys and cookies are not there anymore once the client is deleted.
testRemoveClient :: Bool -> Brig -> Cannon -> Http ()
testRemoveClient hasPwd brig cannon = do
  u <- randomUser' hasPwd brig
  let uid = userId u
  let Just email = userEmail u
  -- Permanent client with attached cookie
  when hasPwd $ do
    login brig (defEmailLogin email) PersistentCookie
      !!! const 200
        === statusCode
    numCookies <- countCookies brig uid defCookieLabel
    liftIO $ Just 1 @=? numCookies
  c <- responseJsonError =<< addClient brig uid (client PermanentClientType (someLastPrekeys !! 10))
  when hasPwd $ do
    -- Missing password
    deleteClient brig uid (clientId c) Nothing !!! const 403 === statusCode
  -- Success
  WS.bracketR cannon uid $ \ws -> do
    deleteClient brig uid (clientId c) (if hasPwd then Just defPasswordText else Nothing)
      !!! const 200
        === statusCode
    void . liftIO . WS.assertMatch (5 # Second) ws $ \n -> do
      let j = Object $ List1.head (ntfPayload n)
      let etype = j ^? key "type" . _String
      let eclient = j ^? key "client" . key "id" . _String
      etype @?= Just "user.client-remove"
      (fromByteString . T.encodeUtf8 =<< eclient) @?= Just (clientId c)
  -- Not found on retry
  deleteClient brig uid (clientId c) Nothing !!! const 404 === statusCode
  -- Prekeys are gone
  getPreKey brig uid uid (clientId c) !!! const 404 === statusCode
  -- Cookies are gone
  numCookies' <- countCookies brig (userId u) defCookieLabel
  liftIO $ Just 0 @=? numCookies'
  where
    client ty lk =
      (defNewClient ty [somePrekeys !! 0] lk)
        { newClientLabel = Just "Nexus 5x",
          newClientCookie = Just defCookieLabel
        }

-- @END

-- The testRemoveClientShortPwd test conforms to the following testing standards:
-- @SF.Provisioning @TSFI.RESTfulAPI @S2
--
-- The test checks if a client can be deleted by providing a too short password.
-- This is done by using a single-character password, whereas the minimum is 6
-- characters. The client deletion attempt fails as expected.
testRemoveClientShortPwd :: Brig -> Http ()
testRemoveClientShortPwd brig = do
  u <- randomUser brig
  let uid = userId u
  let Just email = userEmail u
  -- Permanent client with attached cookie
  login brig (defEmailLogin email) PersistentCookie
    !!! const 200
      === statusCode
  numCookies <- countCookies brig uid defCookieLabel
  liftIO $ Just 1 @=? numCookies
  c <- responseJsonError =<< addClient brig uid (client PermanentClientType (someLastPrekeys !! 10))
  resp <-
    deleteClient brig uid (clientId c) (Just "a")
      <!! const 400
        === statusCode
  err :: Object <- responseJsonError resp
  liftIO $ do
    (err ^. at "code") @?= Just (Number 400)
    (err ^. at "label") @?= Just (String "bad-request")
    (err ^. at "message") @?= Just (String "Error in $.password: outside range [6, 1024]")
  where
    client ty lk =
      (defNewClient ty [somePrekeys !! 0] lk)
        { newClientLabel = Just "Nexus 5x",
          newClientCookie = Just defCookieLabel
        }

-- @END

-- The testRemoveClientIncorrectPwd test conforms to the following testing standards:
-- @SF.Provisioning @TSFI.RESTfulAPI @S2
--
-- The test checks if a client can be deleted by providing a syntax-valid, but
-- incorrect password. The client deletion attempt fails with a 403 error
-- response.
testRemoveClientIncorrectPwd :: Brig -> Http ()
testRemoveClientIncorrectPwd brig = do
  u <- randomUser brig
  let uid = userId u
  let Just email = userEmail u
  -- Permanent client with attached cookie
  login brig (defEmailLogin email) PersistentCookie
    !!! const 200
      === statusCode
  numCookies <- countCookies brig uid defCookieLabel
  liftIO $ Just 1 @=? numCookies
  c <- responseJsonError =<< addClient brig uid (client PermanentClientType (someLastPrekeys !! 10))
  resp <-
    deleteClient brig uid (clientId c) (Just "abcdef")
      <!! const 403
        === statusCode
  err :: Object <- responseJsonError resp
  liftIO $ do
    (err ^. at "code") @?= Just (Number 403)
    (err ^. at "label") @?= Just (String "invalid-credentials")
    (err ^. at "message") @?= Just (String "Authentication failed")
  where
    client ty lk =
      (defNewClient ty [somePrekeys !! 0] lk)
        { newClientLabel = Just "Nexus 5x",
          newClientCookie = Just defCookieLabel
        }

-- @END

testUpdateClient :: Opt.Opts -> Brig -> Http ()
testUpdateClient opts brig = do
  uid <- userId <$> randomUser brig
  let clt =
        (defNewClient TemporaryClientType [somePrekeys !! 0] (someLastPrekeys !! 0))
          { newClientClass = Just PhoneClient,
            newClientModel = Just "featurephone"
          }
  c <- responseJsonError =<< addClient brig uid clt
  get (apiVersion "v1" . brig . paths ["users", toByteString' uid, "prekeys", toByteString' (clientId c)] . zUser uid) !!! do
    const 200 === statusCode
    const (Just $ ClientPrekey (clientId c) (somePrekeys !! 0)) === responseJsonMaybe
  getClient brig uid (clientId c) !!! do
    const 200 === statusCode
    const (Just "Test Device") === (clientLabel <=< responseJsonMaybe)
    const (Just PhoneClient) === (clientClass <=< responseJsonMaybe)
    const (Just "featurephone") === (clientModel <=< responseJsonMaybe)
  let newPrekey = somePrekeys !! 2
  let update =
        defUpdateClient
          { updateClientPrekeys = [newPrekey],
            updateClientLabel = Just "label"
          }
  put
    ( brig
        . paths ["clients", toByteString' (clientId c)]
        . zUser uid
        . contentJson
        . body (RequestBodyLBS $ encode update)
    )
    !!! const 200
      === statusCode
  get (apiVersion "v1" . brig . paths ["users", toByteString' uid, "prekeys", toByteString' (clientId c)] . zUser uid) !!! do
    const 200 === statusCode
    const (Just $ ClientPrekey (clientId c) newPrekey) === responseJsonMaybe

  -- check if label has been updated
  getClient brig uid (clientId c) !!! do
    const 200 === statusCode
    const (Just "label") === (clientLabel <=< responseJsonMaybe)

  -- via `/users/:uid/clients/:client`, only `id` and `class` are visible:
  get (apiVersion "v1" . brig . paths ["users", toByteString' uid, "clients", toByteString' (clientId c)]) !!! do
    const 200 === statusCode
    const (Just $ clientId c) === (fmap pubClientId . responseJsonMaybe)
    const (Just PhoneClient) === (pubClientClass <=< responseJsonMaybe)
    const Nothing === (preview (key "label") <=< responseJsonMaybe @Value)
    const Nothing === (preview (key "mls_public_keys") <=< responseJsonMaybe @Value)

  -- via `/users/:domain/:uid/clients/:client`, only `id` and `class` are visible:
  let localdomain = opts ^. Opt.optionSettings & Opt.setFederationDomain
  get (brig . paths ["users", toByteString' localdomain, toByteString' uid, "clients", toByteString' (clientId c)]) !!! do
    const 200 === statusCode
    const (Just $ clientId c) === (fmap pubClientId . responseJsonMaybe)
    const (Just PhoneClient) === (pubClientClass <=< responseJsonMaybe)
    const Nothing === (preview (key "label") <=< responseJsonMaybe @Value)
    const Nothing === (preview (key "mls_public_keys") <=< responseJsonMaybe @Value)

  let update' = defUpdateClient

  -- empty update should be a no-op
  put
    ( apiVersion "v1"
        . brig
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

  -- update supported client capabilities work
  let checkUpdate :: HasCallStack => Maybe [ClientCapability] -> Bool -> [ClientCapability] -> Http ()
      checkUpdate capsIn respStatusOk capsOut = do
        let update'' = defUpdateClient {updateClientCapabilities = Set.fromList <$> capsIn}
        put
          ( apiVersion "v1"
              . brig
              . paths ["clients", toByteString' (clientId c)]
              . zUser uid
              . contentJson
              . body (RequestBodyLBS $ encode update'')
          )
          !!! if respStatusOk
            then do
              const 200 === statusCode
            else do
              const 409 === statusCode
              const (Just "client-capabilities-cannot-be-removed") === fmap Error.label . responseJsonMaybe

        getClientCapabilities brig uid (clientId c) !!! do
          const 200 === statusCode
          const (Just (ClientCapabilityList (Set.fromList capsOut))) === responseJsonMaybe

  checkUpdate (Just [ClientSupportsLegalholdImplicitConsent]) True [ClientSupportsLegalholdImplicitConsent]
  checkUpdate Nothing True [ClientSupportsLegalholdImplicitConsent]
  checkUpdate (Just []) False [ClientSupportsLegalholdImplicitConsent]

  -- update supported client capabilities don't break prekeys or label
  do
    let checkClientLabel :: HasCallStack => Http ()
        checkClientLabel = do
          getClient brig uid (clientId c) !!! do
            const 200 === statusCode
            const (Just label) === (clientLabel <=< responseJsonMaybe)

        flushClientPrekey :: HasCallStack => Http (Maybe ClientPrekey)
        flushClientPrekey = do
          responseJsonMaybe
            <$> ( get
                    (apiVersion "v1" . brig . paths ["users", toByteString' uid, "prekeys", toByteString' (clientId c)] . zUser uid)
                    <!! const 200
                      === statusCode
                )

        checkClientPrekeys :: HasCallStack => Prekey -> Http ()
        checkClientPrekeys expectedPrekey = do
          flushClientPrekey >>= \case
            Nothing -> error "unexpected."
            Just (ClientPrekey cid' prekey') -> liftIO $ do
              assertEqual "" (clientId c) cid'
              assertEqual "" expectedPrekey prekey'

        caps = Just $ Set.fromList [ClientSupportsLegalholdImplicitConsent]

        label = "label-bc1b7b0c-b7bf-11eb-9a1d-233d397f934a"
        prekey = somePrekeys !! 4
        lastprekey = someLastPrekeys !! 4

    void $ flushClientPrekey >> flushClientPrekey
    put
      ( brig
          . paths ["clients", toByteString' (clientId c)]
          . zUser uid
          . json
            defUpdateClient
              { updateClientPrekeys = [prekey],
                updateClientLastKey = Just lastprekey,
                updateClientLabel = Just label
              }
      )
      !!! const 200
        === statusCode
    checkClientLabel
    put
      ( brig
          . paths ["clients", toByteString' (clientId c)]
          . zUser uid
          . json defUpdateClient {updateClientCapabilities = caps}
      )
      !!! const 200
        === statusCode
    checkClientLabel
    checkClientPrekeys prekey
    checkClientPrekeys (unpackLastPrekey lastprekey)

testMLSPublicKeyUpdate :: Brig -> Http ()
testMLSPublicKeyUpdate brig = do
  uid <- userId <$> randomUser brig
  let clt =
        (defNewClient TemporaryClientType [somePrekeys !! 0] (someLastPrekeys !! 0))
          { newClientClass = Just PhoneClient,
            newClientModel = Just "featurephone"
          }
  c <- responseJsonError =<< addClient brig uid clt
  let keys = Map.fromList [(Ed25519, "aGVsbG8gd29ybGQ=")]
  putClient brig uid (clientId c) keys !!! const 200 === statusCode
  c' <- responseJsonError =<< getClient brig uid (clientId c) <!! const 200 === statusCode
  liftIO $ clientMLSPublicKeys c' @?= keys
  -- adding the key again should fail
  putClient brig uid (clientId c) keys !!! const 400 === statusCode

testMissingClient :: Brig -> Http ()
testMissingClient brig = do
  uid <- userId <$> randomUser brig
  c <- liftIO $ generate arbitrary
  getClient brig uid c !!! do
    const 404 === statusCode
    -- This is unfortunate, but fixing this breaks clients.
    const Nothing === responseBody
    const ["text/plain;charset=utf-8"]
      === map snd
        . filter ((== "Content-Type") . fst)
        . responseHeaders

-- The testAddMultipleTemporary test conforms to the following testing standards:
-- @SF.Provisioning @TSFI.RESTfulAPI @S2
-- Legacy (galley)
--
-- Add temporary client, check that all services (both galley and
-- brig) have registered it.  Add second temporary client, check
-- again.  (NB: temp clients replace each other, there can always be
-- at most one per account.)
testAddMultipleTemporary :: HasCallStack => Brig -> Galley -> Cannon -> Http ()
testAddMultipleTemporary brig galley cannon = do
  uid <- userId <$> randomUser brig
  let clt1 =
        (defNewClient TemporaryClientType [somePrekeys !! 0] (someLastPrekeys !! 0))
          { newClientClass = Just PhoneClient,
            newClientModel = Just "featurephone1"
          }

  client <- responseJsonError =<< addClient brig uid clt1

  brigClients1 <- numOfBrigClients uid
  galleyClients1 <- numOfGalleyClients uid
  liftIO $ assertEqual "Too many clients found" (Just 1) brigClients1
  liftIO $ assertEqual "Too many clients found" (Just 1) galleyClients1
  let clt2 =
        (defNewClient TemporaryClientType [somePrekeys !! 1] (someLastPrekeys !! 1))
          { newClientClass = Just PhoneClient,
            newClientModel = Just "featurephone2"
          }

  brigClients2 <- numOfBrigClients uid
  liftIO $ assertEqual "Too many clients found" (Just 1) brigClients2

  WS.bracketR cannon uid $ \ws -> do
    _ <- addClient brig uid clt2
    void . liftIO . WS.assertMatch (5 # Second) ws $ \n -> do
      let j = Object $ List1.head (ntfPayload n)
      let etype = j ^? key "type" . _String
      let eclient = j ^? key "client" . key "id" . _String
      etype @?= Just "user.client-remove"
      (fromByteString . T.encodeUtf8 =<< eclient) @?= Just (clientId client)

  galleyClients2 <- numOfGalleyClients uid
  liftIO $ assertEqual "Too many clients found" (Just 1) galleyClients2
  where
    numOfBrigClients u = do
      r <-
        get $
          brig
            . path "clients"
            . zUser u
      pure $ Vec.length <$> (preview _Array =<< responseJsonMaybe @Value r)
    numOfGalleyClients u = do
      r <-
        get $
          galley
            . path "i/test/clients"
            . zUser u
      pure $ Vec.length <$> (preview _Array =<< responseJsonMaybe @Value r)

-- @END

testPreKeyRace :: Brig -> Http ()
testPreKeyRace brig = do
  uid <- userId <$> randomUser brig
  let pks = map (\i -> somePrekeys !! i) [1 .. 10]
  c <- responseJsonError =<< addClient brig uid (defNewClient PermanentClientType pks (someLastPrekeys !! 0))
  pks' <- flip mapConcurrently pks $ \_ -> do
    rs <- getPreKey brig uid uid (clientId c) <!! const 200 === statusCode
    pure $ prekeyId . prekeyData <$> responseJsonMaybe rs
  -- We should not hand out regular prekeys more than once (i.e. at most once).
  let actual = catMaybes pks'
  liftIO $ assertEqual "insufficient prekeys" (length pks) (length actual)
  let regular = filter (/= lastPrekeyId) actual
  liftIO $ assertEqual "duplicate prekeys" (length regular) (length (nub regular))
  deleteClient brig uid (clientId c) (Just defPasswordText) !!! const 200 === statusCode

testNewNonce :: Brig -> Http ()
testNewNonce brig = do
  n1 <- check Util.getNonce 204
  n2 <- check Util.headNonce 200
  lift $ assertBool "nonces should not be equal" (n1 /= n2)
  where
    check f status = do
      uid <- userId <$> randomUser brig
      cid <- randomClient
      response <- f brig uid cid <!! const status === statusCode
      let nonceBs = getHeader "Replay-Nonce" response
      liftIO $ do
        assertBool "Replay-Nonce header should contain a valid base64url encoded uuidv4" $ any isValidBase64UrlEncodedUUID nonceBs
        Just "no-store" @=? getHeader "Cache-Control" response
      pure nonceBs

data DPoPClaimsSet = DPoPClaimsSet
  { jwtClaims :: ClaimsSet,
    claimNonce :: Text,
    claimHtm :: Text,
    claimHtu :: Text,
    claimChal :: Text,
    claimHandle :: Text,
    claimDisplayName :: Text,
    claimTeamId :: Text
  }
  deriving (Eq, Show, Generic)

instance HasClaimsSet DPoPClaimsSet where
  claimsSet f s = fmap (\a' -> s {jwtClaims = a'}) (f (jwtClaims s))

instance A.FromJSON DPoPClaimsSet where
  parseJSON = A.withObject "OAuthClaimsSet" $ \o ->
    DPoPClaimsSet
      <$> A.parseJSON (A.Object o)
      <*> o A..: "nonce"
      <*> o A..: "htm"
      <*> o A..: "htu"
      <*> o A..: "chal"
      <*> o A..: "handle"
      <*> o A..: "name"
      <*> o A..: "team"

instance A.ToJSON DPoPClaimsSet where
  toJSON s =
    ins "nonce" (claimNonce s) (A.toJSON (jwtClaims s))
      & ins "htm" (claimHtm s)
      & ins "htu" (claimHtu s)
      & ins "chal" (claimChal s)
      & ins "handle" (claimHandle s)
      & ins "name" (claimDisplayName s)
      & ins "team" (claimTeamId s)
    where
      ins k v (Object o) = Object $ M.insert k (A.toJSON v) o
      ins _ _ a = a

testCreateAccessToken :: Opts.Opts -> Nginz -> Brig -> Http ()
testCreateAccessToken opts n brig = do
  let localDomain = opts ^. Opt.optionSettings & Opt.setFederationDomain
  (u, tid) <- Util.createUserWithTeam' brig
  handle <- do
    Just h <- userHandle <$> Util.setRandomHandle brig u
    pure $ "wireapp://%40" <> fromHandle h <> "@" <> cs (toByteString' localDomain)
  let uid = userId u
  let Just email = userEmail u
  -- convert the user Id into 16 octets of binary and then base64url
  let uidBS = Data.UUID.toByteString (toUUID uid)
  let uidB64 = encodeBase64UrlUnpadded (cs uidBS)
  rs <-
    login n (defEmailLogin email) PersistentCookie
      <!! const 200 === statusCode
  let t = decodeToken rs
  cid <- createClientForUser brig uid
  nonceResponse <- Util.headNonceNginz n t cid <!! const 200 === statusCode
  let nonceBs = cs $ fromMaybe (error "invalid nonce") $ getHeader "Replay-Nonce" nonceResponse
  now <- liftIO $ posixSecondsToUTCTime . fromInteger <$> (floor <$> getPOSIXTime)
  let clientIdentity = cs $ "wireapp://" <> cs (toText uidB64) <> "!" <> toByteString' cid <> "@" <> toByteString' localDomain
  let httpsUrl = cs $ "https://" <> toByteString' localDomain <> "/clients/" <> toByteString' cid <> "/access-token"
  let expClaim = NumericDate $ addUTCTime 10 now
  let claimsSet' =
        emptyClaimsSet
          & claimIat ?~ NumericDate now
          & claimExp ?~ expClaim
          & claimNbf ?~ NumericDate now
          & claimSub ?~ fromMaybe (error "invalid sub claim") ((clientIdentity :: Text) ^? stringOrUri)
          & claimJti ?~ "6fc59e7f-b666-4ffc-b738-4f4760c884ca"
          & claimAud ?~ (maybe (error "invalid sub claim") (Audience . (: [])) (("https://wire.com/acme/challenge/abcd" :: Text) ^? stringOrUri))
  let dpopClaims =
        DPoPClaimsSet
          claimsSet'
          nonceBs
          "POST"
          httpsUrl
          "wa2VrkCtW1sauJ2D3uKY8rc7y4kl4usH"
          handle
          (fromName u.userDisplayName)
          (UUID.toText (toUUID tid))
  signedOrError <- fmap encodeCompact <$> liftIO (signAccessToken dpopClaims)
  case signedOrError of
    Left err -> liftIO $ assertFailure $ "failed to sign claims: " <> show err
    Right signed -> do
      let accessControlExposeHeaders = maybe "" cs $ getHeader "Access-Control-Expose-Headers" nonceResponse
      liftIO $ assertBool "Access-Control-Expose-Headers should contain Replay-Nonce" $ "Replay-Nonce" `isInfixOf` accessControlExposeHeaders
      let proof = Just $ Proof (cs signed)
      response <- Util.createAccessTokenNginz n t cid proof
      let accessToken = fromRight (error $ "failed to create token: " <> show response) $ responseJsonEither response
      liftIO $ datrType accessToken @?= DPoP
  where
    signAccessToken :: DPoPClaimsSet -> IO (Either JWTError SignedJWT)
    signAccessToken claims = runJOSE $ do
      algo <- bestJWSAlg jwkKey
      let h =
            newJWSHeader ((), algo)
              & (jwk ?~ HeaderParam () jwkPubKey)
              & (typ ?~ HeaderParam () "dpop+jwt")
      signJWT jwkKey h claims

    jwkKey :: JWK
    jwkKey = do
      fromMaybe (error "invalid jwk") . A.decode $
        "{\"kty\":\"OKP\",\"d\":\"9RVey-A7ENCO5e9fDiPf8An1gkVBmAhFgu4WZMVwS-A\",\"crv\":\"Ed25519\",\"x\":\"nLJGN-Oa6Jsq3KclZggLl7UvAVdmB0Q6C3N5BCgphHw\"}"

    jwkPubKey :: JWK
    jwkPubKey = do
      fromMaybe (error "invalid jwk") . A.decode $
        "{\"kty\":\"OKP\",\"crv\":\"Ed25519\",\"x\":\"nLJGN-Oa6Jsq3KclZggLl7UvAVdmB0Q6C3N5BCgphHw\"}"

testCreateAccessTokenMissingProof :: Brig -> Http ()
testCreateAccessTokenMissingProof brig = do
  uid <- userId <$> randomUser brig
  cid <- createClientForUser brig uid
  let mProof = Nothing
  Util.createAccessToken brig uid "some_host_name" cid mProof
    !!! do
      const 400 === statusCode

testCreateAccessTokenNoNonce :: Brig -> Http ()
testCreateAccessTokenNoNonce brig = do
  (u, _) <- Util.createUserWithTeam' brig
  void $ Util.setRandomHandle brig u
  let uid = userId u
  cid <- createClientForUser brig uid
  Util.createAccessToken brig uid "some_host_name" cid (Just $ Proof "xxxx.yyyy.zzzz")
    !!! do
      const 400 === statusCode
      const (Just "client-token-bad-nonce") === fmap Error.label . responseJsonMaybe

createClientForUser :: Brig -> UserId -> Http ClientId
createClientForUser brig uid =
  clientId <$> (responseJsonError =<< addClient brig uid (defNewClient PermanentClientType [head somePrekeys] (head someLastPrekeys)))
