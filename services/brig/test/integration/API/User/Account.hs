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

module API.User.Account
  ( tests,
  )
where

import qualified API.Search.Util as Search
import API.Team.Util hiding (listConnections)
import API.User.Util
import Bilge hiding (accept, timeout)
import Bilge.Assert
import qualified Brig.AWS as AWS
import Brig.AWS.Types
import qualified Brig.Options as Opt
import Brig.Types
import Brig.Types.Intra
import Brig.Types.User.Auth hiding (user)
import qualified Brig.Types.User.Auth as Auth
import qualified CargoHold.Types.V3 as CHV3
import Control.Arrow ((&&&))
import Control.Lens ((^.), (^?))
import Control.Monad.Catch
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Aeson.Lens as AesonL
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Conversion
import Data.Id hiding (client)
import Data.Json.Util (fromUTCTimeMillis)
import Data.List1 (singleton)
import qualified Data.List1 as List1
import Data.Misc (PlainTextPassword (..))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock (diffUTCTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Galley.Types.Teams (noPermissions)
import Gundeck.Types.Notification
import Imports
import qualified Network.Wai.Utilities.Error as Error
import Test.Tasty hiding (Timeout)
import Test.Tasty.Cannon hiding (Cannon)
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import UnliftIO (mapConcurrently_)
import Util as Util
import Util.AWS as Util
import Web.Cookie (parseSetCookie)

tests :: ConnectionLimit -> Opt.Timeout -> Opt.Opts -> Manager -> Brig -> Cannon -> CargoHold -> Galley -> AWS.Env -> TestTree
tests _ at opts p b c ch g aws =
  testGroup
    "account"
    [ test' aws p "post /register - 201 (with preverified)" $ testCreateUserWithPreverified opts b aws,
      test' aws p "post /register - 201" $ testCreateUser b g,
      test' aws p "post /register - 201 + no email" $ testCreateUserNoEmailNoPassword b,
      test' aws p "post /register - 201 anonymous" $ testCreateUserAnon b g,
      test' aws p "post /register - 201 anonymous expiry" $ testCreateUserAnonExpiry b,
      test' aws p "post /register - 201 pending" $ testCreateUserPending opts b,
      test' aws p "post /register - 201 existing activation" $ testCreateAccountPendingActivationKey opts b,
      test' aws p "post /register - 409 conflict" $ testCreateUserConflict opts b,
      test' aws p "post /register - 400 invalid input" $ testCreateUserInvalidEmailOrPhone opts b,
      test' aws p "post /register - 403 blacklist" $ testCreateUserBlacklist opts b aws,
      test' aws p "post /register - 400 external-SSO" $ testCreateUserExternalSSO b,
      test' aws p "post /register - 403 restricted user creation" $ testRestrictedUserCreation opts b,
      test' aws p "post /activate - 200/204 + expiry" $ testActivateWithExpiry opts b at,
      test' aws p "get /users/:uid - 404" $ testNonExistingUser b,
      test' aws p "get /users/:uid - 200" $ testExistingUser b,
      test' aws p "get /users?:id=.... - 200" $ testMultipleUsers b,
      test' aws p "put /self - 200" $ testUserUpdate b c aws,
      test' aws p "put /self/email - 2xx" $ testEmailUpdate b aws,
      test' aws p "put /self/phone - 202" $ testPhoneUpdate b,
      test' aws p "head /self/password - 200/404" $ testPasswordSet b,
      test' aws p "put /self/password - 200" $ testPasswordChange b,
      test' aws p "put /self/locale - 200" $ testUserLocaleUpdate b aws,
      test' aws p "post /activate/send - 200" $ testSendActivationCode opts b,
      test' aws p "post /activate/send - 400 invalid input" $ testSendActivationCodeInvalidEmailOrPhone b,
      test' aws p "post /activate/send - 403 prefix excluded" $ testSendActivationCodePrefixExcluded b,
      test' aws p "post /i/users/phone-prefix" $ testInternalPhonePrefixes b,
      test' aws p "put /i/users/:uid/status (suspend)" $ testSuspendUser b,
      test' aws p "get /i/users?:(email|phone) - 200" $ testGetByIdentity b,
      test' aws p "delete/phone-email" $ testEmailPhoneDelete b c,
      test' aws p "delete/by-password" $ testDeleteUserByPassword b c aws,
      test' aws p "delete/with-legalhold" $ testDeleteUserWithLegalHold b c aws,
      test' aws p "delete/by-code" $ testDeleteUserByCode b,
      test' aws p "delete/anonymous" $ testDeleteAnonUser b,
      test' aws p "delete /i/users/:uid - 202" $ testDeleteInternal b c aws,
      test' aws p "delete with profile pic" $ testDeleteWithProfilePic b ch,
      test' aws p "put /i/users/:uid/sso-id" $ testUpdateSSOId b g,
      testGroup
        "temporary customer extensions"
        [ test' aws p "domains blocked for registration" $ testDomainsBlockedForRegistration opts b
        ]
    ]

testCreateUserWithPreverified :: Opt.Opts -> Brig -> AWS.Env -> Http ()
testCreateUserWithPreverified opts brig aws = do
  -- Register (pre verified) user with phone
  p <- randomPhone
  let phoneReq = RequestBodyLBS . encode $ object ["phone" .= fromPhone p]
  post (brig . path "/activate/send" . contentJson . body phoneReq)
    !!! (const 200 === statusCode)
  getActivationCode brig (Right p) >>= \case
    Nothing -> liftIO $ assertFailure "missing activation key/code"
    Just (_, c) -> do
      let Object reg =
            object
              [ "name" .= Name "Alice",
                "phone" .= fromPhone p,
                "phone_code" .= c
              ]
      if Opt.setRestrictUserCreation (Opt.optSettings opts) == Just True
        then do
          postUserRegister' reg brig !!! const 403 === statusCode
        else do
          usr <- postUserRegister reg brig
          let uid = userId usr
          get (brig . path "/self" . zUser uid) !!! do
            const 200 === statusCode
            const (Just p) === (userPhone <=< responseJsonMaybe)
          liftIO $ Util.assertUserJournalQueue "user activate" aws (userActivateJournaled usr)
  -- Register (pre verified) user with email
  e <- randomEmail
  let emailReq = RequestBodyLBS . encode $ object ["email" .= fromEmail e]
  post (brig . path "/activate/send" . contentJson . body emailReq)
    !!! (const 200 === statusCode)
  getActivationCode brig (Left e) >>= \case
    Nothing -> liftIO $ assertFailure "missing activation key/code"
    Just (_, c) -> do
      let Object reg =
            object
              [ "name" .= Name "Alice",
                "email" .= fromEmail e,
                "email_code" .= c
              ]
      if Opt.setRestrictUserCreation (Opt.optSettings opts) == Just True
        then do
          postUserRegister' reg brig !!! const 403 === statusCode
        else do
          usr <- postUserRegister reg brig
          let uid = userId usr
          get (brig . path "/self" . zUser uid) !!! do
            const 200 === statusCode
            const (Just e) === (userEmail <=< responseJsonMaybe)
          liftIO $ Util.assertUserJournalQueue "user activate" aws (userActivateJournaled usr)
  liftIO $ Util.assertEmptyUserJournalQueue aws

testCreateUser :: Brig -> Galley -> Http () -- TODO: this has nothing to do with /register.  what's going on here?
testCreateUser brig galley = do
  uid <- userId <$> randomUser brig
  get (galley . path "conversations" . zAuthAccess uid "conn") !!! do
    const 200 === statusCode
    -- check number of conversations:
    const (Just 1) === \r -> do
      b <- responseBody r
      c <- b ^? key "conversations"
      Vec.length <$> (maybeFromJSON c :: Maybe (Vector Value))
    -- check conversation type:
    const (Just (1 :: Integer)) === \r -> do
      b <- responseBody r
      b ^? key "conversations" . nth 0 . key "type" >>= maybeFromJSON

testCreateUserAnon :: Brig -> Galley -> Http ()
testCreateUserAnon brig galley = do
  let p =
        RequestBodyLBS . encode $
          object
            ["name" .= ("Mr. Pink" :: Text)]
  rs <-
    post (brig . path "/register" . contentJson . body p)
      <!! const 201 === statusCode
  -- Every registered user gets a cookie.
  let zuid = parseSetCookie <$> getHeader "Set-Cookie" rs
  liftIO $ assertBool "Missing zuid cookie" (isJust zuid)
  -- Every registered user gets a self conversation.
  let Just uid = userId <$> responseJsonMaybe rs
  get (galley . path "conversations" . zAuthAccess uid "conn") !!! do
    const 200 === statusCode
    -- check number of conversations:
    const (Just 1) === \r -> do
      b <- responseBody r
      c <- b ^? key "conversations"
      Vec.length <$> (maybeFromJSON c :: Maybe (Vector Value))
    -- check conversation type:
    const (Just (1 :: Integer)) === \r -> do
      b <- responseBody r
      b ^? key "conversations" . nth 0 . key "type" >>= maybeFromJSON
  -- should not appear in search
  suid <- userId <$> randomUser brig
  Search.refreshIndex brig
  Search.assertCan'tFind brig suid uid "Mr. Pink"

testCreateUserPending :: Opt.Opts -> Brig -> Http ()
testCreateUserPending (Opt.setRestrictUserCreation . Opt.optSettings -> Just True) _ = pure ()
testCreateUserPending _ brig = do
  e <- randomEmail
  let p =
        RequestBodyLBS . encode $
          object
            [ "name" .= ("Mr. Pink" :: Text),
              "email" .= fromEmail e,
              "password" .= defPassword
            ]
  rs <-
    post (brig . path "/register" . contentJson . body p)
      <!! const 201 === statusCode
  -- Even though activation is pending, the user gets an access cookie,
  -- i.e. every user starts out as a "Wireless" user.
  let zuid = parseSetCookie <$> getHeader "Set-Cookie" rs
  liftIO $ assertBool "Missing zuid cookie" (isJust zuid)
  -- Cannot login via email (pending activation)
  login brig (defEmailLogin e) PersistentCookie !!! do
    const 403 === statusCode
    const (Just "pending-activation") === fmap Error.label . responseJsonMaybe
  -- The user has no verified / activated identity yet
  let Just uid = userId <$> responseJsonMaybe rs
  get (brig . path "/self" . zUser uid) !!! do
    const 200 === statusCode
    const (Just True) === \rs' -> do
      self <- responseJsonMaybe rs'
      return $! isNothing (userIdentity (selfUser self))
  -- should not appear in search
  suid <- userId <$> randomUser brig
  Search.refreshIndex brig
  Search.assertCan'tFind brig suid uid "Mr. Pink"

testCreateUserNoEmailNoPassword :: Brig -> Http ()
testCreateUserNoEmailNoPassword brig = do
  p <- randomPhone
  let newUser =
        RequestBodyLBS . encode $
          object
            [ "name" .= ("Alice" :: Text),
              "phone" .= fromPhone p
            ]
  rs <-
    post (brig . path "/i/users" . contentJson . body newUser)
      <!! const 201 === statusCode
  let Just uid = userId <$> responseJsonMaybe rs
  e <- randomEmail
  let setEmail = RequestBodyLBS . encode $ EmailUpdate e
  put (brig . path "/self/email" . contentJson . zUser uid . zConn "conn" . body setEmail)
    !!! const 202 === statusCode

-- | email address must not be taken on @/register@.
testCreateUserConflict :: Opt.Opts -> Brig -> Http ()
testCreateUserConflict (Opt.setRestrictUserCreation . Opt.optSettings -> Just True) _ = pure ()
testCreateUserConflict _ brig = do
  -- trusted email domains
  u <- createUser "conflict" brig
  let p =
        RequestBodyLBS . encode $
          object
            [ "name" .= ("conflict1" :: Text),
              "email" .= (fromEmail <$> userEmail u), -- dup. email
              "password" .= defPassword
            ]
  post (brig . path "/register" . contentJson . body p) !!! do
    const 409 === statusCode
    const (Just "key-exists") === fmap Error.label . responseJsonMaybe
  -- untrusted email domains
  u2 <- createUserUntrustedEmail "conflict" brig
  let Just (Email loc dom) = userEmail u2
  let p2 =
        RequestBodyLBS . encode $
          object
            [ "name" .= ("conflict2" :: Text),
              "email" .= (T.takeWhile (/= '+') loc <> "@" <> dom), -- dup. email
              "password" .= defPassword
            ]
  post (brig . path "/register" . contentJson . body p2) !!! do
    const 409 === statusCode
    const (Just "key-exists") === fmap Error.label . responseJsonMaybe

testCreateUserInvalidEmailOrPhone :: Opt.Opts -> Brig -> Http ()
testCreateUserInvalidEmailOrPhone (Opt.setRestrictUserCreation . Opt.optSettings -> Just True) _ = pure ()
testCreateUserInvalidEmailOrPhone _ brig = do
  email <- randomEmail
  let reqEmail =
        RequestBodyLBS . encode $
          object
            [ "name" .= ("foo" :: Text),
              "email" .= fromEmail email,
              "password" .= defPassword,
              "phone" .= ("123456" :: Text) -- invalid phone nr
            ]
  post (brig . path "/register" . contentJson . body reqEmail)
    !!! const 400 === statusCode

  phone <- randomPhone
  let reqPhone =
        RequestBodyLBS . encode $
          object
            [ "name" .= ("foo" :: Text),
              "email" .= ("invalid@email" :: Text), -- invalid since there's only a single label
              "password" .= defPassword,
              "phone" .= fromPhone phone
            ]
  post (brig . path "/register" . contentJson . body reqPhone)
    !!! const 400 === statusCode

testCreateUserBlacklist :: Opt.Opts -> Brig -> AWS.Env -> Http ()
testCreateUserBlacklist (Opt.setRestrictUserCreation . Opt.optSettings -> Just True) _ _ = pure ()
testCreateUserBlacklist _ brig aws =
  mapM_ ensureBlacklist ["bounce", "complaint"]
  where
    ensureBlacklist typ = do
      e <- randomEmail
      flip finally (removeBlacklist brig e) $ do
        post (brig . path "/register" . contentJson . body (p e)) !!! const 201 === statusCode
        -- If we are using a local env, we need to fake this bounce
        unless (Util.isRealSESEnv aws) $
          forceBlacklist typ e
        -- Typically bounce/complaint messages arrive instantaneously
        awaitBlacklist 30 e
        post (brig . path "/register" . contentJson . body (p e)) !!! do
          const 403 === statusCode
          const (Just "blacklisted-email") === fmap Error.label . responseJsonMaybe
    p email =
      RequestBodyLBS . encode $
        object
          [ "name" .= ("Alice" :: Text),
            "email" .= email,
            "password" .= defPassword
          ]
    -- If there is no queue available, we need to force it either by publishing an event or using the API
    forceBlacklist :: Text -> Email -> Http ()
    forceBlacklist typ em = case aws ^. AWS.sesQueue of
      Just queue -> publishMessage typ em queue
      Nothing -> Bilge.post (brig . path "i/users/blacklist" . queryItem "email" (toByteString' em)) !!! const 200 === statusCode
    publishMessage :: Text -> Email -> Text -> Http ()
    publishMessage typ em queue = do
      let bdy = encode $ case typ of
            "bounce" -> MailBounce BouncePermanent [em]
            "complaint" -> MailComplaint [em]
            x -> error ("Unsupported message type: " ++ show x)
      void . AWS.execute aws $ AWS.enqueueStandard queue bdy
    awaitBlacklist :: Int -> Email -> Http ()
    awaitBlacklist n e = do
      r <- Bilge.head (brig . path "i/users/blacklist" . queryItem "email" (toByteString' e))
      when (statusCode r == 404 && n > 0) $ do
        liftIO $ threadDelay 1000000
        awaitBlacklist (n -1) e

testCreateUserExternalSSO :: Brig -> Http ()
testCreateUserExternalSSO brig = do
  teamid <- Id <$> liftIO UUID.nextRandom
  let ssoid = UserSSOId "nil" "nil"
      p withsso withteam =
        RequestBodyLBS . encode . object $
          ["name" .= ("foo" :: Text)]
            <> ["sso_id" .= Just ssoid | withsso]
            <> ["team_id" .= Just teamid | withteam]
  post (brig . path "/register" . contentJson . body (p False True))
    !!! const 400 === statusCode
  post (brig . path "/register" . contentJson . body (p True False))
    !!! const 400 === statusCode
  post (brig . path "/register" . contentJson . body (p True True))
    !!! const 400 === statusCode

testActivateWithExpiry :: Opt.Opts -> Brig -> Opt.Timeout -> Http ()
testActivateWithExpiry (Opt.setRestrictUserCreation . Opt.optSettings -> Just True) _ _ = pure ()
testActivateWithExpiry _ brig timeout = do
  u <- responseJsonError =<< registerUser "dilbert" brig
  let email = fromMaybe (error "missing email") (userEmail u)
  act <- getActivationCode brig (Left email)
  case act of
    Nothing -> liftIO $ assertFailure "activation key/code not found"
    Just kc -> do
      activate brig kc !!! do
        const 200 === statusCode
        const (Just (userIdentity u, True)) === actualBody
      -- Note: This value must be larger than the option passed as `activation-timeout`
      awaitExpiry (round timeout + 5) kc
      activate brig kc !!! const 404 === statusCode
  where
    actualBody :: HasCallStack => ResponseLBS -> Maybe (Maybe UserIdentity, Bool)
    actualBody rs = do
      a <- responseJsonMaybe rs
      Just (Just (activatedIdentity a), activatedFirst a)
    awaitExpiry :: HasCallStack => Int -> ActivationPair -> Http ()
    awaitExpiry n kc = do
      liftIO $ threadDelay 1000000
      r <- activate brig kc
      when (statusCode r == 204 && n > 0) $
        awaitExpiry (n -1) kc

testNonExistingUser :: Brig -> Http ()
testNonExistingUser brig = do
  findingOne <- liftIO $ Id <$> UUID.nextRandom
  foundOne <- liftIO $ Id <$> UUID.nextRandom
  get (brig . paths ["users", pack $ show foundOne] . zUser findingOne)
    !!! const 404 === statusCode
  get (brig . paths ["users", pack $ show foundOne] . zUser foundOne)
    !!! const 404 === statusCode

testExistingUser :: Brig -> Http ()
testExistingUser brig = do
  uid <- userId <$> randomUser brig
  get (brig . paths ["users", pack $ show uid] . zUser uid) !!! do
    const 200 === statusCode
    const (Just uid)
      === ( \r -> do
              b <- responseBody r
              b ^? key "id" >>= maybeFromJSON
          )

testMultipleUsers :: Brig -> Http ()
testMultipleUsers brig = do
  u1 <- randomUser brig
  u2 <- randomUser brig
  u3 <- createAnonUser "a" brig
  let uids = C8.intercalate "," $ map (toByteString' . userId) [u1, u2, u3]
      -- n.b. email addresses and phone numbers are never returned
      -- on this endpoint, only from the self profile (/self).
      expected =
        Set.fromList
          [ (Just $ userDisplayName u1, Nothing :: Maybe Email),
            (Just $ userDisplayName u2, Nothing),
            (Just $ userDisplayName u3, Nothing)
          ]
  get (brig . zUser (userId u1) . path "users" . queryItem "ids" uids) !!! do
    const 200 === statusCode
    const (Just expected) === result
  where
    result r =
      Set.fromList
        . map (field "name" &&& field "email")
        <$> responseJsonMaybe r
    field :: FromJSON a => Text -> Value -> Maybe a
    field f u = u ^? key f >>= maybeFromJSON

testCreateUserAnonExpiry :: Brig -> Http ()
testCreateUserAnonExpiry b = do
  u1 <- randomUser b
  alice <- randomUser b
  now <- liftIO getCurrentTime
  bob <- createAnonUserExpiry (Just 2) "bob" b
  liftIO $ assertBool "expiry not set on regular creation" (not $ isJust $ userExpire alice)
  ensureExpiry now (fromUTCTimeMillis <$> userExpire bob) "bob/register"
  resAlice <- getProfile (userId u1) (userId alice)
  resBob <- getProfile (userId u1) (userId bob)
  selfBob <- get (b . zUser (userId bob) . path "self") <!! const 200 === statusCode
  liftIO $ assertBool "Bob must not be in a deleted state initially" (fromMaybe True (not <$> deleted selfBob))
  liftIO $ assertBool "Regular user should not have any expiry" (null $ expire resAlice)
  ensureExpiry now (expire resBob) "bob/public"
  ensureExpiry now (expire selfBob) "bob/self"
  awaitExpiry 5 (userId u1) (userId bob)
  resBob' <- getProfile (userId u1) (userId bob)
  liftIO $ assertBool "Bob must be in deleted state" (fromMaybe False $ deleted resBob')
  where
    getProfile :: UserId -> UserId -> Http ResponseLBS
    getProfile zusr uid = get (b . zUser zusr . paths ["users", toByteString' uid]) <!! const 200 === statusCode
    awaitExpiry :: Int -> UserId -> UserId -> Http ()
    awaitExpiry n zusr uid = do
      -- after expiration, a profile lookup should trigger garbage collection of ephemeral users
      r <- getProfile zusr uid
      when (statusCode r == 200 && deleted r == Nothing && n > 0) $ do
        liftIO $ threadDelay 1000000
        awaitExpiry (n -1) zusr uid
    ensureExpiry :: UTCTime -> Maybe UTCTime -> String -> Http ()
    ensureExpiry now expiry s = case expiry of
      Nothing -> liftIO $ assertFailure ("user must have an expiry" <> s)
      Just a -> do
        let diff = diffUTCTime a now
            minExp = 1 :: Integer -- 1 second
            maxExp = 60 * 60 * 24 * 10 :: Integer -- 10 days
        liftIO $ assertBool "expiry must in be the future" (diff >= fromIntegral minExp)
        liftIO $ assertBool "expiry must be less than 10 days" (diff < fromIntegral maxExp)
    expire :: ResponseLBS -> Maybe UTCTime
    expire r = join $ field "expires_at" <$> responseJsonMaybe r
    deleted :: ResponseLBS -> Maybe Bool
    deleted r = join $ field "deleted" <$> responseJsonMaybe r
    field :: FromJSON a => Text -> Value -> Maybe a
    field f u = u ^? key f >>= maybeFromJSON

testUserUpdate :: Brig -> Cannon -> AWS.Env -> Http ()
testUserUpdate brig cannon aws = do
  aliceUser <- randomUser brig
  liftIO $ Util.assertUserJournalQueue "user create alice" aws (userActivateJournaled aliceUser)
  let alice = userId aliceUser
  bobUser <- randomUser brig
  liftIO $ Util.assertUserJournalQueue "user create bob" aws (userActivateJournaled bobUser)
  let bob = userId bobUser
  connectUsers brig alice (singleton bob)
  let newColId = Just 5
      newAssets = Just [ImageAsset "abc" (Just AssetComplete)]
      newName = Just $ Name "dogbert"
      newPic = Nothing -- Legacy
      userUpdate = UserUpdate newName newPic newAssets newColId
      update = RequestBodyLBS . encode $ userUpdate
  -- Update profile & receive notification
  WS.bracketRN cannon [alice, bob] $ \[aliceWS, bobWS] -> do
    put (brig . path "/self" . contentJson . zUser alice . zConn "c" . body update)
      !!! const 200 === statusCode
    liftIO $ mapConcurrently_ (\ws -> assertUpdateNotification ws alice userUpdate) [aliceWS, bobWS]
    -- Should generate a user update journaled event with the new name
    liftIO $ Util.assertUserJournalQueue "user update" aws (userUpdateJournaled alice userUpdate)
  -- get the updated profile
  get (brig . path "/self" . zUser alice) !!! do
    const 200 === statusCode
    const (newName, newColId, newAssets)
      === ( \u ->
              ( fmap userDisplayName u,
                fmap userAccentId u,
                fmap userAssets u
              )
          )
        . responseJsonMaybe
  -- get only the new name
  get (brig . path "/self/name" . zUser alice) !!! do
    const 200 === statusCode
    const (String . fromName <$> newName)
      === ( \r -> do
              b <- responseBody r
              b ^? key "name"
          )
  -- should appear in search by 'newName'
  suid <- userId <$> randomUser brig
  Search.refreshIndex brig
  Search.assertCanFind brig suid alice "dogbert"

testEmailUpdate :: Brig -> AWS.Env -> Http ()
testEmailUpdate brig aws = do
  usr <- randomUser brig
  liftIO $ Util.assertUserJournalQueue "user create random" aws (userActivateJournaled usr)
  let uid = userId usr
  eml <- randomEmail
  -- update email
  initiateEmailUpdate brig eml uid !!! const 202 === statusCode
  -- activate
  activateEmail brig eml
  checkEmail brig uid eml
  liftIO $ Util.assertUserJournalQueue "user update" aws (userEmailUpdateJournaled uid eml)
  -- update email, which is exactly the same as before
  initiateEmailUpdate brig eml uid !!! const 204 === statusCode
  -- ensure no other user has "test+<uuid>@example.com"
  -- if there is such a user, let's delete it first.  otherwise
  -- this test fails since there can be only one user with "test+...@example.com"
  ensureNoOtherUserWithEmail (Email "test" "example.com")
  -- we want to use a non-trusted domain in order to verify profile changes
  flip initiateUpdateAndActivate uid =<< mkEmailRandomLocalSuffix "test@example.com"
  flip initiateUpdateAndActivate uid =<< mkEmailRandomLocalSuffix "test@example.com"
  where
    ensureNoOtherUserWithEmail eml = do
      tk :: Maybe AccessToken <-
        responseJsonMaybe <$> login brig (defEmailLogin eml) SessionCookie
      for_ tk $ \t -> do
        deleteUser (Auth.user t) (Just defPassword) brig !!! const 200 === statusCode
        liftIO $ Util.assertUserJournalQueue "user deletion" aws (userDeleteJournaled $ Auth.user t)
    initiateUpdateAndActivate eml uid = do
      initiateEmailUpdateNoSend brig eml uid !!! const 202 === statusCode
      activateEmail brig eml
      checkEmail brig uid eml
      liftIO $ Util.assertUserJournalQueue "user update" aws (userEmailUpdateJournaled uid eml)
      -- Ensure login work both with the full email and the "short" version
      login brig (defEmailLogin eml) SessionCookie !!! const 200 === statusCode
      login brig (defEmailLogin (Email "test" "example.com")) SessionCookie !!! const 200 === statusCode

testPhoneUpdate :: Brig -> Http ()
testPhoneUpdate brig = do
  uid <- userId <$> randomUser brig
  phn <- randomPhone
  updatePhone brig uid phn
  -- check new phone
  get (brig . path "/self" . zUser uid) !!! do
    const 200 === statusCode
    const (Just phn) === (userPhone <=< responseJsonMaybe)

testCreateAccountPendingActivationKey :: Opt.Opts -> Brig -> Http ()
testCreateAccountPendingActivationKey (Opt.setRestrictUserCreation . Opt.optSettings -> Just True) _ = pure ()
testCreateAccountPendingActivationKey _ brig = do
  uid <- userId <$> randomUser brig
  phn <- randomPhone
  -- update phone
  let phoneUpdate = RequestBodyLBS . encode $ PhoneUpdate phn
  put (brig . path "/self/phone" . contentJson . zUser uid . zConn "c" . body phoneUpdate)
    !!! (const 202 === statusCode)
  -- create a new user with that phone/code
  act <- getActivationCode brig (Right phn)
  case act of
    Nothing -> liftIO $ assertFailure "missing activation key/code"
    Just kc@(_, c) -> do
      let p =
            RequestBodyLBS . encode $
              object
                [ "name" .= ("foo" :: Text),
                  "phone" .= phn,
                  "phone_code" .= c
                ]
      post (brig . path "/register" . contentJson . body p)
        !!! const 201 === statusCode
      -- try to activate already active phone
      activate brig kc !!! const 409 === statusCode

testUserLocaleUpdate :: Brig -> AWS.Env -> Http ()
testUserLocaleUpdate brig aws = do
  usr <- randomUser brig
  liftIO $ Util.assertUserJournalQueue "user create random" aws (userActivateJournaled usr)
  let uid = userId usr
  -- update locale info with locale supported in templates
  let locEN = fromMaybe (error "Failed to parse locale") $ parseLocale "en-US"
  put (brig . path "/self/locale" . contentJson . zUser uid . zConn "c" . locale locEN)
    !!! const 200 === statusCode
  liftIO $ Util.assertUserJournalQueue "user update" aws (userLocaleUpdateJournaled uid locEN)
  -- update locale info with locale NOT supported in templates
  let locPT = fromMaybe (error "Failed to parse locale") $ parseLocale "pt-PT"
  put (brig . path "/self/locale" . contentJson . zUser uid . zConn "c" . locale locPT)
    !!! const 200 === statusCode
  liftIO $ Util.assertUserJournalQueue "user update" aws (userLocaleUpdateJournaled uid locPT)
  -- get the updated locale
  get (brig . path "/self" . zUser uid) !!! do
    const 200 === statusCode
    const (parseLocale "pt-PT") === (Just . userLocale . selfUser <=< responseJsonMaybe)
  where
    locale l = body . RequestBodyLBS . encode $ object ["locale" .= l]

testSuspendUser :: Brig -> Http ()
testSuspendUser brig = do
  u <- randomUser brig
  let uid = userId u
      Just email = userEmail u
  setStatus brig uid Suspended
  -- login fails
  login brig (defEmailLogin email) PersistentCookie !!! do
    const 403 === statusCode
    const (Just "suspended") === fmap Error.label . responseJsonMaybe
  -- check status
  chkStatus brig uid Suspended
  -- should not appear in search
  suid <- userId <$> randomUser brig
  Search.refreshIndex brig
  Search.assertCan'tFind brig suid uid (fromName (userDisplayName u))
  -- re-activate
  setStatus brig uid Active
  chkStatus brig uid Active
  -- should appear in search again
  Search.refreshIndex brig
  Search.assertCanFind brig suid uid (fromName (userDisplayName u))

testGetByIdentity :: Brig -> Http ()
testGetByIdentity brig = do
  p <- randomPhone
  e <- randomEmail
  let emailBs = T.encodeUtf8 $ fromEmail e
      phoneBs = T.encodeUtf8 $ fromPhone p
      newUser =
        RequestBodyLBS . encode $
          object
            [ "name" .= ("Alice" :: Text),
              "phone" .= fromPhone p,
              "email" .= fromEmail e
            ]
  rs <-
    post (brig . path "/i/users" . contentJson . body newUser)
      <!! const 201 === statusCode
  let Just uid = userId <$> responseJsonMaybe rs
  get (brig . zUser uid . path "i/users" . queryItem "email" emailBs) !!! do
    const 200 === statusCode
    const (Just [uid]) === getUids
  get (brig . zUser uid . path "i/users" . queryItem "phone" phoneBs) !!! do
    const 200 === statusCode
    const (Just [uid]) === getUids
  where
    getUids r = return . fmap (userId . accountUser) =<< responseJsonMaybe r

testPasswordSet :: Brig -> Http ()
testPasswordSet brig = do
  p <- randomPhone
  let newUser =
        RequestBodyLBS . encode $
          object
            [ "name" .= ("Alice" :: Text),
              "phone" .= fromPhone p
            ]
  rs <-
    post (brig . path "/i/users" . contentJson . body newUser)
      <!! const 201 === statusCode
  let Just uid = userId <$> responseJsonMaybe rs
  -- No password set yet
  Bilge.head (brig . path "/self/password" . zUser uid)
    !!! const 404 === statusCode
  -- Since there is no password set, we can just set one
  put (brig . path "/self/password" . contentJson . zUser uid . body pwSet)
    !!! const 200 === statusCode
  -- Now we should have a password
  Bilge.head (brig . path "/self/password" . zUser uid)
    !!! const 200 === statusCode
  where
    pwSet =
      RequestBodyLBS . encode $
        object
          [ "new_password" .= ("a_very_long_password" :: Text)
          ]

testPasswordChange :: Brig -> Http ()
testPasswordChange brig = do
  (uid, Just email) <- (userId &&& userEmail) <$> randomUser brig
  put (brig . path "/self/password" . contentJson . zUser uid . body pwChange)
    !!! const 200 === statusCode
  -- login with new password
  login brig (PasswordLogin (LoginByEmail email) newPass Nothing) PersistentCookie
    !!! const 200 === statusCode
  -- try to change the password to itself should fail
  put (brig . path "/self/password" . contentJson . zUser uid . body pwChange')
    !!! const 409 === statusCode
  -- Setting a password for an anonymous / unverified user should fail
  uid2 <- userId <$> createAnonUser "foo2" brig
  put (brig . path "/self/password" . contentJson . zUser uid2 . body pwSet)
    !!! (const 403 === statusCode)
  where
    newPass = PlainTextPassword "topsecret"
    pwChange =
      RequestBodyLBS . encode $
        object
          [ "old_password" .= defPassword,
            "new_password" .= newPass
          ]
    pwChange' =
      RequestBodyLBS . encode $
        object
          [ "old_password" .= newPass,
            "new_password" .= newPass
          ]
    pwSet =
      RequestBodyLBS . encode $
        object
          [ "new_password" .= newPass
          ]

testSendActivationCode :: Opt.Opts -> Brig -> Http ()
testSendActivationCode opts brig = do
  -- Code for phone pre-verification
  requestActivationCode brig 200 . Right =<< randomPhone
  -- Code for email pre-verification
  requestActivationCode brig 200 . Left =<< randomEmail
  -- Standard email registration flow
  if Opt.setRestrictUserCreation (Opt.optSettings opts) == Just True
    then do
      registerUser "Alice" brig !!! const 403 === statusCode
    else do
      r <- registerUser "Alice" brig <!! const 201 === statusCode
      let Just email = userEmail =<< responseJsonMaybe r
      -- Re-request existing activation code
      requestActivationCode brig 200 (Left email)

testSendActivationCodeInvalidEmailOrPhone :: Brig -> Http ()
testSendActivationCodeInvalidEmailOrPhone brig = do
  let Just invalidEmail = parseEmail "?@?"
  let invalidPhone = Phone "1234"
  -- Code for phone pre-verification
  requestActivationCode brig 400 (Right invalidPhone)
  -- Code for email pre-verification
  requestActivationCode brig 400 (Left invalidEmail)

testSendActivationCodePrefixExcluded :: Brig -> Http ()
testSendActivationCodePrefixExcluded brig = do
  p <- randomPhone
  let prefix = mkPrefix $ T.take 5 (fromPhone p)
  -- expect activation to fail after it was excluded
  insertPrefix brig prefix
  requestActivationCode brig 403 (Right p)
  -- expect activation to work again after removing block
  deletePrefix brig (phonePrefix prefix)
  requestActivationCode brig 200 (Right p)

testInternalPhonePrefixes :: Brig -> Http ()
testInternalPhonePrefixes brig = do
  -- prefix1 is a prefix of prefix2
  let prefix1 = mkPrefix "+5678"
      prefix2 = mkPrefix "+56789"
  insertPrefix brig prefix1
  insertPrefix brig prefix2
  -- test getting prefixs
  res <- getPrefixes prefix1
  liftIO $ assertEqual "prefix match prefix" res [prefix1]
  -- we expect both prefixes returned when searching for the longer one
  res2 <- getPrefixes prefix2
  liftIO $ assertEqual "prefix match phone number" res2 [prefix1, prefix2]
  deletePrefix brig (phonePrefix prefix1)
  deletePrefix brig (phonePrefix prefix2)
  getPrefix (phonePrefix prefix1) !!! const 404 === statusCode
  where
    getPrefixes :: ExcludedPrefix -> Http [ExcludedPrefix]
    getPrefixes prefix = responseJsonError =<< getPrefix (phonePrefix prefix)
    getPrefix :: PhonePrefix -> Http ResponseLBS
    getPrefix prefix = get (brig . paths ["/i/users/phone-prefixes", toByteString' prefix])

mkPrefix :: Text -> ExcludedPrefix
mkPrefix t = ExcludedPrefix (PhonePrefix t) "comment"

insertPrefix :: Brig -> ExcludedPrefix -> Http ()
insertPrefix brig prefix = do
  let payload = body $ RequestBodyLBS (encode prefix)
  post (brig . path "/i/users/phone-prefixes" . contentJson . payload) !!! const 200 === statusCode

deletePrefix :: Brig -> PhonePrefix -> Http ()
deletePrefix brig prefix = delete (brig . paths ["/i/users/phone-prefixes", toByteString' prefix]) !!! const 200 === statusCode

testEmailPhoneDelete :: Brig -> Cannon -> Http ()
testEmailPhoneDelete brig cannon = do
  user <- randomUser brig
  let uid = userId user
  let Just email = userEmail user
  -- Cannot remove the only identity
  delete (brig . path "/self/email" . zUser uid . zConn "c")
    !!! const 403 === statusCode
  -- Add a phone number
  phone <- randomPhone
  let phoneUpdate = RequestBodyLBS . encode $ PhoneUpdate phone
  put (brig . path "/self/phone" . contentJson . zUser uid . zConn "c" . body phoneUpdate)
    !!! const 202 === statusCode
  act <- getActivationCode brig (Right phone)
  case act of
    Nothing -> liftIO $ assertFailure "missing activation key/code"
    Just kc -> activate brig kc !!! const 200 === statusCode
  -- Remove the email
  WS.bracketR cannon uid $ \ws -> do
    delete (brig . path "/self/email" . zUser uid . zConn "c")
      !!! (const 200 === statusCode)
    void . liftIO . WS.assertMatch (5 # Second) ws $ \n -> do
      let j = Object $ List1.head (ntfPayload n)
      let etype = j ^? key "type" . _String
      let euser = j ^? key "user" . key "id" . _String
      let eemail = j ^? key "user" . key "email" . _String
      etype @?= Just "user.identity-remove"
      euser @?= Just (UUID.toText (toUUID uid))
      eemail @?= Just (fromEmail email)
  get (brig . path "/self" . zUser uid) !!! do
    const 200 === statusCode
    const Nothing === (userEmail <=< responseJsonMaybe)
  -- Cannot remove the only remaining identity
  delete (brig . path "/self/phone" . zUser uid . zConn "c")
    !!! const 403 === statusCode
  -- Add back a new email address
  eml <- randomEmail
  let emailUpdate = RequestBodyLBS . encode $ EmailUpdate eml
  put (brig . path "/self/email" . contentJson . zUser uid . zConn "c" . body emailUpdate)
    !!! const 202 === statusCode
  act' <- getActivationCode brig (Left eml)
  case act' of
    Nothing -> liftIO $ assertFailure "missing activation key/code"
    Just kc -> activate brig kc !!! const 200 === statusCode
  -- Remove the phone number
  WS.bracketR cannon uid $ \ws -> do
    delete (brig . path "/self/phone" . zUser uid . zConn "c")
      !!! const 200 === statusCode
    void . liftIO . WS.assertMatch (5 # Second) ws $ \n -> do
      let j = Object $ List1.head (ntfPayload n)
      let etype = j ^? key "type" . _String
      let euser = j ^? key "user" . key "id" . _String
      let ephone = j ^? key "user" . key "phone" . _String
      etype @?= Just "user.identity-remove"
      euser @?= Just (UUID.toText (toUUID uid))
      ephone @?= Just (fromPhone phone)
  get (brig . path "/self" . zUser uid) !!! do
    const 200 === statusCode
    const Nothing === (userPhone <=< responseJsonMaybe)

testDeleteUserByPassword :: Brig -> Cannon -> AWS.Env -> Http ()
testDeleteUserByPassword brig cannon aws = do
  u <- randomUser brig
  liftIO $ Util.assertUserJournalQueue "user activate" aws (userActivateJournaled u)
  let uid1 = userId u
  let email = fromMaybe (error "Missing email") (userEmail u)
  -- Initiate a change of email address, to verify that activation
  -- does not work after the account has been deleted.
  eml <- randomEmail
  let emailUpdate = RequestBodyLBS . encode $ EmailUpdate eml
  put (brig . path "/self/email" . contentJson . zUser uid1 . zConn "c" . body emailUpdate)
    !!! const 202 === statusCode
  -- Establish some connections
  usr2 <- randomUser brig
  let uid2 = userId usr2
  liftIO $ Util.assertUserJournalQueue "user activate" aws (userActivateJournaled usr2)
  usr3 <- randomUser brig
  let uid3 = userId usr3
  liftIO $ Util.assertUserJournalQueue "user activate" aws (userActivateJournaled usr3)
  postConnection brig uid1 uid2 !!! const 201 === statusCode
  postConnection brig uid1 uid3 !!! const 201 === statusCode
  putConnection brig uid2 uid1 Accepted !!! const 200 === statusCode
  putConnection brig uid3 uid1 Accepted !!! const 200 === statusCode
  postConnection brig uid2 uid3 !!! const 201 === statusCode
  con32 <- putConnection brig uid3 uid2 Accepted <!! const 200 === statusCode
  con23 <- getConnection brig uid2 uid3 <!! const 200 === statusCode
  -- Register a client
  addClient brig uid1 (defNewClient PermanentClientType [somePrekeys !! 0] (someLastPrekeys !! 0))
    !!! const 201 === statusCode
  -- Initial login
  login brig (defEmailLogin email) PersistentCookie
    !!! const 200 === statusCode
  n1 <- countCookies brig uid1 defCookieLabel
  liftIO $ Just 1 @=? n1
  setHandleAndDeleteUser brig cannon u [] aws $
    \uid -> deleteUser uid (Just defPassword) brig !!! const 200 === statusCode
  -- Activating the new email address now should not work
  act <- getActivationCode brig (Left eml)
  case act of
    Nothing -> liftIO $ assertFailure "missing activation key/code"
    Just kc ->
      activate brig kc !!! do
        const 404 === statusCode
        const (Just "invalid-code") === fmap Error.label . responseJsonMaybe
  -- Connections involving uid1 are gone (uid2 <-> uid3 remains)
  let u1Conns = UserConnectionList [] False
  let u2Conns = UserConnectionList (maybeToList (responseJsonMaybe con23)) False
  let u3Conns = UserConnectionList (maybeToList (responseJsonMaybe con32)) False
  listConnections brig uid1 !!! do
    const 200 === statusCode
    const (Just u1Conns) === responseJsonMaybe
  listConnections brig uid2 !!! do
    const 200 === statusCode
    const (Just u2Conns) === responseJsonMaybe
  listConnections brig uid3 !!! do
    const 200 === statusCode
    const (Just u3Conns) === responseJsonMaybe

testDeleteUserWithLegalHold :: Brig -> Cannon -> AWS.Env -> Http ()
testDeleteUserWithLegalHold brig cannon aws = do
  user <- randomUser brig
  let uid = userId user
  -- Register a legalhold client
  addClientInternal brig uid (defNewClient LegalHoldClientType [somePrekeys !! 0] (someLastPrekeys !! 0))
    !!! const 201 === statusCode
  liftIO $ Util.assertUserJournalQueue "user activate testDeleteInternal1: " aws (userActivateJournaled user)
  setHandleAndDeleteUser brig cannon user [] aws $
    \uid' -> deleteUser uid' (Just defPassword) brig !!! const 200 === statusCode

testDeleteUserByCode :: Brig -> Http ()
testDeleteUserByCode brig = do
  u <- randomUser brig
  deleteUser (userId u) Nothing brig
    !!! const 202 === statusCode
  -- (Syntactically) invalid key / code
  let _key = "" :: Text
  let _code = "123" :: Text
  send _key _code !!! do
    const 400 === statusCode
    const (Just "bad-request") === fmap Error.label . responseJsonMaybe
  -- (Semantically) invalid key / code
  let _key = T.replicate 20 "x"
  let _code = "idontknow" :: Text
  send _key _code !!! do
    const 403 === statusCode
    const (Just "invalid-code") === fmap Error.label . responseJsonMaybe
  where
    send k c = post (brig . path "/delete" . contentJson . body (payload k c))
    payload k c =
      RequestBodyLBS . encode $
        object
          ["key" .= k, "code" .= c]

testDeleteAnonUser :: Brig -> Http ()
testDeleteAnonUser brig = do
  uid <- userId <$> createAnonUser "anon" brig
  deleteUser uid Nothing brig
    !!! const 200 === statusCode

testDeleteInternal :: Brig -> Cannon -> AWS.Env -> Http ()
testDeleteInternal brig cannon aws = do
  u <- randomUser brig
  liftIO $ Util.assertUserJournalQueue "user activate testDeleteInternal1: " aws (userActivateJournaled u)
  setHandleAndDeleteUser brig cannon u [] aws $
    \uid -> delete (brig . paths ["/i/users", toByteString' uid]) !!! const 202 === statusCode

-- Check that user deletion is also triggered
-- liftIO $ Util.assertUserJournalQueue "user deletion testDeleteInternal2: " aws (userDeleteJournaled $ userId u)

testDeleteWithProfilePic :: Brig -> CargoHold -> Http ()
testDeleteWithProfilePic brig cargohold = do
  uid <- userId <$> createAnonUser "anon" brig
  ast <- uploadAsset cargohold uid "this is my profile pic"
  -- Ensure that the asset is there
  downloadAsset cargohold uid (toByteString' (ast ^. CHV3.assetKey)) !!! const 200 === statusCode
  let newAssets = Just [ImageAsset (T.decodeLatin1 $ toByteString' (ast ^. CHV3.assetKey)) (Just AssetComplete)]
      userUpdate = UserUpdate Nothing Nothing newAssets Nothing
      update = RequestBodyLBS . encode $ userUpdate
  -- Update profile with the uploaded asset
  put (brig . path "/self" . contentJson . zUser uid . zConn "c" . body update)
    !!! const 200 === statusCode
  deleteUser uid Nothing brig !!! const 200 === statusCode
  -- Check that the asset gets deleted
  downloadAsset cargohold uid (toByteString' (ast ^. CHV3.assetKey)) !!! const 404 === statusCode

testUpdateSSOId :: Brig -> Galley -> Http ()
testUpdateSSOId brig galley = do
  noSuchUserId <- Id <$> liftIO UUID.nextRandom
  put
    ( brig
        . paths ["i", "users", toByteString' noSuchUserId, "sso-id"]
        . Bilge.json (UserSSOId "1" "1")
    )
    !!! const 404 === statusCode
  let go :: HasCallStack => User -> UserSSOId -> Http ()
      go user ssoid = do
        let uid = userId user
        put
          ( brig
              . paths ["i", "users", toByteString' uid, "sso-id"]
              . Bilge.json ssoid
          )
          !!! const 200 === statusCode
        profile :: SelfProfile <- responseJsonError =<< get (brig . path "/self" . zUser uid)
        let Just (SSOIdentity ssoid' mEmail mPhone) = userIdentity . selfUser $ profile
        liftIO $ do
          assertEqual "updateSSOId/ssoid" ssoid ssoid'
          assertEqual "updateSSOId/email" (userEmail user) mEmail
          assertEqual "updateSSOId/phone" (userPhone user) mPhone
  (owner, teamid) <- createUserWithTeam brig
  let mkMember :: Bool -> Bool -> Http User
      mkMember hasEmail hasPhone = do
        member <- createTeamMember brig galley owner teamid noPermissions
        when hasPhone $ do
          updatePhone brig (userId member) =<< randomPhone
        when (not hasEmail) $ do
          error "not implemented"
        selfUser <$> (responseJsonError =<< get (brig . path "/self" . zUser (userId member)))
  let ssoids1 = [UserSSOId "1" "1", UserSSOId "1" "2"]
      ssoids2 = [UserSSOId "2" "1", UserSSOId "2" "2"]
  users <-
    sequence
      [ mkMember True False,
        mkMember True True
        -- the following two could be implemented by creating the user implicitly via SSO login.
        -- , mkMember False  False
        -- , mkMember False  True
      ]
  sequence_ $ zipWith go users ssoids1
  sequence_ $ zipWith go users ssoids2

testDomainsBlockedForRegistration :: Opt.Opts -> Brig -> Http ()
testDomainsBlockedForRegistration opts brig = withDomainsBlockedForRegistration opts ["bad1.domain.com", "bad2.domain.com"] $ do
  badEmail1 <- randomEmail <&> \e -> e {emailDomain = "bad1.domain.com"}
  badEmail2 <- randomEmail <&> \e -> e {emailDomain = "bad2.domain.com"}
  post (brig . path "/activate/send" . contentJson . body (p badEmail1)) !!! do
    const 451 === statusCode
    const (Just "domain-blocked-for-registration") === (^? AesonL.key "label" . AesonL._String) . (responseJsonUnsafe @Value)
  post (brig . path "/activate/send" . contentJson . body (p badEmail2)) !!! do
    const 451 === statusCode
    const (Just "domain-blocked-for-registration") === (^? AesonL.key "label" . AesonL._String) . (responseJsonUnsafe @Value)
  goodEmail <- randomEmail <&> \e -> e {emailDomain = "good.domain.com"}
  post (brig . path "/activate/send" . contentJson . body (p goodEmail)) !!! do
    const 200 === statusCode
  where
    p email = RequestBodyLBS . encode $ SendActivationCode (Left email) Nothing False

-- | FUTUREWORK: @setRestrictUserCreation@ perhaps needs to be tested in one place only, since it's the
-- first thing that we check on the /register endpoint. Other tests that make use of @setRestrictUserCreation@
-- can probably be removed and simplified. It's probably a good candidate for Quickcheck.
testRestrictedUserCreation :: Opt.Opts -> Brig -> Http ()
testRestrictedUserCreation opts brig = do
  -- We create a team before to help in other tests
  (teamOwner, createdTeam) <- createUserWithTeam brig

  let opts' = opts {Opt.optSettings = (Opt.optSettings opts) {Opt.setRestrictUserCreation = Just True}}
  withSettingsOverrides opts' $ do
    e <- randomEmail
    -- Ephemeral users MUST have an expires_in
    let Object ephemeralUserWithoutExpires =
          object
            [ "name" .= Name "Alice"
            ]
    postUserRegister' ephemeralUserWithoutExpires brig !!! do
      const 403 === statusCode
      const (Just "user-creation-restricted") === (^? AesonL.key "label" . AesonL._String) . (responseJsonUnsafe @Value)

    let Object regularUser =
          object
            [ "name" .= Name "Alice",
              "email" .= fromEmail e,
              "email_code" .= ("123456" :: Text),
              "password" .= PlainTextPassword "123123123"
            ]
    postUserRegister' regularUser brig !!! do
      const 403 === statusCode
      const (Just "user-creation-restricted") === (^? AesonL.key "label" . AesonL._String) . (responseJsonUnsafe @Value)

    let Object regularUserNotPreActivated =
          object
            [ "name" .= Name "Alice",
              "email" .= fromEmail e,
              "password" .= PlainTextPassword "123123123"
            ]
    postUserRegister' regularUserNotPreActivated brig !!! do
      const 403 === statusCode
      const (Just "user-creation-restricted") === (^? AesonL.key "label" . AesonL._String) . (responseJsonUnsafe @Value)

    let Object teamCreator =
          object
            [ "name" .= Name "Alice",
              "email" .= fromEmail e,
              "email_code" .= ("123456" :: Text),
              "team" .= object ["name" .= ("Alice team" :: Text), "icon" .= ("default" :: Text), "binding" .= True],
              "password" .= PlainTextPassword "123123123"
            ]
    postUserRegister' teamCreator brig !!! do
      const 403 === statusCode
      const (Just "user-creation-restricted") === (^? AesonL.key "label" . AesonL._String) . (responseJsonUnsafe @Value)

    -- Ensure you can invite team users
    void $ inviteAndRegisterUser teamOwner createdTeam brig

    -- Ephemeral users can always be created
    let Object ephemeralUser =
          object
            [ "name" .= Name "Alice",
              "expires_in" .= (600000 :: Int)
            ]
    postUserRegister' ephemeralUser brig !!! const 201 === statusCode

    -- NOTE: SSO users are anyway not allowed on the `/register` endpoint
    teamid <- Id <$> liftIO UUID.nextRandom
    let ssoid = UserSSOId "nil" "nil"
    let Object ssoUser =
          object
            [ "name" .= Name "Alice",
              "sso_id" .= Just ssoid,
              "team_id" .= Just teamid
            ]
    postUserRegister' ssoUser brig !!! const 400 === statusCode

-- helpers

setHandleAndDeleteUser :: Brig -> Cannon -> User -> [UserId] -> AWS.Env -> (UserId -> HttpT IO ()) -> Http ()
setHandleAndDeleteUser brig cannon u others aws execDelete = do
  let uid = userId u
      email = fromMaybe (error "Must have an email set") (userEmail u)
  -- First set a unique handle (to verify freeing of the handle)
  hdl <- randomHandle
  let update = RequestBodyLBS . encode $ HandleUpdate hdl
  put (brig . path "/self/handle" . contentJson . zUser uid . zConn "c" . body update)
    !!! const 200 === statusCode
  -- Delete the user
  WS.bracketRN cannon (uid : others) $ \wss -> do
    execDelete uid
    void . liftIO . WS.assertMatchN (5 # Second) wss $ \n -> do
      let j = Object $ List1.head (ntfPayload n)
      let etype = j ^? key "type" . _String
      let euser = j ^? key "id" . _String
      etype @?= Just "user.delete"
      euser @?= Just (UUID.toText (toUUID uid))
    liftIO $ Util.assertUserJournalQueue "user deletion, setHandleAndDeleteUser: " aws (userDeleteJournaled uid)
  -- Cookies are gone
  n2 <- countCookies brig uid defCookieLabel
  liftIO $ Just 0 @=? n2
  -- Clients are gone
  get (brig . path "clients" . zUser (userId u)) !!! do
    const 200 === statusCode
    const (Just [] :: Maybe [Client]) === responseJsonMaybe
  -- Can no longer log in
  login brig (defEmailLogin email) PersistentCookie !!! do
    const 403 === statusCode
    const (Just "invalid-credentials") === fmap Error.label . responseJsonMaybe
  -- Deleted flag appears in self profile; email, handle and picture are gone
  get (brig . path "/self" . zUser uid) !!! assertDeletedProfileSelf
  Search.refreshIndex brig
  -- Does not appear in search; public profile shows the user as deleted
  forM_ others $ \usr -> do
    get (brig . paths ["users", toByteString' uid] . zUser usr) !!! assertDeletedProfilePublic
    Search.assertCan'tFind brig usr uid (fromName (userDisplayName u))
    Search.assertCan'tFind brig usr uid hdl
  -- Email address is available again
  let Object o =
        object
          [ "name" .= ("Someone Else" :: Text),
            "email" .= fromEmail email,
            "password" .= defPassword
          ]
  -- This will generate a new event, we need to consume it here
  usr <- postUserInternal o brig
  liftIO $ Util.assertUserJournalQueue "user activate testDeleteInternal: " aws (userActivateJournaled usr)
  -- Handle is available again
  Bilge.head (brig . paths ["users", "handles", toByteString' hdl] . zUser uid)
    !!! const 404 === statusCode
  where
    assertDeletedProfileSelf = do
      const 200 === statusCode
      const (Just noPict, Just Nothing, Just True, Just [], Nothing)
        === ( \u' ->
                ( fmap userPict u',
                  fmap userEmail u',
                  fmap userDeleted u',
                  fmap userAssets u',
                  userHandle =<< u'
                )
            )
          . responseJsonMaybe
    assertDeletedProfilePublic = do
      const 200 === statusCode
      const (Just noPict, Just True, Nothing)
        === ( \u' ->
                ( fmap profilePict u',
                  fmap profileDeleted u',
                  profileHandle =<< u'
                )
            )
          . responseJsonMaybe
