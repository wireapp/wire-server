{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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

module API.User.Auth
  ( tests,
  )
where

import API.Team.Util
import Bilge hiding (body)
import qualified Bilge as Http
import Bilge.Assert hiding (assert)
import qualified Brig.Options as Opts
import qualified Brig.Types.Code as Code
import Brig.Types.Intra
import Brig.Types.User
import Brig.Types.User.Auth
import qualified Brig.Types.User.Auth as Auth
import Brig.ZAuth (ZAuth, runZAuth)
import qualified Brig.ZAuth as ZAuth
import Control.Lens (set, (^.), (^?))
import Control.Retry
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString as BS
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as Lazy
import Data.Handle (Handle (Handle))
import Data.Id
import Data.Misc (PlainTextPassword (..))
import Data.Proxy
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as Lazy
import Data.Time.Clock
import qualified Data.UUID.V4 as UUID
import qualified Data.ZAuth.Token as ZAuth
import Imports
import Network.HTTP.Client (equivCookie)
import qualified Network.Wai.Utilities.Error as Error
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.HUnit as HUnit
import UnliftIO.Async hiding (wait)
import Util
import Wire.API.Team.Feature (TeamFeatureStatusValue (..))

tests :: Opts.Opts -> Manager -> ZAuth.Env -> Brig -> Galley -> Nginz -> TestTree
tests conf m z b g n =
  testGroup
    "auth"
    [ testGroup
        "login"
        [ test m "email" (testEmailLogin b),
          test m "phone" (testPhoneLogin b),
          test m "handle" (testHandleLogin b),
          test m "email-untrusted-domain" (testLoginUntrustedDomain b),
          test m "send-phone-code" (testSendLoginCode b),
          test m "failure" (testLoginFailure b),
          test m "throttle" (testThrottleLogins conf b),
          test m "limit-retry" (testLimitRetries conf b),
          testGroup
            "sso-login"
            [ test m "email" (testEmailSsoLogin b),
              test m "failure-suspended" (testSuspendedSsoLogin b),
              test m "failure-no-user" (testNoUserSsoLogin b)
            ],
          testGroup
            "legalhold-login"
            [ test m "failure-no-team" (testRegularUserLegalHoldLogin b),
              test m "team-user-with-legalhold-enabled" (testTeamUserLegalHoldLogin b g),
              test m "failure-suspended" (testSuspendedLegalHoldLogin b),
              test m "failure-no-user" (testNoUserLegalHoldLogin b),
              test m "failure-wrong-password" (testWrongPasswordLegalHoldLogin b g),
              test m "always-persistent-cookie" (testLegalHoldSessionCookie b g),
              test m "legalhold-logout" (testLegalHoldLogout b g)
            ],
          testGroup
            "nginz"
            [ test m "nginz-login" (testNginz b n),
              test m "nginz-legalhold-login" (testNginzLegalHold b g n)
            ]
        ],
      testGroup
        "refresh /access"
        [ test m "invalid-cookie" (testInvalidCookie @ZAuth.User z b),
          test m "invalid-cookie legalhold" (testInvalidCookie @ZAuth.LegalHoldUser z b),
          test m "invalid-token" (testInvalidToken b),
          test m "missing-cookie" (testMissingCookie @ZAuth.User @ZAuth.Access z b),
          test m "missing-cookie legalhold" (testMissingCookie @ZAuth.LegalHoldUser @ZAuth.LegalHoldAccess z b),
          test m "unknown-cookie" (testUnknownCookie @ZAuth.User z b),
          test m "unknown-cookie legalhold" (testUnknownCookie @ZAuth.LegalHoldUser z b),
          test m "token mismatch" (testTokenMismatch z b g),
          test m "new-persistent-cookie" (testNewPersistentCookie conf b),
          test m "new-session-cookie" (testNewSessionCookie conf b),
          test m "suspend-inactive" (testSuspendInactiveUsers conf b)
        ],
      testGroup
        "cookies"
        [ test m "list" (testListCookies b),
          test m "remove-by-label" (testRemoveCookiesByLabel b),
          test m "remove-by-label-id" (testRemoveCookiesByLabelAndId b),
          test m "limit" (testTooManyCookies conf b),
          test m "logout" (testLogout b)
        ],
      testGroup
        "reauth"
        [ test m "reauthentication" (testReauthentication b)
        ]
    ]

--------------------------------------------------------------------------------
-- ZAuth test environment for generating arbitrary tokens.

randomAccessToken :: forall u a. ZAuth.TokenPair u a => ZAuth (ZAuth.Token a)
randomAccessToken = randomUserToken @u >>= ZAuth.newAccessToken

randomUserToken :: ZAuth.UserTokenLike u => ZAuth (ZAuth.Token u)
randomUserToken = (Id <$> liftIO UUID.nextRandom) >>= ZAuth.newUserToken

-------------------------------------------------------------------------------
-- Nginz authentication tests (end-to-end sanity checks)
--

testNginz :: Brig -> Nginz -> Http ()
testNginz b n = do
  u <- randomUser b
  let Just email = userEmail u
  -- Login with email
  rs <-
    login b (defEmailLogin email) PersistentCookie
      <!! const 200 === statusCode
  let c = decodeCookie rs
      t = decodeToken rs
  -- ensure regular user tokens can be used with (for example) /clients
  --
  -- Note: If you get 403 test failures:
  -- 1. check that the private/public keys in brig and nginz match.
  -- 2. check that the nginz acl file is correct.
  _rs <- get (n . path "/clients" . header "Authorization" ("Bearer " <> (toByteString' t)))
  liftIO $ assertEqual "Ensure nginz is started. Ensure nginz and brig share the same private/public zauth keys. Ensure ACL file is correct." 200 (statusCode _rs)
  -- ensure nginz allows refresh at /access
  _rs <-
    post (n . path "/access" . cookie c . header "Authorization" ("Bearer " <> (toByteString' t))) <!! do
      const 200 === statusCode
  -- ensure regular user tokens can fetch notifications
  get (n . path "/notifications" . header "Authorization" ("Bearer " <> (toByteString' t))) !!! const 200 === statusCode

testNginzLegalHold :: Brig -> Galley -> Nginz -> Http ()
testNginzLegalHold b g n = do
  -- create team user Alice
  (alice, tid) <- createUserWithTeam b
  putLegalHoldEnabled tid TeamFeatureEnabled g -- enable it for this team
  rs <-
    legalHoldLogin b (LegalHoldLogin alice (Just defPassword) Nothing) PersistentCookie
      <!! const 200 === statusCode
  let c = decodeCookie rs
      t = decodeToken' @ZAuth.LegalHoldAccess rs
  -- ensure nginz allows passing legalhold cookies / tokens through to /access
  post (n . path "/access" . cookie c . header "Authorization" ("Bearer " <> (toByteString' t))) !!! do
    const 200 === statusCode
  -- ensure legalhold tokens CANNOT fetch /clients
  get (n . path "/clients" . header "Authorization" ("Bearer " <> (toByteString' t))) !!! const 403 === statusCode
  get (n . path "/self" . header "Authorization" ("Bearer " <> (toByteString' t))) !!! const 403 === statusCode
  -- ensure legal hold tokens can fetch notifications
  get (n . path "/notifications" . header "Authorization" ("Bearer " <> (toByteString' t))) !!! const 200 === statusCode

-------------------------------------------------------------------------------
-- Login

testEmailLogin :: Brig -> Http ()
testEmailLogin brig = do
  u <- randomUser brig
  let Just email = userEmail u
  now <- liftIO getCurrentTime
  -- Login with email and do some checks
  rs <-
    login brig (defEmailLogin email) PersistentCookie
      <!! const 200 === statusCode
  liftIO $ do
    assertSanePersistentCookie @ZAuth.User (decodeCookie rs)
    assertSaneAccessToken now (userId u) (decodeToken rs)
  -- Login again, but with capitalised email address
  let Email loc dom = email
  let email' = Email (Text.toUpper loc) dom
  login brig (defEmailLogin email') PersistentCookie
    !!! const 200 === statusCode

testPhoneLogin :: Brig -> Http ()
testPhoneLogin brig = do
  p <- randomPhone
  let newUser =
        RequestBodyLBS . encode $
          object
            [ "name" .= ("Alice" :: Text),
              "phone" .= fromPhone p
            ]
  post (brig . path "/i/users" . contentJson . Http.body newUser)
    !!! const 201 === statusCode
  sendLoginCode brig p LoginCodeSMS False !!! const 200 === statusCode
  code <- getPhoneLoginCode brig p
  case code of
    Nothing -> liftIO $ assertFailure "missing login code"
    Just c ->
      login brig (SmsLogin p c Nothing) PersistentCookie
        !!! const 200 === statusCode

testHandleLogin :: Brig -> Http ()
testHandleLogin brig = do
  usr <- userId <$> randomUser brig
  hdl <- randomHandle
  let update = RequestBodyLBS . encode $ HandleUpdate hdl
  put (brig . path "/self/handle" . contentJson . zUser usr . zConn "c" . Http.body update)
    !!! const 200 === statusCode
  let l = PasswordLogin (LoginByHandle (Handle hdl)) defPassword Nothing
  login brig l PersistentCookie !!! const 200 === statusCode

-- | Check that local part after @+@ is ignored by equality on email addresses if the domain is
-- untrusted.
testLoginUntrustedDomain :: Brig -> Http ()
testLoginUntrustedDomain brig = do
  Just (Email loc dom) <- userEmail <$> createUserUntrustedEmail "Homer" brig
  -- login without "+" suffix
  let email' = Email (Text.takeWhile (/= '+') loc) dom
  login brig (defEmailLogin email') PersistentCookie
    !!! const 200 === statusCode

testSendLoginCode :: Brig -> Http ()
testSendLoginCode brig = do
  p <- randomPhone
  let newUser =
        RequestBodyLBS . encode $
          object
            [ "name" .= ("Alice" :: Text),
              "phone" .= fromPhone p,
              "password" .= ("secret" :: Text)
            ]
  post (brig . path "/i/users" . contentJson . Http.body newUser)
    !!! const 201 === statusCode
  -- Unless forcing it, SMS/voice code login is not permitted if
  -- the user has a password.
  sendLoginCode brig p LoginCodeSMS False !!! do
    const 403 === statusCode
    const (Just "password-exists") === errorLabel
  rsp1 <-
    sendLoginCode brig p LoginCodeSMS True
      <!! const 200 === statusCode
  let _timeout = fromLoginCodeTimeout <$> responseJsonMaybe rsp1
  liftIO $ assertEqual "timeout" (Just (Code.Timeout 600)) _timeout
  -- Retry with a voice call
  rsp2 <-
    sendLoginCode brig p LoginCodeVoice True
      <!! const 200 === statusCode
  -- Timeout is reset
  let _timeout = fromLoginCodeTimeout <$> responseJsonMaybe rsp2
  liftIO $ assertEqual "timeout" (Just (Code.Timeout 600)) _timeout

testLoginFailure :: Brig -> Http ()
testLoginFailure brig = do
  Just email <- userEmail <$> randomUser brig
  -- login with wrong password
  let badpw = PlainTextPassword "wrongpassword"
  login brig (PasswordLogin (LoginByEmail email) badpw Nothing) PersistentCookie
    !!! const 403 === statusCode
  -- login with wrong / non-existent email
  let badmail = Email "wrong" "wire.com"
  login brig (PasswordLogin (LoginByEmail badmail) defPassword Nothing) PersistentCookie
    !!! const 403 === statusCode
  -- Create user with only phone number
  p <- randomPhone
  let newUser =
        RequestBodyLBS . encode $
          object
            [ "name" .= ("Alice" :: Text),
              "phone" .= fromPhone p
            ]
  res <-
    post (brig . path "/i/users" . contentJson . Http.body newUser)
      <!! const 201 === statusCode
  uid <- userId <$> responseJsonError res
  eml <- randomEmail
  -- Add email
  let emailUpdate = RequestBodyLBS . encode $ EmailUpdate eml
  put (brig . path "/self/email" . contentJson . zUser uid . zConn "c" . Http.body emailUpdate)
    !!! (const 202 === statusCode)
  -- Activate
  act <- getActivationCode brig (Left eml)
  case act of
    Nothing -> liftIO $ assertFailure "missing activation key/code"
    Just kc -> do
      activate brig kc !!! const 200 === statusCode
      -- Attempt to log in without having set a password
      login brig (defEmailLogin eml) PersistentCookie !!! do
        const 403 === statusCode
        const (Just "invalid-credentials") === errorLabel

testThrottleLogins :: Opts.Opts -> Brig -> Http ()
testThrottleLogins conf b = do
  -- Get the maximum amount of times we are allowed to login before
  -- throttling begins
  let l = Opts.setUserCookieLimit (Opts.optSettings conf)
  u <- randomUser b
  let Just e = userEmail u
  -- Login exactly that amount of times, as fast as possible
  pooledForConcurrentlyN_ 8 [1 .. l] $ \_ ->
    login b (defEmailLogin e) SessionCookie
  -- Login once more. This should fail!
  x <-
    login b (defEmailLogin e) SessionCookie
      <!! const 429 === statusCode
  -- After the amount of time specified in "Retry-After", though,
  -- throttling should stop and login should work again
  let Just n = fromByteString =<< getHeader "Retry-After" x
  liftIO $ do
    assertBool "throttle delay" (n > 0)
    threadDelay (1000000 * (n + 1))
  login b (defEmailLogin e) SessionCookie !!! const 200 === statusCode

testLimitRetries :: HasCallStack => Opts.Opts -> Brig -> Http ()
testLimitRetries conf brig = do
  let Just opts = Opts.setLimitFailedLogins . Opts.optSettings $ conf
  unless (Opts.timeout opts <= 30) $
    error "`loginRetryTimeout` is the number of seconds this test is running.  Please pick a value < 30."
  usr <- randomUser brig
  let Just email = userEmail usr
  usr' <- randomUser brig
  let Just email' = userEmail usr'
  -- Login 5 times with bad password.
  forM_ [1 .. Opts.retryLimit opts] $ \_ ->
    login brig (emailLogin email defWrongPassword (Just defCookieLabel)) SessionCookie
      <!! const 403 === statusCode
  -- Login once more. This should fail for usr, even though password is correct...
  resp <-
    login brig (defEmailLogin email) SessionCookie
      <!! const 403 === statusCode
  -- ...  but not for usr'!
  login brig (defEmailLogin email') SessionCookie
    !!! const 200 === statusCode
  -- After the amount of time specified in "Retry-After", though,
  -- throttling should stop and login should work again
  do
    let Just retryAfterSecs = fromByteString =<< getHeader "Retry-After" resp
        retryTimeout = Opts.Timeout $ fromIntegral retryAfterSecs
    liftIO $ do
      assertBool
        ("throttle delay (1): " <> show (retryTimeout, Opts.timeout opts))
        -- (this accounts for slow CI systems that lose up to 2 secs)
        ( retryTimeout >= Opts.timeout opts - 2
            && retryTimeout <= Opts.timeout opts
        )
      threadDelay (1000000 * (retryAfterSecs - 2)) -- wait almost long enough.

  -- fail again later into the block time window
  rsp <- login brig (defEmailLogin email) SessionCookie <!! const 403 === statusCode
  do
    let Just retryAfterSecs = fromByteString =<< getHeader "Retry-After" rsp
    liftIO $ do
      assertBool ("throttle delay (2): " <> show retryAfterSecs) (retryAfterSecs <= 2)
      threadDelay (1000000 * (retryAfterSecs + 1)) -- wait one more second, just to be safe.

  -- wait long enough and login successfully!
  liftIO $ threadDelay (1000000 * 2)
  login brig (defEmailLogin email) SessionCookie !!! const 200 === statusCode

-------------------------------------------------------------------------------
-- LegalHold Login

testRegularUserLegalHoldLogin :: Brig -> Http ()
testRegularUserLegalHoldLogin brig = do
  -- Create a regular user
  uid <- userId <$> randomUser brig
  -- fail if user is not a team user
  legalHoldLogin brig (LegalHoldLogin uid (Just defPassword) Nothing) PersistentCookie !!! do
    const 403 === statusCode

testTeamUserLegalHoldLogin :: Brig -> Galley -> Http ()
testTeamUserLegalHoldLogin brig galley = do
  -- create team user Alice
  (alice, tid) <- createUserWithTeam brig
  now <- liftIO getCurrentTime
  -- fail if legalhold isn't activated yet for this user
  legalHoldLogin brig (LegalHoldLogin alice (Just defPassword) Nothing) PersistentCookie !!! do
    const 403 === statusCode
  putLegalHoldEnabled tid TeamFeatureEnabled galley -- enable it for this team
  _rs <-
    legalHoldLogin brig (LegalHoldLogin alice (Just defPassword) Nothing) PersistentCookie
      <!! const 200 === statusCode
  -- check for the correct (legalhold) token and cookie
  liftIO $ do
    assertSanePersistentCookie @ZAuth.LegalHoldUser (decodeCookie _rs)
    assertSaneAccessToken now alice (decodeToken' @ZAuth.LegalHoldAccess _rs)

testLegalHoldSessionCookie :: Brig -> Galley -> Http ()
testLegalHoldSessionCookie brig galley = do
  alice <- prepareLegalHoldUser brig galley
  -- attempt a legalhold login with a session cookie (?persist=false)
  rs <-
    legalHoldLogin brig (LegalHoldLogin alice (Just defPassword) Nothing) SessionCookie
      <!! const 200 === statusCode
  -- ensure server always returns a refreshable PersistentCookie irrespective of argument passed
  liftIO $ assertSanePersistentCookie @ZAuth.LegalHoldUser (decodeCookie rs)

-- | Check that @/i/legalhold-login/@ can not be used to login as a suspended
-- user.
testSuspendedLegalHoldLogin :: Brig -> Http ()
testSuspendedLegalHoldLogin brig = do
  -- Create a user and immediately suspend them
  (uid, _tid) <- createUserWithTeam brig
  setStatus brig uid Suspended
  -- Try to login and see if we fail
  legalHoldLogin brig (LegalHoldLogin uid (Just defPassword) Nothing) PersistentCookie !!! do
    const 403 === statusCode
    const (Just "suspended") === errorLabel

-- | Check that @/i/legalhold-login@ fails if the user doesn't exist.
testNoUserLegalHoldLogin :: Brig -> Http ()
testNoUserLegalHoldLogin brig = do
  -- Try to login with random UID and see if we fail
  uid <- randomId
  legalHoldLogin brig (LegalHoldLogin uid (Just defPassword) Nothing) PersistentCookie !!! do
    const 403 === statusCode
    const (Just "invalid-credentials") === errorLabel

testWrongPasswordLegalHoldLogin :: Brig -> Galley -> Http ()
testWrongPasswordLegalHoldLogin brig galley = do
  alice <- prepareLegalHoldUser brig galley
  -- attempt a legalhold login with a wrong password
  legalHoldLogin brig (LegalHoldLogin alice (Just (PlainTextPassword "wrong-password")) Nothing) PersistentCookie !!! do
    const 403 === statusCode
    const (Just "invalid-credentials") === errorLabel
  -- attempt a legalhold login with a no password
  legalHoldLogin brig (LegalHoldLogin alice Nothing Nothing) PersistentCookie !!! do
    const 403 === statusCode
    const (Just "missing-auth") === errorLabel

testLegalHoldLogout :: Brig -> Galley -> Http ()
testLegalHoldLogout brig galley = do
  uid <- prepareLegalHoldUser brig galley
  _rs <- legalHoldLogin brig (LegalHoldLogin uid (Just defPassword) Nothing) PersistentCookie <!! const 200 === statusCode
  let (t, c) = (decodeToken' @ZAuth.LegalHoldAccess _rs, decodeCookie _rs)
  post (brig . path "/access" . cookie c)
    !!! const 200 === statusCode
  post (brig . path "/access/logout" . cookie c . queryItem "access_token" (toByteString' t))
    !!! const 200 === statusCode
  post (brig . path "/access" . cookie c)
    !!! const 403 === statusCode

-------------------------------------------------------------------------------
-- Sso login

-- | Check that login works with @/sso-login@ even without having the
-- right password.
testEmailSsoLogin :: Brig -> Http ()
testEmailSsoLogin brig = do
  -- Create a user
  uid <- userId <$> randomUser brig
  now <- liftIO getCurrentTime
  -- Login and do some checks
  _rs <-
    ssoLogin brig (SsoLogin uid Nothing) PersistentCookie
      <!! const 200 === statusCode
  liftIO $ do
    assertSanePersistentCookie @ZAuth.User (decodeCookie _rs)
    assertSaneAccessToken now uid (decodeToken _rs)

-- | Check that @/sso-login@ can not be used to login as a suspended
-- user.
testSuspendedSsoLogin :: Brig -> Http ()
testSuspendedSsoLogin brig = do
  -- Create a user and immediately suspend them
  uid <- userId <$> randomUser brig
  setStatus brig uid Suspended
  -- Try to login and see if we fail
  ssoLogin brig (SsoLogin uid Nothing) PersistentCookie !!! do
    const 403 === statusCode
    const (Just "suspended") === errorLabel

-- | Check that @/sso-login@ fails if the user doesn't exist.
testNoUserSsoLogin :: Brig -> Http ()
testNoUserSsoLogin brig = do
  -- Try to login with random UID and see if we fail
  uid <- randomId
  ssoLogin brig (SsoLogin uid Nothing) PersistentCookie !!! do
    const 403 === statusCode
    const (Just "invalid-credentials") === errorLabel

-------------------------------------------------------------------------------
-- Token Refresh

testInvalidCookie :: forall u. ZAuth.UserTokenLike u => ZAuth.Env -> Brig -> Http ()
testInvalidCookie z b = do
  -- Syntactically invalid
  post (b . path "/access" . cookieRaw "zuid" "xxx") !!! do
    const 403 === statusCode
    const (Just "Invalid user token") =~= responseBody
  -- Expired
  user <- userId <$> randomUser b
  let f = set (ZAuth.userTTL (Proxy @u)) 0
  t <- toByteString' <$> runZAuth z (ZAuth.localSettings f (ZAuth.newUserToken @u user))
  liftIO $ threadDelay 1000000
  post (b . path "/access" . cookieRaw "zuid" t) !!! do
    const 403 === statusCode
    const (Just "expired") =~= responseBody

testInvalidToken :: Brig -> Http ()
testInvalidToken b = do
  -- Syntactically invalid
  post (b . path "/access" . queryItem "access_token" "xxx")
    !!! errResponse
  post (b . path "/access" . header "Authorization" "Bearer xxx")
    !!! errResponse
  where
    errResponse = do
      const 403 === statusCode
      const (Just "Invalid access token") =~= responseBody

testMissingCookie :: forall u a. ZAuth.TokenPair u a => ZAuth.Env -> Brig -> Http ()
testMissingCookie z b = do
  -- Missing cookie, i.e. token refresh mandates a cookie.
  post (b . path "/access")
    !!! errResponse
  t <- toByteString' <$> runZAuth z (randomAccessToken @u @a)
  post (b . path "/access" . header "Authorization" ("Bearer " <> t))
    !!! errResponse
  post (b . path "/access" . queryItem "access_token" t)
    !!! errResponse
  where
    errResponse = do
      const 403 === statusCode
      const (Just "Missing cookie") =~= responseBody
      const (Just "invalid-credentials") =~= responseBody

testUnknownCookie :: forall u. ZAuth.UserTokenLike u => ZAuth.Env -> Brig -> Http ()
testUnknownCookie z b = do
  -- Valid cookie but unknown to the server.
  t <- toByteString' <$> runZAuth z (randomUserToken @u)
  post (b . path "/access" . cookieRaw "zuid" t) !!! do
    const 403 === statusCode
    const (Just "invalid-credentials") =~= responseBody

testTokenMismatch :: ZAuth.Env -> Brig -> Galley -> Http ()
testTokenMismatch z brig galley = do
  u <- randomUser brig
  let Just email = userEmail u
  _rs <-
    login brig (emailLogin email defPassword (Just "nexus1")) PersistentCookie
      <!! const 200 === statusCode
  -- try refresh with a regular UserCookie but a LegalHoldAccessToken
  let c = decodeCookie _rs
  t <- toByteString' <$> runZAuth z (randomAccessToken @ZAuth.LegalHoldUser @ZAuth.LegalHoldAccess)
  post (brig . path "/access" . cookie c . header "Authorization" ("Bearer " <> t)) !!! do
    const 403 === statusCode
    const (Just "Token mismatch") =~= responseBody
  -- try refresh with a regular AccessToken but a LegalHoldUserCookie
  (alice, tid) <- createUserWithTeam brig
  putLegalHoldEnabled tid TeamFeatureEnabled galley -- enable it for this team
  _rs <- legalHoldLogin brig (LegalHoldLogin alice (Just defPassword) Nothing) PersistentCookie
  let c' = decodeCookie _rs
  t' <- toByteString' <$> runZAuth z (randomAccessToken @ZAuth.User @ZAuth.Access)
  post (brig . path "/access" . cookie c' . header "Authorization" ("Bearer " <> t')) !!! do
    const 403 === statusCode
    const (Just "Token mismatch") =~= responseBody

testNewPersistentCookie :: Opts.Opts -> Brig -> Http ()
testNewPersistentCookie config b = do
  u <- randomUser b
  let renewAge = Opts.setUserCookieRenewAge $ Opts.optSettings config
  let minAge = fromIntegral $ renewAge * 1000000 + 1
      Just email = userEmail u
  _rs <-
    login b (emailLogin email defPassword (Just "nexus1")) PersistentCookie
      <!! const 200 === statusCode
  let c = decodeCookie _rs
  -- Wait for the cookie to be eligible for renewal
  liftIO $ threadDelay minAge
  -- Refresh tokens
  _rs <-
    post (b . path "/access" . cookie c) <!! do
      const 200 === statusCode
      const Nothing =/= getHeader "Set-Cookie"
      const (Just "access_token") =~= responseBody
  let c' = decodeCookie _rs
  liftIO $ assertBool "expiry" (cookie_expiry_time c' > cookie_expiry_time c)
  -- Refresh with the old cookie should still work for the
  -- duration of another BRIG_COOKIE_RENEW_AGE seconds,
  -- but the response should keep advertising the new cookie.
  _rs <-
    post (b . path "/access" . cookie c) <!! do
      const 200 === statusCode
      const Nothing =/= getHeader "Set-Cookie"
      const (Just "access_token") =~= responseBody
  -- we got a new cookie value, but the key is the same
  liftIO $ assertBool "cookie" (c' `equivCookie` decodeCookie _rs)
  -- Refresh with the new cookie should succeed
  -- (without advertising yet another new cookie).
  post (b . path "/access" . cookie c') !!! do
    const 200 === statusCode
    const Nothing === getHeader "Set-Cookie"
    const (Just "access_token") =~= responseBody
  -- Check cookies
  _cs <- listCookies b (userId u)
  let (old, new) = partition (isJust . cookieSucc) _cs
  liftIO $ do
    2 @=? length _cs
    mapMaybe cookieSucc old @=? map cookieId new
    [PersistentCookie, PersistentCookie] @=? map cookieType _cs
  -- Wait for the old cookie to disappear
  liftIO $ threadDelay minAge
  _cs <- listCookies b (userId u)
  liftIO $ do
    [PersistentCookie] @=? map cookieType _cs
    [Nothing] @=? map cookieSucc _cs

testNewSessionCookie :: Opts.Opts -> Brig -> Http ()
testNewSessionCookie config b = do
  u <- randomUser b
  let renewAge = Opts.setUserCookieRenewAge $ Opts.optSettings config
  let minAge = fromIntegral $ renewAge * 1000000 + 1
      Just email = userEmail u
  _rs <-
    login b (emailLogin email defPassword (Just "nexus1")) SessionCookie
      <!! const 200 === statusCode
  let c = decodeCookie _rs
  liftIO $ threadDelay minAge
  -- Session cookies are never renewed
  post (b . path "/access" . cookie c) !!! do
    const 200 === statusCode
    const Nothing === getHeader "Set-Cookie"

testSuspendInactiveUsers :: HasCallStack => Opts.Opts -> Brig -> Http ()
testSuspendInactiveUsers config brig = do
  -- (context information: cookies are stored by user, not be device; so if there if the
  -- cookie is old it means none of the devices of a user has used it for a request.)

  let Just suspendAge = Opts.suspendTimeout <$> Opts.setSuspendInactiveUsers (Opts.optSettings config)
  unless (suspendAge <= 30) $
    error "`suspendCookiesOlderThanSecs` is the number of seconds this test is running.  Please pick a value < 30."
  let check :: HasCallStack => CookieType -> String -> Http ()
      check cookieType endPoint = do
        user <- randomUser brig
        let Just email = userEmail user
        rs <-
          login brig (emailLogin email defPassword Nothing) cookieType
            <!! const 200 === statusCode
        let cky = decodeCookie rs
        -- wait slightly longer than required for being marked as inactive.
        let waitTime :: Int = floor (Opts.timeoutDiff suspendAge) + 5 -- adding 1 *should* be enough, but it's not.
        liftIO $ threadDelay (1000000 * waitTime)
        case endPoint of
          "/access" -> do
            post (brig . path "/access" . cookie cky) !!! do
              const 403 === statusCode
              const Nothing === getHeader "Set-Cookie"
          "/login" -> do
            login brig (emailLogin email defPassword Nothing) cookieType !!! do
              const 403 === statusCode
              const Nothing === getHeader "Set-Cookie"
        let assertStatus want = do
              have <-
                retrying
                  (exponentialBackoff 200000 <> limitRetries 6)
                  (\_ have -> pure $ have == Suspended)
                  (\_ -> getStatus brig (userId user))
              let errmsg = "testSuspendInactiveUsers: " <> show (want, cookieType, endPoint, waitTime, suspendAge)
              liftIO $ HUnit.assertEqual errmsg want have
        assertStatus Suspended
        setStatus brig (userId user) Active
        assertStatus Active
        login brig (emailLogin email defPassword Nothing) cookieType
          !!! const 200 === statusCode
  check SessionCookie "/access"
  check SessionCookie "/login"
  check PersistentCookie "/access"
  check PersistentCookie "/login"

-------------------------------------------------------------------------------
-- Cookie Management

testListCookies :: Brig -> Http ()
testListCookies b = do
  u <- randomUser b
  let Just e = userEmail u
  void $ login b (emailLogin e defPassword (Just "nexus1")) SessionCookie
  _cookies <- mapMaybe cookieLabel <$> listCookies b (userId u)
  liftIO $ ["nexus1"] @=? _cookies
  void $ login b (emailLogin e defPassword (Just "nexus2")) SessionCookie
  void $ login b (emailLogin e defPassword (Just "nexus3")) SessionCookie
  _cookies <- mapMaybe cookieLabel <$> listCookies b (userId u)
  liftIO $ ["nexus1", "nexus2", "nexus3"] @=? sort _cookies

testRemoveCookiesByLabel :: Brig -> Http ()
testRemoveCookiesByLabel b = do
  u <- randomUser b
  let Just e = userEmail u
  void $ login b (emailLogin e defPassword (Just "nexus1")) SessionCookie
  void $ login b (emailLogin e defPassword (Just "nexus2")) SessionCookie
  void $ login b (emailLogin e defPassword (Just "nexus3")) SessionCookie
  _cookies <- mapMaybe cookieLabel <$> listCookies b (userId u)
  liftIO $ ["nexus1", "nexus2", "nexus3"] @=? sort _cookies
  let rem1 = encode $ remJson defPassword (Just ["nexus1"]) Nothing
  post
    ( b . path "/cookies/remove"
        . content "application/json"
        . lbytes rem1
        . zUser (userId u)
    )
    !!! const 200
    === statusCode
  _cookies <- mapMaybe cookieLabel <$> listCookies b (userId u)
  liftIO $ ["nexus2", "nexus3"] @=? sort _cookies
  let rem2 = encode $ remJson defPassword Nothing Nothing
  post
    ( b
        . path "/cookies/remove"
        . content "application/json"
        . lbytes rem2
        . zUser (userId u)
    )
    !!! const 200
    === statusCode
  listCookies b (userId u) >>= liftIO . ([] @=?) . mapMaybe cookieLabel

testRemoveCookiesByLabelAndId :: Brig -> Http ()
testRemoveCookiesByLabelAndId b = do
  u <- randomUser b
  let Just e = userEmail u
  -- Create some cookies
  forM_ ["nexus1", "nexus2", "nexus3", "nexus4"] $ \l ->
    login b (emailLogin e defPassword (Just l)) SessionCookie
  [c1, c2, c3, c4] <- listCookies b (userId u)
  -- Remove 2 cookies by ID and 1 by label, leaving 1
  let rmLabel = maybeToList (cookieLabel c2)
  let rmIds = map cookieId [c1, c3]
  let rmJs = encode $ remJson defPassword (Just rmLabel) (Just rmIds)
  post
    ( b
        . path "/cookies/remove"
        . content "application/json"
        . lbytes rmJs
        . zUser (userId u)
    )
    !!! const 200
    === statusCode
  -- Check the remaining cookie
  let lbl = cookieLabel c4
  listCookies b (userId u) >>= liftIO . ([lbl] @=?) . map cookieLabel

testTooManyCookies :: Opts.Opts -> Brig -> Http ()
testTooManyCookies config b = do
  u <- randomUser b
  let l = Opts.setUserCookieLimit (Opts.optSettings config)
  let Just e = userEmail u
      carry = 2
      pwlP = emailLogin e defPassword (Just "persistent")
      pwlS = emailLogin e defPassword (Just "session")
  void $
    concurrently
      -- Persistent logins
      ( do
          tcs <- replicateM (l + carry) $ loginWhenAllowed pwlP PersistentCookie
          cs <- listCookiesWithLabel b (userId u) ["persistent"]
          liftIO $ map cookieId cs @=? map (getCookieId @ZAuth.User) (drop carry tcs)
      )
      -- Session logins
      ( do
          tcs' <- replicateM (l + carry) $ loginWhenAllowed pwlS SessionCookie
          cs' <- listCookiesWithLabel b (userId u) ["session"]
          liftIO $ map cookieId cs' @=? map (getCookieId @ZAuth.User) (drop carry tcs')
      )
  where
    -- We expect that after `setUserCookieLimit` login attempts, we get rate
    -- limited; in those cases, we need to wait `Retry-After` seconds.
    loginWhenAllowed pwl t = do
      x <- login b pwl t <* wait
      case statusCode x of
        200 -> return $ decodeCookie x
        429 -> do
          -- After the amount of time specified in "Retry-After", though,
          -- throttling should stop and login should work again
          let Just n = fromByteString =<< getHeader "Retry-After" x
          liftIO $ threadDelay (1000000 * (n + 1))
          loginWhenAllowed pwl t
        403 ->
          error
            ( "forbidden; "
                <> "perhaps setSuspendInactiveUsers.suspendTimeout is too small? "
                <> "(try 29 seconds)."
            )
        xxx -> error ("Unexpected status code when logging in: " ++ show xxx)

testLogout :: Brig -> Http ()
testLogout b = do
  Just email <- userEmail <$> randomUser b
  _rs <- login b (defEmailLogin email) SessionCookie
  let (t, c) = (decodeToken _rs, decodeCookie _rs)
  post (b . path "/access" . cookie c)
    !!! const 200 === statusCode
  post (b . path "/access/logout" . cookie c . queryItem "access_token" (toByteString' t))
    !!! const 200 === statusCode
  post (b . path "/access" . cookie c)
    !!! const 403 === statusCode

testReauthentication :: Brig -> Http ()
testReauthentication b = do
  u <- userId <$> randomUser b
  let js = Http.body . RequestBodyLBS . encode $ object ["foo" .= ("bar" :: Text)]
  get (b . paths ["/i/users", toByteString' u, "reauthenticate"] . contentJson . js) !!! do
    const 403 === statusCode
  -- it's ok to not give a password in the request body, but if the user has a password set,
  -- response will be `forbidden`.

  get (b . paths ["/i/users", toByteString' u, "reauthenticate"] . contentJson . payload (Just $ PlainTextPassword "123456")) !!! do
    const 403 === statusCode
    const (Just "invalid-credentials") === errorLabel
  get (b . paths ["/i/users", toByteString' u, "reauthenticate"] . contentJson . payload (Just defPassword)) !!! do
    const 200 === statusCode
  setStatus b u Suspended
  get (b . paths ["/i/users", toByteString' u, "reauthenticate"] . contentJson . payload (Just defPassword)) !!! do
    const 403 === statusCode
    const (Just "suspended") === errorLabel
  where
    payload = Http.body . RequestBodyLBS . encode . ReAuthUser

-----------------------------------------------------------------------------
-- Helpers

prepareLegalHoldUser :: Brig -> Galley -> Http (UserId)
prepareLegalHoldUser brig galley = do
  (uid, tid) <- createUserWithTeam brig
  -- enable it for this team - without that, legalhold login will fail.
  putLegalHoldEnabled tid TeamFeatureEnabled galley
  return uid

decodeCookie :: HasCallStack => Response a -> Http.Cookie
decodeCookie = fromMaybe (error "missing zuid cookie") . getCookie "zuid"

decodeToken :: HasCallStack => Response (Maybe Lazy.ByteString) -> ZAuth.AccessToken
decodeToken = decodeToken' @ZAuth.Access

decodeToken' :: (HasCallStack, ZAuth.AccessTokenLike a) => Response (Maybe Lazy.ByteString) -> ZAuth.Token a
decodeToken' r = fromMaybe (error "invalid access_token") $ do
  x <- responseBody r
  t <- x ^? key "access_token" . _String
  fromByteString (encodeUtf8 t)

getCookieId :: forall u. (HasCallStack, ZAuth.UserTokenLike u) => Http.Cookie -> CookieId
getCookieId c =
  maybe
    (error "no cookie value")
    (CookieId . ZAuth.userTokenRand @u)
    (fromByteString (cookie_value c))

listCookies :: HasCallStack => Brig -> UserId -> Http [Auth.Cookie ()]
listCookies b u = listCookiesWithLabel b u []

listCookiesWithLabel :: HasCallStack => Brig -> UserId -> [CookieLabel] -> Http [Auth.Cookie ()]
listCookiesWithLabel b u l = do
  rs <-
    get
      ( b . path "/cookies"
          . queryItem "labels" labels
          . header "Z-User" (toByteString' u)
      )
      <!! const 200 === statusCode
  let Just cs = cookieList <$> responseJsonMaybe rs
  return cs
  where
    labels = BS.intercalate "," $ map toByteString' l

-- | Check that the cookie returned after login is sane.
--
-- Doesn't check everything, just some basic properties.
assertSanePersistentCookie :: forall u. ZAuth.UserTokenLike u => Http.Cookie -> Assertion
assertSanePersistentCookie ck = do
  assertBool "type" (cookie_persistent ck)
  assertBool "http-only" (cookie_http_only ck)
  assertBool "expiry" (cookie_expiry_time ck > cookie_creation_time ck)
  assertBool "domain" (cookie_domain ck /= "")
  assertBool "path" (cookie_path ck /= "")
  let Just (token :: ZAuth.Token u) = fromByteString (cookie_value ck)
      tokentype = ZAuth.zauthType @u
  assertBool "type field (t=)" $ token ^. ZAuth.header . ZAuth.typ == tokentype

-- | Check that the access token returned after login is sane.
assertSaneAccessToken ::
  ZAuth.AccessTokenLike a =>
  -- | Some moment in time before the user was created
  UTCTime ->
  UserId ->
  ZAuth.Token a ->
  Assertion
assertSaneAccessToken now uid tk = do
  assertEqual "user" uid (ZAuth.accessTokenOf tk)
  assertBool "expiry" (ZAuth.tokenExpiresUTC tk > now)

-- | Get error label from the response (for use in assertions).
errorLabel :: Response (Maybe Lazy.ByteString) -> Maybe Lazy.Text
errorLabel = fmap Error.label . responseJsonMaybe

remJson :: PlainTextPassword -> Maybe [CookieLabel] -> Maybe [CookieId] -> Value
remJson p l ids =
  object
    [ "password" .= p,
      "labels" .= l,
      "ids" .= ids
    ]

wait :: MonadIO m => m ()
wait = liftIO $ threadDelay 1000000
