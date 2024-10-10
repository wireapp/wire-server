{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

module API.User.Auth
  ( tests,
  )
where

import API.Team.Util
import Bilge hiding (body)
import Bilge qualified as Http
import Bilge.Assert hiding (assert)
import Brig.Options qualified as Opts
import Brig.ZAuth (ZAuth, runZAuth)
import Brig.ZAuth qualified as ZAuth
import Cassandra hiding (Value)
import Cassandra qualified as DB
import Control.Arrow ((&&&))
import Control.Lens (set, (^.))
import Control.Retry
import Data.Aeson as Aeson hiding (json)
import Data.ByteString qualified as BS
import Data.ByteString.Conversion
import Data.ByteString.Lazy qualified as Lazy
import Data.Handle (parseHandle)
import Data.Id
import Data.Misc (PlainTextPassword6, plainTextPassword6, plainTextPassword6Unsafe)
import Data.Proxy
import Data.Qualified
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO (hPutStrLn)
import Data.Text.Lazy qualified as Lazy
import Data.Time.Clock
import Data.UUID.V4 qualified as UUID
import Data.ZAuth.Token qualified as ZAuth
import Imports
import Network.HTTP.Client (equivCookie)
import Network.Wai.Utilities.Error qualified as Error
import Test.Tasty hiding (Timeout)
import Test.Tasty.HUnit
import Test.Tasty.HUnit qualified as HUnit
import UnliftIO.Async hiding (wait)
import Util
import Util.Timeout
import Wire.API.Conversation (Conversation (..))
import Wire.API.Password (Password, mkSafePassword)
import Wire.API.User as Public
import Wire.API.User.Auth as Auth
import Wire.API.User.Auth.LegalHold
import Wire.API.User.Auth.ReAuth
import Wire.API.User.Auth.Sso
import Wire.API.User.Client

-- | FUTUREWORK: Implement this function. This wrapper should make sure that
-- wrapped tests run only when the feature flag 'legalhold' is set to
-- 'whitelist-teams-and-implicit-consent' in galley's config. If tests marked
-- with this are failing then assumption that
-- 'whitelist-teams-and-implicit-consent' is set in all test environments is no
-- longer correct.
onlyIfLhWhitelisted :: (MonadIO m) => m () -> m ()
onlyIfLhWhitelisted action = do
  let isGalleyLegalholdFeatureWhitelist = True
  if isGalleyLegalholdFeatureWhitelist
    then action
    else
      liftIO $
        hPutStrLn
          stderr
          "*** skipping test. This test only works if you manually adjust the server config files\
          \(the 'withLHWhitelist' trick does not work because it does not allow \
          \brig to talk to the dynamically spawned galley)."

tests :: Opts.Opts -> Manager -> ZAuth.Env -> DB.ClientState -> Brig -> Galley -> Nginz -> TestTree
tests conf m z db b g n =
  testGroup
    "auth"
    [ testGroup
        "login"
        [ test m "email" (testEmailLogin b),
          test m "handle" (testHandleLogin b),
          test m "email-untrusted-domain" (testLoginUntrustedDomain b),
          test m "testLoginFailure - failure" (testLoginFailure b),
          test m "throttle" (testThrottleLogins conf b),
          test m "testLimitRetries - limit-retry" (testLimitRetries conf b),
          test m "login with 6 character password" (testLoginWith6CharPassword b db),
          testGroup
            "sso-login"
            [ test m "email" (testEmailSsoLogin b),
              test m "failure-suspended" (testSuspendedSsoLogin b),
              test m "failure-no-user" (testNoUserSsoLogin b)
            ],
          testGroup
            "legalhold-login"
            [ test m "failure-no-team" (testRegularUserLegalHoldLogin b),
              test m "team-user-with-legalhold-enabled" (onlyIfLhWhitelisted (testTeamUserLegalHoldLogin b g)),
              test m "failure-suspended" (testSuspendedLegalHoldLogin b),
              test m "failure-no-user" (testNoUserLegalHoldLogin b),
              test m "failure-wrong-password" (onlyIfLhWhitelisted (testWrongPasswordLegalHoldLogin b g)),
              test m "always-persistent-cookie" (onlyIfLhWhitelisted (testLegalHoldSessionCookie b g)),
              test m "legalhold-logout" (onlyIfLhWhitelisted (testLegalHoldLogout b g))
            ],
          testGroup
            "nginz"
            [ test m "nginz-login" (testNginz b n),
              test m "nginz-legalhold-login" (onlyIfLhWhitelisted (testNginzLegalHold b g n)),
              test m "nginz-login-multiple-cookies" (testNginzMultipleCookies conf b n)
            ]
        ],
      testGroup
        "refresh /access"
        [ test m "testInvalidCookie - invalid-cookie" (testInvalidCookie @ZAuth.User z b),
          test m "testInvalidCookie - invalid-cookie legalhold" (testInvalidCookie @ZAuth.LegalHoldUser z b),
          test m "invalid-token" (testInvalidToken z b),
          test m "missing-cookie" (testMissingCookie @ZAuth.User @ZAuth.Access z b),
          test m "missing-cookie legalhold" (testMissingCookie @ZAuth.LegalHoldUser @ZAuth.LegalHoldAccess z b),
          test m "unknown-cookie" (testUnknownCookie @ZAuth.User z b),
          test m "unknown-cookie legalhold" (testUnknownCookie @ZAuth.LegalHoldUser z b),
          test m "token mismatch" (onlyIfLhWhitelisted (testTokenMismatchLegalhold z b g)),
          test m "new-persistent-cookie" (testNewPersistentCookie conf b),
          test m "new-session-cookie" (testNewSessionCookie conf b),
          testGroup "suspend-inactive" $ do
            cookieType <- [SessionCookie, PersistentCookie]
            endPoint <- ["/access", "/login"]
            let testName = "cookieType=" <> show cookieType <> ",endPoint=" <> show endPoint
            pure $ test m testName $ testSuspendInactiveUsers conf b cookieType endPoint,
          test m "client access" (testAccessWithClientId b),
          test m "client access with old token" (testAccessWithClientIdAndOldToken b),
          test m "client access incorrect" (testAccessWithIncorrectClientId b),
          test m "multiple client accesses" (testAccessWithExistingClientId b)
        ],
      testGroup
        "update /access/self/email"
        [ test m "valid token (idempotency case) (with cookie)" (testAccessSelfEmailAllowed n b True),
          test m "valid token (idempotency case) (without cookie)" (testAccessSelfEmailAllowed n b False),
          test m "invalid or missing token (with cookie)" (testAccessSelfEmailDenied z n b True),
          test m "invalid or missing token (without cookie)" (testAccessSelfEmailDenied z n b False)
        ],
      testGroup
        "cookies"
        [ test m "list" (testListCookies b),
          test m "remove-by-label" (testRemoveCookiesByLabel b),
          test m "remove-by-label-id" (testRemoveCookiesByLabelAndId b),
          test m "testTooManyCookies - limit" (testTooManyCookies conf b),
          test m "logout" (testLogout b)
        ],
      testGroup
        "reauth"
        [ test m "reauthentication" (testReauthentication b)
        ]
    ]

testLoginWith6CharPassword :: Brig -> DB.ClientState -> Http ()
testLoginWith6CharPassword brig db = do
  (uid, Just email) <- (userId &&& userEmail) <$> randomUser brig
  checkLogin email defPassword 200
  let pw6 = plainTextPassword6Unsafe "123456"
  writeDirectlyToDB uid pw6
  checkLogin email defPassword 403
  checkLogin email pw6 200
  where
    checkLogin :: EmailAddress -> PlainTextPassword6 -> Int -> Http ()
    checkLogin email pw expectedStatusCode =
      login
        brig
        (MkLogin (LoginByEmail email) pw Nothing Nothing)
        PersistentCookie
        !!! const expectedStatusCode === statusCode

    -- Since 8 char passwords are required, when setting a password via the API,
    -- we need to write this directly to the db, to be able to test this
    writeDirectlyToDB :: UserId -> PlainTextPassword6 -> Http ()
    writeDirectlyToDB uid pw =
      liftIO (runClient db (updatePassword uid pw >> deleteAllCookies uid))

    updatePassword :: (MonadClient m) => UserId -> PlainTextPassword6 -> m ()
    updatePassword u t = do
      p <- mkSafePassword Nothing t
      retry x5 $ write userPasswordUpdate (params LocalQuorum (p, u))

    userPasswordUpdate :: PrepQuery W (Password, UserId) ()
    userPasswordUpdate = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE user SET password = ? WHERE id = ?"

    deleteAllCookies :: (MonadClient m) => UserId -> m ()
    deleteAllCookies u = retry x5 (write cql (params LocalQuorum (Identity u)))
      where
        cql :: PrepQuery W (Identity UserId) ()
        cql = "DELETE FROM user_cookies WHERE user = ?"

--------------------------------------------------------------------------------
-- ZAuth test environment for generating arbitrary tokens.

randomAccessToken :: forall u a. (ZAuth.TokenPair u a) => ZAuth (ZAuth.Token a)
randomAccessToken = randomUserToken @u >>= ZAuth.newAccessToken

randomUserToken :: (ZAuth.UserTokenLike u) => ZAuth (ZAuth.Token u)
randomUserToken = do
  r <- Id <$> liftIO UUID.nextRandom
  ZAuth.newUserToken r Nothing

-------------------------------------------------------------------------------
-- Nginz authentication tests (end-to-end sanity checks)
--

testNginz :: Brig -> Nginz -> Http ()
testNginz b n = do
  u <- randomUser b
  let Just email = userEmail u
  -- Login with email
  rs <-
    login n (defEmailLogin email) PersistentCookie
      <!! const 200 === statusCode
  let c = decodeCookie rs
      t = decodeToken rs
  -- ensure regular user tokens can be used with (for example) /clients
  --
  -- Note: If you get 403 test failures:
  -- 1. check that the private/public keys in brig and nginz match.
  -- 2. check that the nginz acl file is correct.
  _rs <- get (n . path "/clients" . header "Authorization" ("Bearer " <> toByteString' t))
  liftIO $ assertEqual "Ensure nginz is started. Ensure nginz and brig share the same private/public zauth keys. Ensure ACL file is correct." 200 (statusCode _rs)
  -- ensure nginz allows refresh at /access
  _rs <-
    post (unversioned . n . path "/access" . cookie c . header "Authorization" ("Bearer " <> toByteString' t)) <!! do
      const 200 === statusCode
  -- ensure regular user tokens can fetch notifications
  get (n . path "/notifications" . header "Authorization" ("Bearer " <> toByteString' t)) !!! const 200 === statusCode

testNginzLegalHold :: Brig -> Galley -> Nginz -> Http ()
testNginzLegalHold b g n = do
  -- create team user Alice
  (alice, tid) <- createUserWithTeam' b
  putLHWhitelistTeam g tid !!! const 200 === statusCode
  (c, t) <- do
    -- we need to get the cookie domain from a login through nginz.  otherwise, if brig and
    -- nginz are running on different hosts, no cookie will be presented in the later requests
    -- to nginz in this test.  for simplicity, we use the internal end-point for
    -- authenticating an LH dev, and then steal the domain from a cookie obtained via user
    -- login.
    rsUsr <- do
      let Just email = userEmail alice
      login n (defEmailLogin email) PersistentCookie
        <!! const 200 === statusCode
    rsLhDev <-
      legalHoldLogin b (LegalHoldLogin (userId alice) (Just defPassword) Nothing) PersistentCookie
        <!! const 200 === statusCode
    let t = decodeToken' @ZAuth.LegalHoldAccess rsLhDev
        c = cLhDev {cookie_domain = cookie_domain cUsr}
        cLhDev = decodeCookie rsLhDev
        cUsr = decodeCookie rsUsr
    pure (c, t)

  qconv <-
    fmap cnvQualifiedId . responseJsonError
      =<< createConversation g (userId alice) [] <!! const 201 === statusCode

  -- ensure nginz allows passing legalhold cookies / tokens through to /access
  post (unversioned . n . path "/access" . cookie c . header "Authorization" ("Bearer " <> toByteString' t)) !!! do
    const 200 === statusCode
  -- ensure legalhold tokens CANNOT fetch /clients
  get (n . path "/clients" . header "Authorization" ("Bearer " <> toByteString' t)) !!! const 403 === statusCode
  get (n . path "/self" . header "Authorization" ("Bearer " <> toByteString' t)) !!! const 403 === statusCode
  -- ensure legal hold tokens can fetch notifications
  get (n . path "/notifications" . header "Authorization" ("Bearer " <> toByteString' t)) !!! const 200 === statusCode

  get (apiVersion "v1" . n . paths ["legalhold", "conversations", toByteString' (qUnqualified qconv)] . header "Authorization" ("Bearer " <> toByteString' t)) !!! const 200 === statusCode

  get (apiVersion "v2" . n . paths ["conversations", toByteString' (qUnqualified qconv)] . header "Authorization" ("Bearer " <> toByteString' t)) !!! const 200 === statusCode

-- | Corner case for 'testNginz': when upgrading a wire backend from the old behavior (setting
-- cookie domain to eg. @*.wire.com@) to the new behavior (leaving cookie domain empty,
-- effectively setting it to the backend host), clients may start sending two cookies for a
-- while: because the domains differ, new ones will not overwrite old ones locally (it was seen
-- on different browsers) although the cookie does get overriden in the DB). This should be handled
-- gracefully (ie., one invalid cookie should just be ignored if up to two cookies with label @"zuid"@ are present).
--
-- In this test, we actually don't use different domains - testing that correctly is actually pretty
-- complex. The reason why testing 2 domains is complicated has to do with the fact that we need to
-- have 2 distinct domains that point to the same backend; i.e., this is the case on our staging env where
-- old cookies had the domain to the TLD and new ones to <subdomain>.TLD. This is hard to do on k8s for instance
--
-- Instead, we simply set 2 cookies with the same name and different values and the http client
-- will replicate the situation: we have 2 cookies, one of them is incorrect (but must parse correctly!)
-- and the other is valid.
-- In order to make the test even more similar, we also use VALID and NON-EXPIRED but SUPERSEDED cookies
-- cookies to test sending 2 perfectly valid cookies where one of them simply got overriden due to the revew age
testNginzMultipleCookies :: Opts.Opts -> Brig -> Nginz -> Http ()
testNginzMultipleCookies o b n = do
  u <- randomUser b
  let Just email = userEmail u
      dologin :: (HasCallStack) => Http ResponseLBS
      dologin = login n (defEmailLogin email) PersistentCookie <!! const 200 === statusCode
  unparseableCookie <- (\c -> c {cookie_value = "ThisIsNotAZauthCookie"}) . decodeCookie <$> dologin
  badCookie1 <- (\c -> c {cookie_value = "SKsjKQbiqxuEugGMWVbq02fNEA7QFdNmTiSa1Y0YMgaEP5tWl3nYHWlIrM5F8Tt7Cfn2Of738C7oeiY8xzPHAB==.v=1.k=1.d=1.t=u.l=.u=13da31b4-c6bb-4561-8fed-07e728fa6cc5.r=f844b420"}) . decodeCookie <$> dologin
  goodCookie <- decodeCookie <$> dologin
  badCookie2 <- (\c -> c {cookie_value = "SKsjKQbiqxuEugGMWVbq02fNEA7QFdNmTiSa1Y0YMgaEP5tWl3nYHWlIrM5F8Tt7Cfn2Of738C7oeiY8xzPHAC==.v=1.k=1.d=1.t=u.l=.u=13da31b4-c6bb-4561-8fed-07e728fa6cc5.r=f844b420"}) . decodeCookie <$> dologin

  -- Basic sanity checks
  post (unversioned . n . path "/access" . cookie goodCookie) !!! const 200 === statusCode
  post (unversioned . n . path "/access" . cookie badCookie1) !!! const 403 === statusCode
  post (unversioned . n . path "/access" . cookie badCookie2) !!! const 403 === statusCode

  -- Sending both cookies should always work, regardless of the order (they are ordered by time)
  post (unversioned . n . path "/access" . cookie badCookie1 . cookie goodCookie . cookie badCookie2) !!! const 200 === statusCode
  post (unversioned . n . path "/access" . cookie goodCookie . cookie badCookie1 . cookie badCookie2) !!! const 200 === statusCode
  post (unversioned . n . path "/access" . cookie badCookie1 . cookie badCookie2 . cookie goodCookie) !!! const 200 === statusCode -- -- Sending a bad cookie and an unparseble one should work too
  post (unversioned . n . path "/access" . cookie unparseableCookie . cookie goodCookie) !!! const 200 === statusCode
  post (unversioned . n . path "/access" . cookie goodCookie . cookie unparseableCookie) !!! const 200 === statusCode

  -- We want to make sure we are using a cookie that was deleted from the DB but not expired - this way the client
  -- will still have it in the cookie jar because it did not get overriden
  (deleted, valid) <- getAndTestDBSupersededCookieAndItsValidSuccessor o b n
  now <- liftIO getCurrentTime
  liftIO $ assertBool "cookie should not be expired" (cookie_expiry_time deleted > now)
  liftIO $ assertBool "cookie should not be expired" (cookie_expiry_time valid > now)
  post (unversioned . n . path "/access" . cookie deleted) !!! const 403 === statusCode
  post (unversioned . n . path "/access" . cookie valid) !!! const 200 === statusCode
  post (unversioned . n . path "/access" . cookie deleted . cookie valid) !!! const 200 === statusCode
  post (unversioned . n . path "/access" . cookie valid . cookie deleted) !!! const 200 === statusCode

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
  let loc = localPart email
      dom = domainPart email
      email' = unsafeEmailAddress (encodeUtf8 . Text.toUpper . decodeUtf8 $ loc) dom
  login brig (defEmailLogin email') PersistentCookie
    !!! const 200 === statusCode

testHandleLogin :: Brig -> Http ()
testHandleLogin brig = do
  usr <- Public.userId <$> randomUser brig
  hdl <- randomHandle
  let update = RequestBodyLBS . encode $ HandleUpdate hdl
  put (brig . path "/self/handle" . contentJson . zUser usr . zConn "c" . Http.body update)
    !!! const 200 === statusCode
  let l = MkLogin (LoginByHandle (fromJust $ parseHandle hdl)) defPassword Nothing Nothing
  login brig l PersistentCookie !!! const 200 === statusCode

-- | Check that local part after @+@ is ignored by equality on email addresses if the domain is
-- untrusted.
testLoginUntrustedDomain :: Brig -> Http ()
testLoginUntrustedDomain brig = do
  Just email <- userEmail <$> createUserUntrustedEmail "Homer" brig
  let loc = decodeUtf8 $ localPart email
      dom = domainPart email
  -- login without "+" suffix
  let email' = unsafeEmailAddress (encodeUtf8 $ Text.takeWhile (/= '+') loc) dom
  login brig (defEmailLogin email') PersistentCookie
    !!! const 200 === statusCode

-- The testLoginFailure test conforms to the following testing standards:
-- @SF.Provisioning @TSFI.RESTfulAPI @S2
--
-- Test that trying to log in with a wrong password or non-existent email fails.
testLoginFailure :: Brig -> Http ()
testLoginFailure brig = do
  Just email <- userEmail <$> randomUser brig
  -- login with wrong password
  let badpw = plainTextPassword6Unsafe "wrongpassword"
  login
    brig
    (MkLogin (LoginByEmail email) badpw Nothing Nothing)
    PersistentCookie
    !!! const 403 === statusCode
  -- login with wrong / non-existent email
  let badmail = unsafeEmailAddress "wrong" "wire.com"
  login
    brig
    ( MkLogin (LoginByEmail badmail) defPassword Nothing Nothing
    )
    PersistentCookie
    !!! const 403 === statusCode

-- @END

testThrottleLogins :: Opts.Opts -> Brig -> Http ()
testThrottleLogins conf b = do
  -- Get the maximum amount of times we are allowed to login before
  -- throttling begins
  let l = Opts.userCookieLimit (Opts.settings conf)
  u <- randomUser b
  let Just e = userEmail u
  -- Login exactly that amount of times, as fast as possible
  pooledForConcurrentlyN_ 8 [1 .. l] $ \_ ->
    login b (defEmailLogin e) SessionCookie
  -- Login once more. This should fail!  The `recoverAll` is because sometimes it doesn't,
  -- Even though that may have been due to the config line `setUserCookieThrottle.retryAfter: 1`.
  -- `3` should be more robust.
  x <- recoverAll (exponentialBackoff 8000 <> limitRetries 3) . const $ do
    login b (defEmailLogin e) SessionCookie
      <!! const 429 === statusCode
  -- After the amount of time specified in "Retry-After", though,
  -- throttling should stop and login should work again
  let Just n = fromByteString =<< getHeader "Retry-After" x
  liftIO $ do
    assertBool "throttle delay" (n > 0)
    threadDelay (1000000 * (n + 1))
  login b (defEmailLogin e) SessionCookie !!! const 200 === statusCode

-- The testLimitRetries test conforms to the following testing standards:
-- @SF.Channel @TSFI.RESTfulAPI @TSFI.NTP @S2
--
-- The following test tests the login retries. It checks that a user can make
-- only a prespecified number of attempts to log in with an invalid password,
-- after which the user is unable to try again for a configured amount of time.
-- After the configured amount of time has passed, the test asserts the user can
-- successfully log in again. Furthermore, the test asserts that another
-- unrelated user can successfully log-in in parallel to the failed attempts of
-- the aforementioned user.
testLimitRetries :: (HasCallStack) => Opts.Opts -> Brig -> Http ()
testLimitRetries conf brig = do
  let Just opts = conf.settings.limitFailedLogins
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
        retryTimeout = Timeout $ fromIntegral retryAfterSecs
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

-- @END

-------------------------------------------------------------------------------
-- LegalHold Login

testRegularUserLegalHoldLogin :: Brig -> Http ()
testRegularUserLegalHoldLogin brig = do
  -- Create a regular user
  uid <- Public.userId <$> randomUser brig
  -- fail if user is not a team user
  legalHoldLogin brig (LegalHoldLogin uid (Just defPassword) Nothing) PersistentCookie !!! do
    const 403 === statusCode

testTeamUserLegalHoldLogin :: (HasCallStack) => Brig -> Galley -> Http ()
testTeamUserLegalHoldLogin brig galley = do
  -- create team user Alice
  (alice, tid) <- createUserWithTeam brig
  now <- liftIO getCurrentTime
  -- fail if legalhold isn't activated yet for this user
  legalHoldLogin brig (LegalHoldLogin alice (Just defPassword) Nothing) PersistentCookie !!! do
    const 403 === statusCode
  putLHWhitelistTeam galley tid !!! const 200 === statusCode
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
  legalHoldLogin brig (LegalHoldLogin alice (plainTextPassword6 "wrong-password") Nothing) PersistentCookie !!! do
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
  post (unversioned . brig . path "/access" . cookie c)
    !!! const 200 === statusCode
  post (unversioned . brig . path "/access/logout" . cookie c . queryItem "access_token" (toByteString' t))
    !!! const 200 === statusCode
  post (unversioned . brig . path "/access" . cookie c)
    !!! const 403 === statusCode

-------------------------------------------------------------------------------
-- Sso login

-- | Check that login works with @/sso-login@ even without having the
-- right password.
testEmailSsoLogin :: Brig -> Http ()
testEmailSsoLogin brig = do
  -- Create a user
  uid <- Public.userId <$> randomUser brig
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
  uid <- Public.userId <$> randomUser brig
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

-- The testInvalidCookie test conforms to the following testing standards:
-- @SF.Provisioning @TSFI.RESTfulAPI @TSFI.NTP @S2
--
-- Test that invalid and expired tokens do not work.
testInvalidCookie :: forall u. (ZAuth.UserTokenLike u) => ZAuth.Env -> Brig -> Http ()
testInvalidCookie z b = do
  -- Syntactically invalid
  post (unversioned . b . path "/access" . cookieRaw "zuid" "xxx") !!! do
    const 403 === statusCode
    const (Just "Invalid user token") =~= responseBody
  -- Expired
  user <- Public.userId <$> randomUser b
  let f = set (ZAuth.userTTL (Proxy @u)) 0
  t <- toByteString' <$> runZAuth z (ZAuth.localSettings f (ZAuth.newUserToken @u user Nothing))
  liftIO $ threadDelay 1000000
  post (unversioned . b . path "/access" . cookieRaw "zuid" t) !!! do
    const 403 === statusCode
    const (Just "expired") =~= responseBody

-- @END

testInvalidToken :: ZAuth.Env -> Brig -> Http ()
testInvalidToken z b = do
  user <- Public.userId <$> randomUser b
  t <- toByteString' <$> runZAuth z (ZAuth.newUserToken @ZAuth.User user Nothing)

  -- Syntactically invalid
  post (unversioned . b . path "/access" . queryItem "access_token" "xxx" . cookieRaw "zuid" t)
    !!! errResponse
  post (unversioned . b . path "/access" . header "Authorization" "Bearer xxx" . cookieRaw "zuid" t)
    !!! errResponse
  where
    errResponse = do
      const 403 === statusCode
      const (Just "Invalid access token") =~= responseBody

testMissingCookie :: forall u a. (ZAuth.TokenPair u a) => ZAuth.Env -> Brig -> Http ()
testMissingCookie z b = do
  -- Missing cookie, i.e. token refresh mandates a cookie.
  post (unversioned . b . path "/access")
    !!! errResponse
  t <- toByteString' <$> runZAuth z (randomAccessToken @u @a)
  post (unversioned . b . path "/access" . header "Authorization" ("Bearer " <> t))
    !!! errResponse
  post (unversioned . b . path "/access" . queryItem "access_token" t)
    !!! errResponse
  where
    errResponse = do
      const 403 === statusCode
      const (Just "Missing cookie") =~= responseBody
      const (Just "invalid-credentials") =~= responseBody

testUnknownCookie :: forall u. (ZAuth.UserTokenLike u) => ZAuth.Env -> Brig -> Http ()
testUnknownCookie z b = do
  -- Valid cookie but unknown to the server.
  t <- toByteString' <$> runZAuth z (randomUserToken @u)
  post (unversioned . b . path "/access" . cookieRaw "zuid" t) !!! do
    const 403 === statusCode
    const (Just "invalid-credentials") =~= responseBody

testTokenMismatchLegalhold :: ZAuth.Env -> Brig -> Galley -> Http ()
testTokenMismatchLegalhold z brig galley = do
  u <- randomUser brig
  let Just email = userEmail u
  _rs <-
    login brig (emailLogin email defPassword (Just "nexus1")) PersistentCookie
      <!! const 200 === statusCode
  -- try refresh with a regular UserCookie but a LegalHoldAccessToken
  let c = decodeCookie _rs
  t <- toByteString' <$> runZAuth z (randomAccessToken @ZAuth.LegalHoldUser @ZAuth.LegalHoldAccess)
  post (unversioned . brig . path "/access" . cookie c . header "Authorization" ("Bearer " <> t)) !!! do
    const 403 === statusCode
    const (Just "Token mismatch") =~= responseBody
  -- try refresh with a regular AccessToken but a LegalHoldUserCookie
  (alice, tid) <- createUserWithTeam brig
  putLHWhitelistTeam galley tid !!! const 200 === statusCode
  _rs <- legalHoldLogin brig (LegalHoldLogin alice (Just defPassword) Nothing) PersistentCookie
  let c' = decodeCookie _rs
  t' <- toByteString' <$> runZAuth z (randomAccessToken @ZAuth.User @ZAuth.Access)
  post (unversioned . brig . path "/access" . cookie c' . header "Authorization" ("Bearer " <> t')) !!! do
    const 403 === statusCode
    const (Just "Token mismatch") =~= responseBody

-- | This only tests access; the logic is tested in 'testEmailUpdate' in `Account.hs`.
-- this test duplicates some of 'initiateEmailUpdateLogin' intentionally.
testAccessSelfEmailAllowed :: Nginz -> Brig -> Bool -> Http ()
testAccessSelfEmailAllowed nginz brig withCookie = do
  usr <- randomUser brig
  let Just email = userEmail usr
  (mbCky, tok) <- do
    rsp <-
      login nginz (emailLogin email defPassword (Just "nexus1")) PersistentCookie
        <!! const 200 === statusCode
    pure
      ( if withCookie then Just (decodeCookie rsp) else Nothing,
        decodeToken rsp
      )
  let req =
        unversioned
          . nginz
          . path "/access/self/email"
          . maybe id cookie mbCky
          . header "Authorization" ("Bearer " <> toByteString' tok)

  put (req . Bilge.json ())
    !!! const 400 === statusCode

  put (req . Bilge.json (EmailUpdate email))
    !!! const (if withCookie then 204 else 403) === statusCode

-- this test duplicates some of 'initiateEmailUpdateLogin' intentionally.
testAccessSelfEmailDenied :: ZAuth.Env -> Nginz -> Brig -> Bool -> Http ()
testAccessSelfEmailDenied zenv nginz brig withCookie = do
  usr <- randomUser brig
  let Just email = userEmail usr
  mbCky <-
    if withCookie
      then do
        rsp <-
          login nginz (emailLogin email defPassword (Just "nexus1")) PersistentCookie
            <!! const 200 === statusCode
        pure
          (if withCookie then Just (decodeCookie rsp) else Nothing)
      else do
        pure Nothing
  tok <- runZAuth zenv (randomAccessToken @ZAuth.User @ZAuth.Access)
  let req =
        unversioned
          . nginz
          . path "/access/self/email"
          . Bilge.json (EmailUpdate email)
          . maybe id cookie mbCky

  put req
    !!! errResponse "invalid-credentials" "Missing access token"
  put (req . header "Authorization" "xxx")
    !!! errResponse "invalid-credentials" "Invalid authorization scheme"
  put (req . header "Authorization" "Bearer xxx")
    !!! errResponse "client-error" "Failed reading: Invalid access token"
  put (req . header "Authorization" ("Bearer " <> toByteString' tok))
    !!! errResponse "invalid-credentials" "Invalid token"
  where
    errResponse label msg = do
      const 403 === statusCode
      when withCookie $ do
        const (Just label) =~= responseBody
        const (Just msg) =~= responseBody

-- | We are a little bit nasty on this test. For most cases, one can use brig and nginz interchangeably.
--   In this case, the issue relates to the usage of `getAndTestDBSupersededCookieAndItsValidSuccessor`.
--   That function can be refactored though to make this more clear
testNewPersistentCookie :: Opts.Opts -> Brig -> Http ()
testNewPersistentCookie config b =
  void $ getAndTestDBSupersededCookieAndItsValidSuccessor config b b

getAndTestDBSupersededCookieAndItsValidSuccessor :: Opts.Opts -> Brig -> Nginz -> Http (Http.Cookie, Http.Cookie)
getAndTestDBSupersededCookieAndItsValidSuccessor config b n = do
  u <- randomUser b
  let renewAge = config.settings.userCookieRenewAge
  let minAge = fromIntegral $ (renewAge + 1) * 1000000
      Just email = userEmail u
  _rs <-
    login n (emailLogin email defPassword (Just "nexus1")) PersistentCookie
      <!! const 200 === statusCode
  let c = decodeCookie _rs
  -- Wait for the cookie to be eligible for renewal
  liftIO $ threadDelay minAge
  -- Refresh tokens
  _rs <-
    post (unversioned . n . path "/access" . cookie c) <!! do
      const 200 === statusCode
      const Nothing =/= getHeader "Set-Cookie"
      const (Just "access_token") =~= responseBody
  let c' = decodeCookie _rs
  liftIO $ assertBool "expiry" (cookie_expiry_time c' > cookie_expiry_time c)
  -- Refresh with the old cookie should still work for the
  -- duration of another BRIG_COOKIE_RENEW_AGE seconds,
  -- but the response should keep advertising the new cookie.
  _rs <-
    post (unversioned . n . path "/access" . cookie c) <!! do
      const 200 === statusCode
      const Nothing =/= getHeader "Set-Cookie"
      const (Just "access_token") =~= responseBody
  -- we got a new cookie value, but the key is the same
  liftIO $ assertBool "cookie" (c' `equivCookie` decodeCookie _rs)
  -- Refresh with the new cookie should succeed
  -- (without advertising yet another new cookie).
  post (unversioned . n . path "/access" . cookie c') !!! do
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
  -- Return non-expired cookie but removed from DB (because it was renewed)
  -- and a valid cookie
  pure (c, c')

testAccessWithClientId :: Brig -> Http ()
testAccessWithClientId brig = do
  u <- randomUser brig
  rs <-
    login
      brig
      ( emailLogin
          (fromJust (userEmail u))
          defPassword
          (Just "nexus1")
      )
      PersistentCookie
      <!! const 200 === statusCode
  let c = decodeCookie rs
  cl <-
    responseJsonError
      =<< addClient
        brig
        (userId u)
        (defNewClient PermanentClientType [] (Imports.head someLastPrekeys))
        <!! const 201 === statusCode
  r <-
    post
      ( unversioned
          . brig
          . path "/access"
          . queryItem "client_id" (toByteString' (clientId cl))
          . cookie c
      )
      <!! const 200 === statusCode
  now <- liftIO getCurrentTime
  liftIO $ do
    let ck = decodeCookie r
        Just token = fromByteString (cookie_value ck)
        atoken = decodeToken' @ZAuth.Access r
    assertSanePersistentCookie @ZAuth.User ck
    ZAuth.userTokenClient @ZAuth.User token @?= Just (clientId cl)
    assertSaneAccessToken now (userId u) (decodeToken' @ZAuth.Access r)
    ZAuth.accessTokenClient @ZAuth.Access atoken @?= Just (clientId cl)

-- here a fresh client gets a token without client_id first, then allocates a
-- new client ID and finally calls access again with the new client_id
testAccessWithClientIdAndOldToken :: Brig -> Http ()
testAccessWithClientIdAndOldToken brig = do
  u <- randomUser brig
  rs <-
    login
      brig
      ( emailLogin
          (fromJust (userEmail u))
          defPassword
          (Just "nexus1")
      )
      PersistentCookie
      <!! const 200 === statusCode
  let c = decodeCookie rs
  token0 <-
    fmap (decodeToken' @ZAuth.Access) $
      post
        ( unversioned
            . brig
            . path "/access"
            . cookie c
        )
        <!! const 200 === statusCode
  cl <-
    responseJsonError
      =<< addClient
        brig
        (userId u)
        (defNewClient PermanentClientType [] (Imports.head someLastPrekeys))
        <!! const 201 === statusCode
  r <-
    post
      ( unversioned
          . brig
          . path "/access"
          . queryItem "client_id" (toByteString' (clientId cl))
          . header "Authorization" ("Bearer " <> toByteString' token0)
          . cookie c
      )
      <!! const 200 === statusCode
  now <- liftIO getCurrentTime
  liftIO $ do
    let ck = decodeCookie r
        Just token = fromByteString (cookie_value ck)
        atoken = decodeToken' @ZAuth.Access r
    assertSanePersistentCookie @ZAuth.User ck
    ZAuth.userTokenClient @ZAuth.User token @?= Just (clientId cl)
    assertSaneAccessToken now (userId u) atoken
    ZAuth.accessTokenClient @ZAuth.Access atoken @?= Just (clientId cl)

testAccessWithIncorrectClientId :: Brig -> Http ()
testAccessWithIncorrectClientId brig = do
  u <- randomUser brig
  rs <-
    login
      brig
      ( emailLogin
          (fromJust (userEmail u))
          defPassword
          (Just "nexus1")
      )
      PersistentCookie
      <!! const 200 === statusCode
  let c = decodeCookie rs
  addClient
    brig
    (userId u)
    (defNewClient PermanentClientType [] (Imports.head someLastPrekeys))
    !!! const 201 === statusCode
  post
    ( unversioned
        . brig
        . path "/access"
        . queryItem "client_id" "beef"
        . cookie c
    )
    !!! const 403 === statusCode

testAccessWithExistingClientId :: Brig -> Http ()
testAccessWithExistingClientId brig = do
  u <- randomUser brig
  rs <-
    login
      brig
      ( emailLogin
          (fromJust (userEmail u))
          defPassword
          (Just "nexus1")
      )
      PersistentCookie
      <!! const 200 === statusCode
  let c0 = decodeCookie rs
  cl <-
    responseJsonError
      =<< addClient
        brig
        (userId u)
        (defNewClient PermanentClientType [] (Imports.head someLastPrekeys))
        <!! const 201 === statusCode
  now <- liftIO getCurrentTime

  -- access with client ID first
  c1 <- do
    r <-
      post
        ( unversioned
            . brig
            . path "/access"
            . queryItem "client_id" (toByteString' (clientId cl))
            . cookie c0
        )
        <!! const 200 === statusCode
    pure (decodeCookie r)

  -- now access again with no client ID
  c2 <- do
    r <-
      post
        ( unversioned
            . brig
            . path "/access"
            . cookie c1
        )
        <!! const 200 === statusCode
    liftIO $ do
      let ck = decodeCookie r
          Just token = fromByteString (cookie_value ck)
          atoken = decodeToken' @ZAuth.Access r
      assertSanePersistentCookie @ZAuth.User ck
      ZAuth.userTokenClient @ZAuth.User token @?= Just (clientId cl)
      assertSaneAccessToken now (userId u) (decodeToken' @ZAuth.Access r)
      ZAuth.accessTokenClient @ZAuth.Access atoken @?= Just (clientId cl)
    pure (decodeCookie r)

  -- now access with a different client ID
  do
    cl2 <-
      responseJsonError
        =<< addClient
          brig
          (userId u)
          (defNewClient PermanentClientType [] (someLastPrekeys !! 1))
          <!! const 201 === statusCode
    post
      ( unversioned
          . brig
          . path "/access"
          . queryItem "client_id" (toByteString' (clientId cl2))
          . cookie c2
      )
      !!! const 403 === statusCode

testNewSessionCookie :: Opts.Opts -> Brig -> Http ()
testNewSessionCookie config b = do
  u <- randomUser b
  let renewAge = config.settings.userCookieRenewAge
  let minAge = fromIntegral $ renewAge * 1000000 + 1
      Just email = userEmail u
  _rs <-
    login b (emailLogin email defPassword (Just "nexus1")) SessionCookie
      <!! const 200 === statusCode
  let c = decodeCookie _rs
  liftIO $ threadDelay minAge
  -- Session cookies are never renewed
  post (unversioned . b . path "/access" . cookie c) !!! do
    const 200 === statusCode
    const Nothing === getHeader "Set-Cookie"

testSuspendInactiveUsers :: (HasCallStack) => Opts.Opts -> Brig -> CookieType -> String -> Http ()
testSuspendInactiveUsers config brig cookieType endPoint = do
  -- (context information: cookies are stored by user, not by device; so if there is a
  -- cookie that is old, it means none of the devices of the user has used it for a request.)

  let Just suspendAge = Opts.suspendTimeout <$> config.settings.suspendInactiveUsers
  unless (suspendAge <= 30) $
    error "`suspendCookiesOlderThanSecs` is the number of seconds this test is running.  Please pick a value < 30."

  user <- randomUser brig
  let Just email = userEmail user
  rs <-
    login brig (emailLogin email defPassword Nothing) cookieType
      <!! const 200 === statusCode
  let cky = decodeCookie rs
  -- wait slightly longer than required for being marked as inactive.
  let waitTime :: Int = floor (timeoutDiff suspendAge) + 5 -- adding 1 *should* be enough, but it's not.
  liftIO $ threadDelay (1000000 * waitTime)
  case endPoint of
    "/access" -> do
      post (unversioned . brig . path "/access" . cookie cky) !!! do
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
    ( b
        . path "/cookies/remove"
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

-- The testTooManyCookies test conforms to the following testing standards:
-- @SF.Provisioning @TSFI.RESTfulAPI @S2
--
-- The test asserts that there is an upper limit for the number of user cookies
-- per cookie type. It does that by concurrently attempting to create more
-- persistent and session cookies than the configured maximum.
-- Creation of new cookies beyond the limit causes deletion of the
-- oldest cookies.
testTooManyCookies :: Opts.Opts -> Brig -> Http ()
testTooManyCookies config b = do
  u <- randomUser b
  let l = config.settings.userCookieLimit
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
        200 -> pure $ decodeCookie x
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

-- @END

testLogout :: Brig -> Http ()
testLogout b = do
  Just email <- userEmail <$> randomUser b
  _rs <- login b (defEmailLogin email) SessionCookie
  let (t, c) = (decodeToken _rs, decodeCookie _rs)
  post (unversioned . b . path "/access" . cookie c)
    !!! const 200 === statusCode
  post (unversioned . b . path "/access/logout" . cookie c . queryItem "access_token" (toByteString' t))
    !!! const 200 === statusCode
  post (unversioned . b . path "/access" . cookie c)
    !!! const 403 === statusCode

testReauthentication :: Brig -> Http ()
testReauthentication b = do
  u <- Public.userId <$> randomUser b
  let js = Http.body . RequestBodyLBS . encode $ object ["foo" .= ("bar" :: Text)]
  get (b . paths ["/i/users", toByteString' u, "reauthenticate"] . contentJson . js) !!! do
    const 403 === statusCode
  -- it's ok to not give a password in the request body, but if the user has a password set,
  -- response will be `forbidden`.

  get (b . paths ["/i/users", toByteString' u, "reauthenticate"] . contentJson . payload (plainTextPassword6 "123456")) !!! do
    const 403 === statusCode
    const (Just "invalid-credentials") === errorLabel
  get (b . paths ["/i/users", toByteString' u, "reauthenticate"] . contentJson . payload (Just defPassword)) !!! do
    const 200 === statusCode
  setStatus b u Suspended
  get (b . paths ["/i/users", toByteString' u, "reauthenticate"] . contentJson . payload (Just defPassword)) !!! do
    const 403 === statusCode
    const (Just "suspended") === errorLabel
  where
    payload pw = Http.body $ RequestBodyLBS $ encode $ ReAuthUser pw Nothing Nothing

-----------------------------------------------------------------------------
-- Helpers

prepareLegalHoldUser :: Brig -> Galley -> Http UserId
prepareLegalHoldUser brig galley = do
  (uid, tid) <- createUserWithTeam brig
  -- enable it for this team - without that, legalhold login will fail.
  putLHWhitelistTeam galley tid !!! const 200 === statusCode
  pure uid

getCookieId :: forall u. (HasCallStack, ZAuth.UserTokenLike u) => Http.Cookie -> CookieId
getCookieId c =
  maybe
    (error "no cookie value")
    (CookieId . ZAuth.userTokenRand @u)
    (fromByteString (cookie_value c))

listCookies :: (HasCallStack) => Brig -> UserId -> Http [Auth.Cookie ()]
listCookies b u = listCookiesWithLabel b u []

listCookiesWithLabel :: (HasCallStack) => Brig -> UserId -> [CookieLabel] -> Http [Auth.Cookie ()]
listCookiesWithLabel b u l = do
  rs <-
    get
      ( b
          . path "/cookies"
          . queryItem "labels" labels
          . header "Z-User" (toByteString' u)
      )
      <!! const 200 === statusCode
  let Just cs = cookieList <$> responseJsonMaybe rs
  pure cs
  where
    labels = BS.intercalate "," $ map toByteString' l

-- | Check that the cookie returned after login is sane.
--
-- Doesn't check everything, just some basic properties.
assertSanePersistentCookie :: forall u. (ZAuth.UserTokenLike u) => Http.Cookie -> Assertion
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
  (ZAuth.AccessTokenLike a) =>
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

remJson :: PlainTextPassword6 -> Maybe [CookieLabel] -> Maybe [CookieId] -> Value
remJson p l ids =
  object
    [ "password" .= p,
      "labels" .= l,
      "ids" .= ids
    ]

wait :: (MonadIO m) => m ()
wait = liftIO $ threadDelay 1000000
