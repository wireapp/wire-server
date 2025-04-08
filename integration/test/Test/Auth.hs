module Test.Auth where

import API.Brig
import API.BrigInternal
import API.Common
import API.GalleyInternal
import qualified API.Nginz as Nginz
import qualified Data.ByteString.Char8 as BSChar8
import SetupHelpers
import Testlib.Prelude
import Text.Read
import UnliftIO.Async
import UnliftIO.Concurrent

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
testLimitRetries :: (HasCallStack) => App ()
testLimitRetries = do
  let retryLimit = 5
      timeout = 5
  withModifiedBackend
    def
      { brigCfg =
          -- Set a small timeout to make this test fast
          setField @_ @Int "optSettings.setLimitFailedLogins.timeout" timeout
            >=> setField @_ @Int "optSettings.setLimitFailedLogins.retryLimit" retryLimit
            -- Disable password hashing rate limiting, so we can login many times without making this test slow
            >=> setField @_ @Int "optSettings.setPasswordHashingRateLimit.userLimit.inverseRate" 0
      }
    $ \domain -> do
      alice <- randomUser domain def
      aliceEmail <- asString $ alice %. "email"

      bob <- randomUser domain def
      bobEmail <- asString $ bob %. "email"

      -- Alice tries to login 5 times with wrong password
      forM_ [1 .. retryLimit] $ \_ ->
        login domain aliceEmail "wrong-password" `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 403
          resp.json %. "label" `shouldMatch` "invalid-credentials"

      -- Now alice cannot login even with correct password
      retryAfter <-
        login domain aliceEmail defPassword `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 403
          resp.json %. "label" `shouldMatch` "client-error"
          let Just retryAfter = readMaybe . BSChar8.unpack =<< lookup (fromString "Retry-After") resp.headers
          (retryAfter <= timeout) `shouldMatch` True
          pure retryAfter

      -- Bob can still login
      login domain bobEmail defPassword `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200

      -- Waiting 2s less than retryAfter should still cause a failure
      threadDelay ((retryAfter - 2) * 1_000_000)
      login domain aliceEmail defPassword `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 403
        resp.json %. "label" `shouldMatch` "client-error"
        let Just retryAfter2 = readMaybe . BSChar8.unpack =<< lookup (fromString "Retry-After") resp.headers
        -- This should be about 2 seconds or slightly less because we didn't
        -- wait long enough. This also asserts that the throttling doesn't get
        -- reset by making another call
        (retryAfter2 <= (2 :: Int)) `shouldMatch` True

      -- Waiting 2 more seconds should make the login succeed
      threadDelay 2_000_000
      login domain aliceEmail defPassword `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200

-- @END

-- The testTooManyCookies test conforms to the following testing standards:
-- @SF.Provisioning @TSFI.RESTfulAPI @S2
--
-- The test asserts that there is an upper limit for the number of user cookies
-- per cookie type. It does that by concurrently attempting to create more
-- persistent and session cookies than the configured maximum.
-- Creation of new cookies beyond the limit causes deletion of the
-- oldest cookies.
testTooManyCookies :: (HasCallStack) => App ()
testTooManyCookies = do
  let cookieLimit = 5
  withModifiedBackend
    def
      { brigCfg =
          -- Disable password hashing rate limiting, so we can login many times without making this test slow
          setField @_ @Int "optSettings.setPasswordHashingRateLimit.userLimit.inverseRate" 0
            -- Disable cookie throttling so this test is not slow
            >=> setField @_ @Int "optSettings.setUserCookieThrottle.retryAfter" 0
            >=> setField @_ @Int "optSettings.setUserCookieLimit" cookieLimit
      }
    $ \domain -> do
      alice <- randomUser domain def
      aliceEmail <- asString $ alice %. "email"

      let testCookieLimit label = do
            let loginFn = if label == "persistent" then login else loginWithSessionCookie
            (deletedCookie1 : deletedCookie2 : validCookies) <-
              replicateM (cookieLimit + 2)
                $ do
                  -- This threadDelay is required to get around problems caused
                  -- by: https://wearezeta.atlassian.net/browse/WPB-15446
                  threadDelay 1_000_000
                  loginFn domain aliceEmail defPassword
                    `bindResponse` \resp -> do
                      resp.status `shouldMatchInt` 200
                      pure . fromJust $ getCookie "zuid" resp
            addFailureContext ("deletedCookie1: " <> deletedCookie1 <> "\ndeletedCookie2: " <> deletedCookie2 <> "\nvalidCookies:\n" <> unlines validCookies) $ do
              forM_ [deletedCookie1, deletedCookie2] $ \deletedCookie -> do
                renewToken alice deletedCookie `bindResponse` \resp ->
                  resp.status `shouldMatchInt` 403
              forM_ validCookies $ \validCookie ->
                renewToken alice validCookie `bindResponse` \resp ->
                  resp.status `shouldMatchInt` 200
      concurrently_ (testCookieLimit "persistent") (testCookieLimit "session")

-- @END

-- The testInvalidCookie test conforms to the following testing standards:
-- @SF.Provisioning @TSFI.RESTfulAPI @TSFI.NTP @S2
--
-- Test that invalid and expired tokens do not work.
testInvalidCookie :: (HasCallStack) => App ()
testInvalidCookie = do
  let cookieTimeout = 2
  withModifiedBackend
    def
      { brigCfg =
          setField @_ @Int "zauth.authSettings.userTokenTimeout" cookieTimeout
            >=> setField @_ @Int "zauth.authSettings.legalHoldUserTokenTimeout" cookieTimeout
      }
    $ \domain -> do
      Nginz.access domain "zuid=xxx" `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 403
        resp.json %. "label" `shouldMatch` "client-error"
        msg <- asString $ resp.json %. "message"
        msg `shouldContain` "Invalid token"

      (owner, tid, [alice]) <- createTeam domain 2
      aliceEmail <- asString $ alice %. "email"
      aliceId <- asString $ alice %. "qualified_id.id"
      userCookie <-
        login domain aliceEmail defPassword `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          pure . fromJust $ getCookie "zuid" resp

      legalholdWhitelistTeam tid owner >>= assertSuccess

      lhCookie <-
        legalholdLogin domain aliceId defPassword `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          pure . fromJust $ getCookie "zuid" resp

      -- Wait for both cookies to expire
      threadDelay $ (cookieTimeout + 1) * 1_000_000
      -- Assert that the cookies are considered expired
      for_ [userCookie, lhCookie] $ \cookie ->
        Nginz.access domain ("zuid=" <> cookie) `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 403
          resp.json %. "label" `shouldMatch` "invalid-credentials"
          resp.json %. "message" `shouldMatch` "Token expired"

-- @END
