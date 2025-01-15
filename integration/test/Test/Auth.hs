module Test.Auth where

import API.Brig
import API.Common
import qualified Data.ByteString.Char8 as BSChar8
import SetupHelpers
import Testlib.Prelude
import Text.Read
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
testLimitRetries :: App ()
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
