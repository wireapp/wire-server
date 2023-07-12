module Test.Client where

import API.Brig
import API.Gundeck
import Data.Aeson
import Data.Time.Clock.POSIX
import Data.Time.Clock.System
import Data.Time.Format
import SetupHelpers
import Testlib.Prelude

testClientLastActive :: HasCallStack => App ()
testClientLastActive = do
  alice <- randomUser OwnDomain def
  c0 <- addClient alice def >>= getJSON 201
  cid <- c0 %. "id"

  -- newly created clients should not have a last_active value
  tm0 <- fromMaybe Null <$> lookupField c0 "last_active"
  tm0 `shouldMatch` Null

  now <- systemSeconds <$> liftIO getSystemTime

  -- fetching notifications updates last_active
  void $ getNotifications alice cid def

  c1 <- getClient alice cid >>= getJSON 200
  tm1 <- c1 %. "last_active" & asString
  ts1 <-
    round @Double
      . realToFrac
      . utcTimeToPOSIXSeconds
      <$> parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" tm1
  assertBool "last_active is earlier than expected" $ ts1 >= now
