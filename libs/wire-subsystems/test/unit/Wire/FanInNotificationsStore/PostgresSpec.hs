module Wire.FanInNotificationsStore.PostgresSpec (spec) where

import Data.Id
import Data.Time.Clock.POSIX (getPOSIXTime)
import Imports
import Test.Hspec
import Wire.FanInNotificationsStore.Postgres (genNotificationId, notificationIdMillis)

spec :: Spec
spec = do
  describe "genNotificationId" $
    it "generates strictly increasing ids within the process" $ do
      uuids <- replicateM 10000 ((.toUUID) <$> genNotificationId @())
      and (zipWith (<) uuids (drop 1 uuids)) `shouldBe` True
  describe "notificationIdMillis" $
    it "returns the generation time in milliseconds" $ do
      t0 <- nowMillis
      u <- (.toUUID) <$> genNotificationId @()
      t1 <- nowMillis
      let t = notificationIdMillis u
      -- one ms of slack: the id generator may round its clock reading up
      (t >= t0 && t <= t1 + 1) `shouldBe` True
  where
    nowMillis :: IO Word64
    nowMillis = floor . (* 1000) <$> getPOSIXTime
