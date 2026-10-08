module Wire.FanInNotificationsStore.PostgresSpec (spec) where

import Data.Bits (shiftR, (.&.))
import Data.Id
import Data.UUID qualified as UUID
import Imports
import Test.Hspec
import Wire.FanInNotificationsStore.Postgres (genNotificationId)

spec :: Spec
spec = describe "genNotificationId" $ do
  -- REVIEW: This test is not required (Only shows UUID version is 7)
  it "generates version 7 UUIDs" $ do
    ids <- replicateM 100 (genNotificationId @())
    forM_ ids $ \i -> do
      let (_, w2, _, _) = UUID.toWords i.toUUID
      (w2 `shiftR` 12) .&. 0xF `shouldBe` 7

  it "generates strictly increasing ids within the process" $ do
    uuids <- replicateM 10000 ((.toUUID) <$> genNotificationId @())
    and (zipWith (<) uuids (drop 1 uuids)) `shouldBe` True
