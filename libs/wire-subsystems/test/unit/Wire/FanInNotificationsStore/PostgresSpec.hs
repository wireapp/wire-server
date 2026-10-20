module Wire.FanInNotificationsStore.PostgresSpec (spec) where

import Data.Bits (shiftR, (.&.))
import Data.Id
import Data.UUID qualified as UUID
import Imports
import Test.Hspec
import Wire.FanInNotificationsStore.Postgres (genNotificationId)

spec :: Spec
spec = describe "genNotificationId" $ do
  it "generates strictly increasing ids within the process" $ do
    uuids <- replicateM 10000 ((.toUUID) <$> genNotificationId @())
    and (zipWith (<) uuids (drop 1 uuids)) `shouldBe` True
