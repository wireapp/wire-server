module Wire.FanInNotificationsAdminSpec (spec) where

import Data.Text qualified as T
import Imports
import Test.Hspec
import Wire.FanInNotificationsAdmin

spec :: Spec
spec = describe "DbName" $ do
  it "accepts valid names" $ do
    for_ ["fan_in_perf", "_x1", "backendA", T.replicate 63 "a"] $ \n ->
      mkDbName n `shouldSatisfy` isRight

  it "quotes identifiers" $
    (quoteIdent <$> mkDbName "fan_in_perf") `shouldBe` Right "\"fan_in_perf\""

  it "rejects invalid names" $ do
    for_ ["", "a b", "a;drop", "a\"b", "a'b", "1abc", "a-b", T.replicate 64 "a"] $ \n ->
      mkDbName n `shouldBe` Left "invalid database name"
