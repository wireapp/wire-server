module FanInPerf.DatabaseSpec (spec) where

import FanInPerf.Database
import Imports
import PostgresqlConnectionString qualified as C
import Test.Hspec
import Wire.FanInNotificationsAdmin (unDbName)

parseConn :: Text -> C.ConnectionString
parseConn t = either (error . ("bad test connection string: " <>) . show) id (C.parse t)

spec :: Spec
spec = do
  describe "experimentDb" $ do
    let name = fmap unDbName . experimentDb . parseConn
    it "accepts valid database names" $ do
      name "postgresql://u:p@h/fan_in_perf" `shouldBe` Right "fan_in_perf"
      name "postgresql://u:p@h/my_test_db" `shouldBe` Right "my_test_db"
      name "postgresql://u:p@h/backendA" `shouldBe` Right "backendA"
    it "rejects a missing database name" $
      name "postgresql://u:p@h" `shouldSatisfy` isLeft
    it "rejects invalid identifiers" $
      name "postgresql://u:p@h/fan_in_perf%3Bx" `shouldSatisfy` isLeft

  describe "adminConnStr" $ do
    let c = parseConn "postgresql://u:p@h/fan_in_perf"
        a = adminConnStr c
    it "targets the postgres maintenance database" $
      C.toDbname a `shouldBe` Just "postgres"
    it "keeps user and hosts" $ do
      C.toUser a `shouldBe` C.toUser c
      C.toHosts a `shouldBe` C.toHosts c
