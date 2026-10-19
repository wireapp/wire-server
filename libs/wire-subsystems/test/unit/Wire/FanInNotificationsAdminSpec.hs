module Wire.FanInNotificationsAdminSpec (spec) where

import Data.Text qualified as T
import Imports
import Test.Hspec
import Wire.FanInNotificationsAdmin
import Wire.FanInNotificationsAdmin.Postgres

spec :: Spec
spec = do
  dbNameSpec
  sqlSpec

dbNameSpec :: Spec
dbNameSpec = describe "DbName" $ do
  it "accepts valid names" $ do
    for_ ["fan_in_perf", "_x1", "backendA", T.replicate 63 "a"] $ \n ->
      mkDbName n `shouldSatisfy` isRight

  it "quotes identifiers" $
    (quoteIdent <$> mkDbName "fan_in_perf") `shouldBe` Right "\"fan_in_perf\""

  it "rejects invalid names" $ do
    for_ ["", "a b", "a;drop", "a\"b", "a'b", "1abc", "a-b", T.replicate 64 "a"] $ \n ->
      mkDbName n `shouldBe` Left "invalid database name"

sqlSpec :: Spec
sqlSpec = describe "SQL rendering" $ do
  let n = either (error . T.unpack) id (mkDbName "fan_in_perf")

  it "renders CREATE DATABASE" $
    createDatabaseSql n `shouldBe` "CREATE DATABASE \"fan_in_perf\""

  it "renders DROP DATABASE" $
    dropDatabaseSql n `shouldBe` "DROP DATABASE IF EXISTS \"fan_in_perf\" WITH (FORCE)"

  it "renders partman setup" $ do
    let stmts = setupPartmanSql n
    stmts `shouldContain` ["ALTER SYSTEM SET pg_partman_bgw.dbname = 'fan_in_perf'"]
    stmts `shouldContain` ["ALTER SYSTEM SET pg_partman_bgw.interval = 60"]
    stmts `shouldContain` ["ALTER SYSTEM SET pg_partman_bgw.role = 'partman_maintainer'"]
    listToMaybe (reverse stmts) `shouldBe` Just "SELECT pg_reload_conf()"
