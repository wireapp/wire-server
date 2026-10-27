module FanInPerf.NotificationsAdminSpec (spec) where

import Data.Aeson (decode, toJSON)
import Data.Aeson qualified as A
import Data.Text qualified as T
import FanInPerf.NotificationsAdmin
import FanInPerf.NotificationsAdmin.Postgres
import Imports
import Test.Hspec

spec :: Spec
spec = do
  dbNameSpec
  sqlSpec
  serverInfoSpec

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

  it "renders drop of tables and pg_partman" $ do
    dropTablesSql `shouldSatisfy` T.isInfixOf "DROP TABLE IF EXISTS user_notifications"
    dropTablesSql `shouldSatisfy` T.isSuffixOf "DROP SCHEMA IF EXISTS partman CASCADE"

  it "renders partman setup" $ do
    let stmts = setupPartmanSql n
    stmts `shouldContain` ["ALTER SYSTEM SET pg_partman_bgw.dbname = 'fan_in_perf'"]
    stmts `shouldContain` ["ALTER SYSTEM SET pg_partman_bgw.interval = 60"]
    stmts `shouldContain` ["ALTER SYSTEM SET pg_partman_bgw.role = 'partman_maintainer'"]
    listToMaybe (reverse stmts) `shouldBe` Just "SELECT pg_reload_conf()"

serverInfoSpec :: Spec
serverInfoSpec = describe "ServerInfo as JSON" $ do
  it "has the version, the settings by name and the pg_partman version" $
    Just (toJSON (ServerInfo "PostgreSQL 17.11" [("max_wal_size", "1GB"), ("shared_buffers", "128MB")] (Just "5.5.0")))
      `shouldBe` decode "{\"version\":\"PostgreSQL 17.11\",\"settings\":{\"max_wal_size\":\"1GB\",\"shared_buffers\":\"128MB\"},\"partman_version\":\"5.5.0\"}"
  it "has null without the extension" $
    toJSON (ServerInfo "v" [] Nothing) `shouldBe` A.object ["version" A..= ("v" :: Text), "settings" A..= A.object [], "partman_version" A..= A.Null]
