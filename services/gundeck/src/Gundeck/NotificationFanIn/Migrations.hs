{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fforce-recomp #-}

module Gundeck.NotificationFanIn.Migrations (runMigrations) where

import Control.Exception
import Data.FileEmbed
import Data.Text.Encoding qualified as Text
import Hasql.Migration
import Hasql.Pool (Pool, UsageError, use)
import Hasql.Statement (Statement)
import Hasql.TH (resultlessStatement)
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions
import Imports
import System.Logger (Logger)
import System.Logger qualified as Log

fanInMigrations :: [MigrationCommand]
fanInMigrations =
  map
    (\(name, content) -> MigrationScript name (Text.decodeUtf8 content))
    $(makeRelativeToProject "postgres-migrations" >>= embedDir)

fanInMigrationLock :: Statement () ()
fanInMigrationLock =
  -- The first key is the ASCII "WIRE" namespace; the second identifies fan-in migrations.
  [resultlessStatement|SELECT (1 :: integer) FROM (SELECT pg_advisory_xact_lock(1463899717, 1)) AS t|]

data FanInMigrationError
  = FanInMigrationError MigrationError
  | FanInMigrationUsageError UsageError
  deriving (Show)

instance Exception FanInMigrationError

runMigrations :: Pool -> Logger -> IO ()
runMigrations pool logger = do
  let session = do
        Log.info logger $ Log.msg (Log.val "Running notification fan-in migrations")
        forM_ (MigrationInitialization : fanInMigrations) $ \migration -> do
          result <- transaction ReadCommitted Write $ do
            Tx.statement () fanInMigrationLock
            runMigration migration
          for_ result $ throw . FanInMigrationError
        Log.info logger $ Log.msg (Log.val "Notification fan-in migrations completed")
  either (throwIO . FanInMigrationUsageError) pure =<< use pool session
