{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fforce-recomp #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.PostgresMigrations where

import Control.Exception
import Data.FileEmbed
import Data.Hashable qualified as Hashable
import Data.Set qualified as Set
import Hasql.Migration
import Hasql.Pool
import Hasql.Session
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Hasql
import Hasql.TH (resultlessStatement)
import Hasql.Transaction.Sessions
import Imports
import System.Logger (Logger)
import System.Logger qualified as Log

allMigrations :: [MigrationCommand]
allMigrations = map (uncurry MigrationScript) $(makeRelativeToProject "postgres-migrations" >>= embedDir)

-- | Scripts which cannot be run in a transaction
nonTransactionMigrations :: Set ScriptName
nonTransactionMigrations = Set.fromList ["20260428072649-create-conv-parent-index.sql"]

data PostgresMigrationError = PostgresMigrationError MigrationError
  deriving (Show)

instance Exception PostgresMigrationError

runAllMigrations :: Pool -> Logger -> IO ()
runAllMigrations pool logger = do
  let session = do
        Log.info logger $ Log.msg (Log.val "Running migrations")
        forM_ (MigrationInitialization : allMigrations) $ \migrationCmd -> do
          mErr <-
            case migrationScriptName migrationCmd of
              (Just name)
                | name `Set.member` nonTransactionMigrations -> do
                    -- Locking the migration makes sure that only one process is
                    -- running this migration at a time. Without this `CREATE
                    -- INDEX CONCURRENRLY` can deadlock with other processes
                    -- causing a silent failure.
                    let lockId = fromIntegral $ Hashable.hash name
                    Session.statement lockId lockNonTransactionMigration
                    migRes <- runMigrationWithoutTransactions migrationCmd
                    Session.statement lockId unlockNonTransactionMigration
                    pure migRes
              _ ->
                transaction Serializable Write $ runMigration migrationCmd

          case mErr of
            Nothing -> pure ()
            Just err -> throw $ PostgresMigrationError err
        Log.info logger $ Log.msg (Log.val "Migrations completed successfully")

  either throwIO pure =<< use pool session
  where
    lockNonTransactionMigration :: Hasql.Statement Int64 ()
    lockNonTransactionMigration =
      [resultlessStatement|SELECT (1 :: integer) FROM (SELECT pg_advisory_lock($1 :: bigint))|]

    unlockNonTransactionMigration :: Hasql.Statement Int64 ()
    unlockNonTransactionMigration =
      [resultlessStatement|SELECT (1 :: integer) FROM (SELECT pg_advisory_unlock($1 :: bigint))|]

migrationName :: MigrationCommand -> (Log.Msg -> Log.Msg)
migrationName = \case
  MigrationInitialization -> Log.field "migration" ("Initialize Migration Schema" :: ByteString)
  MigrationScript name _ -> Log.field "migration" name
  MigrationValidation cmd -> Log.field "type" ("validation" :: ByteString) . migrationName cmd

migrationScriptName :: MigrationCommand -> Maybe ScriptName
migrationScriptName = \case
  MigrationScript name _ -> Just name
  MigrationInitialization -> Nothing
  MigrationValidation _ -> Nothing

-- | Only to be used to reset the development DB
resetSchema :: Pool -> Logger -> IO ()
resetSchema pool logger = do
  Log.warn logger $ Log.msg (Log.val "resetting postgres schema")
  let session = do
        sql "DROP SCHEMA IF EXISTS public CASCADE"
        sql "CREATE SCHEMA IF NOT EXISTS public"
  either throwIO pure =<< use pool session
