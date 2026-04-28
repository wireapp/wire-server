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
import Data.Set qualified as Set
import Hasql.Migration
import Hasql.Pool
import Hasql.Session
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
            if maybe False (`Set.member` nonTransactionMigrations) (migrationScriptName migrationCmd)
              then runMigrationWithoutTransactions migrationCmd
              else transaction Serializable Write $ runMigration migrationCmd

          case mErr of
            Nothing -> pure ()
            Just err -> throw $ PostgresMigrationError err
        Log.info logger $ Log.msg (Log.val "Migrations completed successfully")

  either throwIO pure =<< use pool session

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
