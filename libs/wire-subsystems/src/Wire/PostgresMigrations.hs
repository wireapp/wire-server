{-# LANGUAGE TemplateHaskell #-}

module Wire.PostgresMigrations where

import Control.Exception
import Data.FileEmbed
import Hasql.Migration
import Hasql.Pool
import Hasql.Session
import Hasql.Transaction.Sessions
import Imports
import System.Logger (Logger)
import System.Logger qualified as Log

allMigrations :: [MigrationCommand]
allMigrations = map (uncurry MigrationScript) $(makeRelativeToProject "postgres-migrations" >>= embedDir)

data PostgresMigrationError = PostgresMigrationError MigrationError
  deriving (Show)

instance Exception PostgresMigrationError

runAllMigrations :: Pool -> Logger -> IO ()
runAllMigrations pool logger = do
  let session = do
        Log.info logger $ Log.msg (Log.val "Running migrations")
        transaction Serializable Write $ do
          forM_ (MigrationInitialization : allMigrations) $ \migrationCmd -> do
            mErr <- runMigration migrationCmd
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

-- | Only to be used to reset the development DB
resetSchema :: Pool -> Logger -> IO ()
resetSchema pool logger = do
  Log.warn logger $ Log.msg (Log.val "resetting postgres schema")
  let session = do
        sql "DROP SCHEMA IF EXISTS public CASCADE"
        sql "CREATE SCHEMA IF NOT EXISTS public"
  either throwIO pure =<< use pool session
