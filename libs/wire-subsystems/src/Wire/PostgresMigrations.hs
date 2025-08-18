{-# LANGUAGE TemplateHaskell #-}

module Wire.PostgresMigrations where

import Control.Exception
import Data.FileEmbed
import Data.Text.Encoding qualified as T
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Migration
import Hasql.Pool
import Hasql.Session
import Hasql.Statement (Statement (..))
import Hasql.Statement qualified as Statement
import Hasql.Transaction qualified as Tx
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
        Log.warn logger $ Log.msg (Log.val "Running migrations")
        transaction Serializable Write $ do
          forM_ (MigrationInitialization : allMigrations) $ \migrationCmd -> do
            let name = migrationName migrationCmd
            void $ Tx.statement (T.decodeUtf8 name) lock
            mErr <- runMigration migrationCmd
            void $ Tx.statement (T.decodeUtf8 name) unlock
            case mErr of
              Nothing -> pure ()
              Just err -> do
                throw $ PostgresMigrationError err
        Log.warn logger $ Log.msg (Log.val "Migrations completed successfully")

  either throwIO pure =<< use pool session
  where
    lock :: Statement Text ()
    lock =
      Statement.Statement
        "SELECT true from pg_advisory_lock(hashtextextended($1 :: text, 0))"
        (E.param (E.nonNullable E.text))
        D.noResult
        True

    unlock :: Statement Text ()
    unlock =
      Statement.Statement
        "SELECT true from pg_advisory_unlock(hashtextextended($1 :: text, 0))"
        (E.param (E.nonNullable E.text))
        D.noResult
        True

migrationName :: MigrationCommand -> ByteString
migrationName = \case
  MigrationInitialization -> "Initialize Migration Schema"
  MigrationScript name _ -> fromString name
  MigrationValidation cmd -> "validation:" <> migrationName cmd

migrationNameLog :: MigrationCommand -> (Log.Msg -> Log.Msg)
migrationNameLog = \case
  MigrationInitialization -> Log.field "migration" ("Initialize Migration Schema" :: ByteString)
  MigrationScript name _ -> Log.field "migration" name
  MigrationValidation cmd -> Log.field "type" ("validation" :: ByteString) . migrationNameLog cmd

-- | Only to be used to reset the development DB
resetSchema :: Pool -> Logger -> IO ()
resetSchema pool logger = do
  Log.warn logger $ Log.msg (Log.val "resetting postgres schema")
  let session = do
        sql "DROP SCHEMA IF EXISTS public CASCADE"
        sql "CREATE SCHEMA IF NOT EXISTS public"
  either throwIO pure =<< use pool session
