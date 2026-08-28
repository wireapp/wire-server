{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
-- for more details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.JobSubsystem.Migrations
  ( defaultSchemaName,
    mkArbiterConnectionString,
    runJobMigrations,
  )
where

import Arbiter.Core (defaultSchemaName)
import Arbiter.Migrations qualified as ArbiterMigrations
import Control.Exception (bracket, bracket_, throwIO)
import Data.Hashable qualified as Hashable
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Secret (SecretText, revealSecretText, secretText)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Hasql.Connection qualified as HasqlConnection
import Hasql.Connection.Settings qualified as HasqlConnectionSettings
import Hasql.Decoders qualified as HasqlDecoders
import Hasql.Encoders qualified as HasqlEncoders
import Hasql.Pool.Extended (runConnStrParser)
import Hasql.Session qualified as HasqlSession
import Hasql.Statement qualified as HasqlStatement
import Hasql.TH
import Imports
import PostgresqlConnectionString qualified
import System.IO.Error (userError)
import System.Timeout (timeout)
import Util.Options (FilePathSecrets, initCredentials)
import Wire.API.Jobs (JobRegistry, conversationsQueueName)

-- | Build the secret-bearing connection string used by the Arbiter migration
-- lock and migration runner.
mkArbiterConnectionString :: Map Text Text -> Maybe FilePathSecrets -> IO SecretText
mkArbiterConnectionString pgConfig mFpSecrets = do
  mPw <- for mFpSecrets initCredentials
  let pgConfig' = maybe pgConfig (\pw -> Map.insert "password" pw pgConfig) mPw
  connStr <- runConnStrParser $ PostgresqlConnectionString.fromKeyValueParams pgConfig'
  pure . secretText $ PostgresqlConnectionString.toKeyValueString connStr

-- | Apply all migrations for the job registry before constructing any worker
-- pools or accepting jobs.
runJobMigrations :: SecretText -> Text -> IO ()
runJobMigrations connStr schemaName =
  withArbiterMigrationLock connStr schemaName $ \lockConnection -> do
    result <-
      ArbiterMigrations.runMigrationsForRegistry
        (Proxy @JobRegistry)
        (Text.encodeUtf8 $ revealSecretText connStr)
        schemaName
        ArbiterMigrations.defaultMigrationConfig
    case result of
      ArbiterMigrations.MigrationSuccess -> ensureAdminlessJobsTeamIndex lockConnection
      ArbiterMigrations.MigrationError err ->
        throwIO . userError $
          "Arbiter migrations failed for schema " <> T.unpack schemaName <> ": " <> err
  where
    -- Add the lookup index after Arbiter has created the conversations queue.
    -- The partial index only covers unclaimed adminless jobs, which are the rows
    -- that feature teardown needs to select and cancel.
    ensureAdminlessJobsTeamIndex :: HasqlConnection.Connection -> IO ()
    ensureAdminlessJobsTeamIndex connection =
      runRawSql connection $
        "CREATE INDEX IF NOT EXISTS "
          <> quoteIdentifier "conversations_adminless_team_id_idx"
          <> " ON "
          <> quoteIdentifier schemaName
          <> "."
          <> quoteIdentifier conversationsQueueName
          <> " ((payload #>> '{data,team_id}'))"
          <> " WHERE claimed_by IS NULL"
          <> " AND payload->>'type' IN ('adminless_setup', 'adminless_deletion', 'adminless_reminder')"

    runRawSql :: HasqlConnection.Connection -> Text -> IO ()
    runRawSql connection sql = do
      result <-
        HasqlConnection.use connection $
          HasqlSession.statement
            ()
            (HasqlStatement.unpreparable sql HasqlEncoders.noParams HasqlDecoders.noResult)
      either
        (\err -> throwIO . userError $ "Arbiter SQL statement failed: " <> show err)
        pure
        result

    quoteIdentifier :: Text -> Text
    quoteIdentifier identifier = "\"" <> T.replace "\"" "\"\"" identifier <> "\""

-- | Serialize Arbiter schema migrations across all service instances that can
-- schedule or execute jobs. The lock is held on the same dedicated connection
-- for the whole migration because PostgreSQL advisory locks are session-scoped.
-- The connection is released and closed after the lock is released.
withArbiterMigrationLock :: SecretText -> Text -> (HasqlConnection.Connection -> IO a) -> IO a
withArbiterMigrationLock connStr schemaName action = do
  bracket acquireConnection HasqlConnection.release $ \lockConnection -> do
    bracket_
      (acquireArbiterMigrationLockWithTimeout lockConnection)
      (runAdvisoryLockStatement lockConnection releaseArbiterMigrationLock)
      (action lockConnection)
  where
    lockId :: Int64
    lockId = fromIntegral . Hashable.hash $ ("wire-server:arbiter-migrations:" <> schemaName :: Text)

    acquireArbiterMigrationLockWithTimeout :: HasqlConnection.Connection -> IO ()
    acquireArbiterMigrationLockWithTimeout connection = do
      acquired <- timeout arbiterMigrationLockWaitTimeoutMicros retryUntilAcquired
      case acquired of
        Just () -> pure ()
        Nothing ->
          throwIO . userError $
            "Timed out waiting for the Arbiter migration lock for schema " <> T.unpack schemaName
      where
        retryUntilAcquired :: IO ()
        retryUntilAcquired = do
          acquired <- runAdvisoryLockStatement connection tryArbiterMigrationLock
          if acquired
            then pure ()
            else do
              threadDelay arbiterMigrationLockRetryIntervalMicros
              retryUntilAcquired

        arbiterMigrationLockRetryIntervalMicros :: Int
        arbiterMigrationLockRetryIntervalMicros = 1_000_000

        -- Do not let a stuck migration block service startup indefinitely.
        arbiterMigrationLockWaitTimeoutMicros :: Int
        arbiterMigrationLockWaitTimeoutMicros = 1 * 60 * 1_000_000

    acquireConnection :: IO HasqlConnection.Connection
    acquireConnection = do
      connectionResult <- HasqlConnection.acquire . HasqlConnectionSettings.connectionString $ revealSecretText connStr
      either
        ( \err ->
            throwIO . userError $
              "Failed to acquire PostgreSQL connection for Arbiter migration lock: " <> show err
        )
        pure
        connectionResult

    runAdvisoryLockStatement :: HasqlConnection.Connection -> HasqlStatement.Statement Int64 a -> IO a
    runAdvisoryLockStatement connection statement = do
      result <- HasqlConnection.use connection (HasqlSession.statement lockId statement)
      either
        ( \err ->
            throwIO . userError $
              "Arbiter migration advisory lock query failed: " <> show err
        )
        pure
        result

    tryArbiterMigrationLock :: HasqlStatement.Statement Int64 Bool
    tryArbiterMigrationLock =
      [singletonStatement|SELECT (pg_try_advisory_lock($1 :: bigint) :: bool)|]

    releaseArbiterMigrationLock :: HasqlStatement.Statement Int64 ()
    releaseArbiterMigrationLock =
      [resultlessStatement|SELECT (1 :: integer) FROM (SELECT pg_advisory_unlock($1 :: bigint))|]
