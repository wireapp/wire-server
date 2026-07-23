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
  ( runJobMigrations,
  )
where

import Arbiter.Migrations qualified as ArbiterMigrations
import Control.Exception (bracket, bracket_, throwIO)
import Data.Hashable qualified as Hashable
import Data.Proxy (Proxy (..))
import Data.Secret (SecretText, revealSecretText)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Hasql.Connection qualified as HasqlConnection
import Hasql.Connection.Settings qualified as HasqlConnectionSettings
import Hasql.Session qualified as HasqlSession
import Hasql.Statement qualified as HasqlStatement
import Hasql.TH
import Imports
import System.IO.Error (userError)
import System.Timeout (timeout)
import Wire.API.Jobs (JobRegistry)

-- | Apply all migrations for the job registry before constructing any worker
-- pools or accepting jobs.
runJobMigrations :: SecretText -> Text -> IO ()
runJobMigrations connStr schemaName =
  withArbiterMigrationLock connStr schemaName $ do
    result <-
      ArbiterMigrations.runMigrationsForRegistry
        (Proxy @JobRegistry)
        (Text.encodeUtf8 $ revealSecretText connStr)
        schemaName
        ArbiterMigrations.defaultMigrationConfig
    case result of
      ArbiterMigrations.MigrationSuccess -> pure ()
      ArbiterMigrations.MigrationError err ->
        throwIO . userError $
          "Arbiter migrations failed for schema " <> T.unpack schemaName <> ": " <> err

-- | Serialize Arbiter schema migrations across all service instances that can
-- schedule or execute jobs. The lock is held on the same dedicated connection
-- for the whole migration because PostgreSQL advisory locks are session-scoped.
withArbiterMigrationLock :: SecretText -> Text -> IO a -> IO a
withArbiterMigrationLock connStr schemaName action = do
  bracket acquireConnection HasqlConnection.release $ \lockConnection -> do
    bracket_
      (acquireArbiterMigrationLockWithTimeout lockConnection)
      (runAdvisoryLockStatement lockConnection releaseArbiterMigrationLock)
      action
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
