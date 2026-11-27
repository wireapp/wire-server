-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Spar.RunSpec where

import Control.Exception (ErrorCall (..))
import Imports
import Spar.Run (rejectScimStoreMigration)
import Test.Hspec
import Wire.PostgresMigrationOpts (PostgresMigrationOpts (..), StorageLocation (..))

spec :: Spec
spec = describe "rejectScimStoreMigration" $ do
  it "accepts cassandra and postgresql for the SCIM stores" $ do
    rejectScimStoreMigration (opts CassandraStorage CassandraStorage)
    rejectScimStoreMigration (opts PostgresqlStorage PostgresqlStorage)
    rejectScimStoreMigration (opts CassandraStorage PostgresqlStorage)

  it "rejects migration-to-postgresql for scimTokens" $
    rejectScimStoreMigration (opts MigrationToPostgresql PostgresqlStorage)
      `shouldThrow` scimError "scimTokens"

  it "rejects migration-to-postgresql for scimExternalIds" $
    rejectScimStoreMigration (opts CassandraStorage MigrationToPostgresql)
      `shouldThrow` scimError "scimExternalIds"

  it "ignores migration-to-postgresql on other stores" $
    rejectScimStoreMigration
      (opts PostgresqlStorage PostgresqlStorage) {conversation = MigrationToPostgresql}
  where
    scimError :: String -> Selector ErrorCall
    scimError key (ErrorCall msg) = ("postgresMigration." <> key <> ":") `isInfixOf` msg

    opts :: StorageLocation -> StorageLocation -> PostgresMigrationOpts
    opts tokens externalIds =
      PostgresMigrationOpts
        { conversation = CassandraStorage,
          conversationCodes = CassandraStorage,
          teamFeatures = CassandraStorage,
          domainRegistration = CassandraStorage,
          user = CassandraStorage,
          blockList = CassandraStorage,
          passwordReset = CassandraStorage,
          customBackend = CassandraStorage,
          budget = CassandraStorage,
          nonce = CassandraStorage,
          connections = CassandraStorage,
          sessions = CassandraStorage,
          scimTokens = tokens,
          scimExternalIds = externalIds
        }
