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

module Test.Migration.Invitations (testInvitationsMigration) where

import API.Brig (PostInvitation (..), listInvitations, postInvitation)
import API.Common (randomEmail)
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Drives the invitation store through the full cutover lifecycle
-- (cassandra -> migration-to-postgresql -> postgresql).  Each phase creates a
-- fresh team and invites a fresh email address; invitations are left open so
-- later phases can list them again, which exercises the writes and reads of
-- all three interpreters (Cassandra, dual-write, Postgres).
--
-- The invitations created before the cutover (pure Cassandra writes) must be
-- served by the pure-Postgres interpreter afterwards — that proves the
-- background worker backfilled rows that only ever existed in Cassandra.
testInvitationsMigration :: (HasCallStack) => App ()
testInvitationsMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

        conf db runMigration =
          def
            { brigCfg = setField "postgresMigration.invitations" db,
              backgroundWorkerCfg =
                setField "postgresMigration.invitations" db
                  >=> setField "migrateInvitations" runMigration
            }

        counterName = "^wire_invitations_migration_finished"

    -- P1 cassandra: write and read via the pure Cassandra interpreter
    (owner1, email1, tid1) <-
      runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ ->
        setupInvitation domain

    -- P2 migration-to-postgresql (worker off): reads still come from
    -- Cassandra, writes go to both stores
    (owner2, email2, tid2) <-
      runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) $ \_ -> do
        assertInvitation owner1 email1 tid1
        setupInvitation domain

    -- P3 migration-to-postgresql (worker on): dual-write while backfilling
    (owner3, email3, tid3) <-
      runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
        assertInvitation owner2 email2 tid2
        waitForMigration domain counterName
        setupInvitation domain

    -- P4 postgresql: reads are served exclusively from Postgres
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      -- Invitations written by every interpreter are visible to the pure
      -- Postgres interpreter; (owner1, email1) only ever existed in Cassandra.
      assertInvitation owner1 email1 tid1
      assertInvitation owner2 email2 tid2
      assertInvitation owner3 email3 tid3

-- | Create a fresh team and invite a fresh email address to it.  Returns the
-- owner, the invited email and the team id so later phases can look the
-- invitation up again.
setupInvitation :: (HasCallStack) => String -> App (Value, String, String)
setupInvitation domain = do
  (owner, tid, []) <- createTeam domain 0
  email <- randomEmail
  postInvitation owner (PostInvitation (Just email) Nothing)
    >>= assertStatus 201
  pure (owner, email, tid)

-- | List the team's invitations and assert the given email is among them.
assertInvitation :: (HasCallStack) => Value -> String -> String -> App ()
assertInvitation owner email tid =
  bindResponse (listInvitations owner tid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    invitations <- resp.json %. "invitations" >>= asList
    emails <- traverse (\inv -> inv %. "email" >>= asString) invitations
    emails `shouldContain` [email]
