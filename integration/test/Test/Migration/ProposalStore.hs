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

module Test.Migration.ProposalStore where

import Control.Monad.Codensity
import Control.Monad.Reader
import MLS.Util
import SetupHelpers
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Drives the MLS proposal store through the full cutover lifecycle
-- (cassandra -> migration-to-postgresql -> postgresql). Pending proposals are
-- uploaded in one phase and committed in a later phase, so every phase reads
-- the rows written by the previous phases.
--
-- Phase 2 proves Cassandra reads in migration mode (the commit picks up u1's
-- proposal written under the pure Cassandra interpreter). Phase 3 lets the
-- background worker copy u3's Cassandra-only row to Postgres. Phase 4 proves
-- the pure Postgres interpreter serves copied rows (u6's proposal was never
-- dual-written, so its commit only succeeds on copied data) and exercises
-- StoreProposal / GetAllPendingProposals / commit in Postgres mode.
testProposalStoreMigration :: (HasCallStack) => App ()
testProposalStoreMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain
    (alice : u1 : u2 : u3 : u4 : u5 : u6 : u7 : _) <-
      runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ ->
        createAndConnectUsers (replicate 8 domain)
    alice1 <- createMLSClient def alice
    conv <- createNewGroup def alice1
    -- conv2 isolates a Cassandra-only proposal: u6 submits in P1 and the
    -- group's pending commit only happens in P4, so it succeeds only if the
    -- background worker copied the row from Cassandra to Postgres.
    conv2 <- createNewGroup def alice1

    let submitProposalInto c target = do
          targetC <- createMLSClient def target
          void $ uploadNewKeyPackage def targetC
          [proposal] <- createAddProposals c alice1 [target]
          void $ sendAndConsumeMessage proposal

    -- P1 cassandra: proposals stored via the pure Cassandra interpreter
    runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
      submitProposalInto conv u1
      submitProposalInto conv2 u6

    -- P2 migration-to-postgresql (worker off): reads still come from
    -- Cassandra, writes go to both stores
    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) $ \_ -> do
      submitProposalInto conv u2
      void $ createPendingProposalCommit conv alice1 >>= sendAndConsumeCommitBundle
      submitProposalInto conv u3

    -- P3 migration-to-postgresql (worker on): dual-write while backfilling
    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      submitProposalInto conv u4
      waitForMigration domain counterName
      void $ createPendingProposalCommit conv alice1 >>= sendAndConsumeCommitBundle
      submitProposalInto conv u5

    -- P4 postgresql: reads are served exclusively from Postgres
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      -- u6's proposal only ever existed in Cassandra (dual-write never saw
      -- it), so this commit only succeeds if the background worker copied it
      -- to Postgres.
      void $ createPendingProposalCommit conv2 alice1 >>= sendAndConsumeCommitBundle

      -- Write and read via the pure Postgres interpreter.
      submitProposalInto conv u7
  where
    conf db runMigration =
      def
        { galleyCfg = setField "postgresMigration.proposal" db,
          backgroundWorkerCfg = setField "migrateProposals" runMigration
        }
    counterName = "^wire_proposals_migration_finished"
