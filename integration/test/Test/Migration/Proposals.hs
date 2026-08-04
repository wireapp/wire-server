module Test.Migration.Proposals where

import API.Galley
import Control.Monad.Codensity
import Control.Monad.Reader
import MLS.Util
import SetupHelpers
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Migrates the MLS proposal store ('mls_proposal_refs') from Cassandra to
-- PostgreSQL.
--
-- Drives the operator lifecycle across dynamic backends: @cassandra@ ->
-- @migration-to-postgresql@ (worker off) -> @migration-to-postgresql@ (worker on)
-- -> @postgresql@. It verifies that galley and the background worker boot under
-- each 'StorageLocation', that the worker runs to completion
-- (@'waitForMigration'@ reaches @1.0@), and that MLS group operation keeps
-- working after the cutover to PostgreSQL (internal-commit processing resolves
-- pending proposal references through @'getAllPendingProposals'@, now served by
-- the PostgreSQL interpreter).
--
-- Note: this test does not assert backfill read-back fidelity of pre-existing
-- rows (pending proposals are short-lived and consumed by later commits, so none
-- reliably survive the cutover to read back). The bytea marshalling and the
-- TTL-preserving backfill upsert are covered by the 'PostgresMarshall' roundtrip
-- properties in @wire-api@ and by the proven @CodeStore@ upsert this migration
-- mirrors.
testProposalsMigration :: (HasCallStack) => App ()
testProposalsMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    (admin, convId, adminClient) <-
      runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
        (admin, _tid, _members) <- createTeam domain 2
        bob <- randomUser domain def
        [adminClient, bobClient] <- traverse (createMLSClient def) [admin, bob]
        void $ uploadNewKeyPackage def bobClient
        convId <- createNewGroup def adminClient
        void $ createAddCommit adminClient convId [bob] >>= sendAndConsumeCommitBundle
        -- Store a pending (backend) proposal under Cassandra.
        void $ createPendingProposalCommit convId adminClient >>= sendAndConsumeCommitBundle
        pure (admin, convId, adminClient)

    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) $ \_ -> do
      -- New writes are dual-written; the previously stored proposal is still
      -- Cassandra-only until the worker runs.
      void $ createPendingProposalCommit convId adminClient >>= sendAndConsumeCommitBundle

    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      waitForMigration domain counterName

    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      -- Reads now come from PostgreSQL; MLS commit processing resolves pending
      -- proposal references through the migrated store.
      void $ createPendingProposalCommit convId adminClient >>= sendAndConsumeCommitBundle
      bindResponse (getConversation admin (convIdToQidObject convId)) $ \resp -> do
        resp.status `shouldMatchInt` 200

conf :: String -> Bool -> ServiceOverrides
conf db runMigration =
  def
    { galleyCfg = setField "postgresMigration.proposals" db,
      backgroundWorkerCfg = setField "migrateProposals" runMigration
    }

counterName :: String
counterName = "^wire_mls_proposal_refs_migration_finished"
