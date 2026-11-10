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
-- with this program.  If not, see <https://www.gnu.org/licenses/>.

module Test.Migration.ListItems (testListItemsMigration) where

import API.Galley
import API.GalleyInternal (deleteInternalUser)
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers (createTeam)
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Cut the ListItems (user -> teams pagination) reads over to PostgreSQL.
--
-- The list-items read has no backfill worker of its own: it pages over the
-- @user_team@ rows copied by the team-member migration, so it must only be
-- flipped to @postgresql@ after @wire_team_members_migration_finished@
-- reaches 1.0 (see config-options.md).  This test proves that after the flip
-- the internal user deletion still drains the deleted user's team
-- memberships through the PostgreSQL read path.
testListItemsMigration :: (HasCallStack) => App ()
testListItemsMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    -- phase 1: cassandra: a team whose memberships live only in Cassandra
    (owner, tid, members) <- runCodensity (startDynamicBackend backend (phase1 backend)) $ \_ -> do
      r@(owner, tid, members) <- createTeam domain 2
      length members `shouldMatchInt` 2
      pure r

    -- phase 2: team member copy worker on: user_team is backfilled
    runCodensity (startDynamicBackend backend (phase2 backend)) $ \_ -> do
      waitForMigration domain counterName

    -- phase 3: postgresql: list items are served from PostgreSQL; deleting
    -- the user through the internal galley endpoint drains their team
    -- membership via the PostgreSQL read path
    runCodensity (startDynamicBackend backend (phase3 backend)) $ \_ -> do
      let doomed = head members
      assertSuccess =<< deleteInternalUser doomed
      listed <- getTeamMembers owner tid >>= getJSON 200 >>= asList >>= traverse (%. "user")
      listedIds <- traverse asString listed
      doomedId <- asString (doomed %. "id")
      assertBool "expected the deleted member to be drained from the team" (doomedId `notElem` listedIds)
      pure ()
  where
    counterName :: String
    counterName = "^wire_team_members_migration_finished"

    conf :: String -> String -> Bool -> ServiceOverrides
    conf teamStoreMode listItemsMode runMigration =
      def
        { galleyCfg =
            setField "postgresMigration.teamStore" teamStoreMode
              >=> setField "postgresMigration.listItems" listItemsMode,
          backgroundWorkerCfg =
            setField "postgresMigration.teamStore" teamStoreMode
              >=> setField "migrateTeamMembers" runMigration
        }

    phase1, phase2, phase3 :: (MakesValue backend) => backend -> ServiceOverrides
    phase1 backend = conf backend "cassandra" "cassandra" False
    phase2 backend = conf backend "migration-to-postgresql" "cassandra" True
    phase3 backend = conf backend "postgresql" "postgresql" False
