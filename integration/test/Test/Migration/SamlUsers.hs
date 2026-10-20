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

module Test.Migration.SamlUsers (testSamlUsersMigration) where

import API.Common
import API.GalleyInternal (setTeamFeatureStatus)
import API.Spar
import Control.Monad.Codensity
import Control.Monad.Reader
import Data.String.Conversions (cs)
import qualified SAML2.WebSSO as SAML
import SetupHelpers
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

-- | Migrate spar's @saml user@ store from Cassandra to PostgreSQL.
--
-- A SAML user is created through a full IdP-initiated login while Cassandra is
-- the source of truth. The background worker then backfills the store into
-- PostgreSQL, and after the cutover the same user can log in again (served
-- from PostgreSQL) and still blocks IdP deletion via the paginated listing.
testSamlUsersMigration :: (HasCallStack) => App ()
testSamlUsersMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain
        mkNameId = fromRight (error "could not create name id") . SAML.emailNameID . cs

    -- Cassandra: create a team with SSO enabled and a SAML user via login.
    (owner, tid, idpId, idp, email) <- runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
      (owner, tid, _) <- createTeam domain 1
      assertSuccess =<< setTeamFeatureStatus domain tid "sso" "enabled"
      (createIdpResp, idp) <- registerTestIdPWithMetaWithPrivateCreds owner
      idpId <- createIdpResp.json %. "id" >>= asString
      email <- randomEmail
      (mUid, _) <- loginWithSamlWithZHost Nothing domain True tid (mkNameId email) (idpId, idp)
      void $ assertJust "expected a SAML user to be created" mUid
      pure (owner, tid, idpId, (idpId, idp), email)

    -- migration-to-postgresql with the worker running: backfill the store.
    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      waitForMigration domain counterName

    -- PostgreSQL only: the SAML user must be served from PostgreSQL; a new
    -- login resolves the same user, and deleting the IdP without purging is
    -- rejected because the paginated listing still finds the bound user.
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      (mUid, _) <- loginWithSamlWithZHost Nothing domain True tid (mkNameId email) idp
      void $ assertJust "expected the SAML user to survive the migration" mUid
      deleteIdp owner idpId >>= assertLabel 412 "idp-has-bound-users"
  where
    conf :: String -> Bool -> ServiceOverrides
    conf db runMigration =
      def
        { sparCfg =
            setField "postgresMigration.samlUsers" db,
          backgroundWorkerCfg =
            setField "postgresMigration.samlUsers" db
              >=> setField "migrateSamlUsers" runMigration
        }

    counterName :: String
    counterName = "^wire_saml_users_migration_finished"
