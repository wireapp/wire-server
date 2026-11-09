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

module Test.Migration.ProviderService (testProviderServiceMigration) where

import API.Brig
import API.Common
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool
import UnliftIO (timeout)
import UnliftIO.Concurrent (threadDelay)

-- | Migrate brig's provider-platform service tables (@service@,
-- @service_tag@, @service_prefix@, @service_whitelist{,_rev}@) from Cassandra
-- to PostgreSQL. A provider service (and its tag/prefix index rows and a team
-- whitelist entry) is created while brig reads from Cassandra; after the
-- background worker backfills, brig reads exclusively from PostgreSQL: the
-- surviving service is still visible via the provider list and the public tag
-- search, while a service deleted during the dual-write phase must be absent.
testProviderServiceMigration :: (HasCallStack) => App ()
testProviderServiceMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain
    (alice, providerId, password, serviceAId, tid) <-
      runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
        (alice, tid, _) <- createTeam domain 1
        password <- randomString 20
        provider <- setupProvider alice def {newProviderPassword = Just password}
        pid <- provider %. "id" & asString
        service <-
          newService domain pid
            $ def {newServiceTags = [serviceTag]}
        sid <- asString $ service %. "id"
        assertStatus 200 =<< getService domain pid sid
        assertStatus 200 =<< updateServiceConn domain pid sid (object ["enabled" .= True, "password" .= password])
        -- whitelisted-services search must be empty before whitelisting
        bindResponse (listTeamServiceProfilesByPrefix alice tid Nothing False 50) $ \res -> do
          res.status `shouldMatchInt` 200
          results <- res.json & asList
          length results `shouldMatchInt` 0
        assertStatus 200 =<< postServiceWhitelist alice tid (object ["service" .= sid, "provider" .= pid, "status" .= True])
        pure (alice, pid, password, sid, tid)
    runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      for_ [counterName, tagCounterName, prefixCounterName, whitelistCounterName, whitelistRevCounterName] $ \n ->
        waitForMigration domain n
      -- dual-write proof: add a second service, then delete it through the
      -- provider API; the deletion must propagate to both stores.
      service <-
        newService domain providerId
          $ def {newServiceTags = [serviceTag]}
      sidB <- asString $ service %. "id"
      assertStatus 200 =<< deleteService domain providerId sidB password
      -- wait until the async deletion finished (service gone via Cassandra)
      timeout 30_000_000 (goDeleted domain providerId sidB)
        >>= maybe (assertFailure "service deletion did not complete in time") pure
    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      -- provider list served from PostgreSQL: exactly the surviving service
      bindResponse (listProviderServices domain providerId) $ \res -> do
        res.status `shouldMatchInt` 200
        sids <- (res.json & asList) >>= mapM (%. "id") >>= mapM asString
        sids `shouldMatch` [serviceAId]
      -- public tag search served from PostgreSQL: only the surviving service
      bindResponse (searchServices domain serviceTag) $ \res -> do
        res.status `shouldMatchInt` 200
        profiles <- res.json %. "results" & asList
        sids <- mapM (%. "id") profiles >>= mapM asString
        sids `shouldMatch` [serviceAId]
      -- team whitelist search served from PostgreSQL (backfilled whitelist)
      bindResponse (listTeamServiceProfilesByPrefix alice tid Nothing False 50) $ \res -> do
        res.status `shouldMatchInt` 200
        profiles <- res.json %. "results" & asList
        sids <- mapM (%. "id") profiles >>= mapM asString
        sids `shouldMatch` [serviceAId]
  where
    conf :: String -> Bool -> ServiceOverrides
    conf db runMigration =
      def
        { brigCfg = setField "postgresMigration.providerService" db,
          backgroundWorkerCfg = setField "migrateProviderService" runMigration
        }

    goDeleted domain pid sid = do
      res <- getService domain pid sid
      if res.status == 404
        then pure ()
        else do
          liftIO (threadDelay 100_000)
          goDeleted domain pid sid

counterName :: String
counterName = "^wire_provider_service_migration_finished"

tagCounterName :: String
tagCounterName = "^wire_provider_service_tag_migration_finished"

prefixCounterName :: String
prefixCounterName = "^wire_provider_service_prefix_migration_finished"

whitelistCounterName :: String
whitelistCounterName = "^wire_provider_service_whitelist_migration_finished"

whitelistRevCounterName :: String
whitelistRevCounterName = "^wire_provider_service_whitelist_rev_migration_finished"

serviceTag :: String
serviceTag = "audio"
