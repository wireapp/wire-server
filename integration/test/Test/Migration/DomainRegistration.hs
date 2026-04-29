module Test.Migration.DomainRegistration where

import qualified API.Brig as Brig
import qualified API.BrigInternal as BrigInternal
import API.Common
import qualified API.GalleyInternal as GalleyInternal
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

data DomainRegistrationMigrationFixtures = DomainRegistrationMigrationFixtures
  { backendDomain :: String,
    backendOwnershipToken :: String,
    challengeDomain :: String,
    challengeId :: String,
    challengeToken :: String,
    legacyBackendDomain :: String,
    lockedDomain :: String,
    teamOwner :: Value,
    teamId :: String,
    teamDomain :: String,
    dualWriteDomain :: String,
    activeMigrationDomain :: String
  }

testDomainRegistrationMigration :: (HasCallStack) => App ()
testDomainRegistrationMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain
        preMigration = runCodensity (startDynamicBackend backend (conf "cassandra" False)) . const
        switchToMigratingInterpreter = runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) . const
        startMigration = runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) . const
        switchToPostgresInterpreter = runCodensity (startDynamicBackend backend (conf "postgresql" False)) . const

    fixtures <- preMigration $ do
      backendDomain <- randomDomain
      challengeDomain <- randomDomain
      legacyBackendDomain <- randomDomain
      lockedDomain <- randomDomain
      teamDomain <- randomDomain
      dualWriteDomain <- randomDomain
      activeMigrationDomain <- randomDomain

      BrigInternal.domainRegistrationPreAuthorize domain backendDomain >>= assertStatus 204
      backendSetup <- setupOwnershipTokenForBackend domain backendDomain

      BrigInternal.domainRegistrationPreAuthorize domain challengeDomain >>= assertStatus 204
      challenge <- setupChallengeAndDnsRecord domain challengeDomain

      BrigInternal.updateDomainRegistration domain legacyBackendDomain legacyBackendUpdate >>= assertStatus 204
      BrigInternal.domainRegistrationLock domain lockedDomain >>= assertStatus 204

      (teamOwner, teamId, _) <- createTeam domain 1
      enableDomainRegistrationFeature teamOwner teamId
      teamSetup <- setupOwnershipTokenForTeam teamOwner teamDomain
      Brig.authorizeTeam teamOwner teamDomain teamSetup.ownershipToken >>= assertStatus 200

      pure
        DomainRegistrationMigrationFixtures
          { backendOwnershipToken = backendSetup.ownershipToken,
            challengeId = challenge.challengeId,
            challengeToken = challenge.challengeToken,
            ..
          }

    switchToMigratingInterpreter $ do
      assertBackendRegistration domain fixtures.legacyBackendDomain "https://legacy.example.com" "https://webapp.legacy.example.com" "not-allowed"
      assertRegistration domain fixtures.lockedDomain "locked" "allowed"
      assertRegisteredDomains fixtures.teamOwner fixtures.teamId [fixtures.teamDomain]

      BrigInternal.domainRegistrationPreAuthorize domain fixtures.dualWriteDomain >>= assertStatus 204
      assertRegistration domain fixtures.dualWriteDomain "pre-authorized" "allowed"

    startMigration $ do
      BrigInternal.updateDomainRegistration domain fixtures.activeMigrationDomain activeMigrationUpdate >>= assertStatus 204
      assertSsoTeamRegistration domain fixtures.activeMigrationDomain activeMigrationSsoCode activeMigrationTeamId
      waitForMigration domain counterName

    switchToPostgresInterpreter $ do
      Brig.updateDomainRedirect
        domain
        Versioned
        fixtures.backendDomain
        (Just fixtures.backendOwnershipToken)
        (Brig.mkDomainRedirectBackend Versioned "https://wire.example.com" "https://webapp.wire.example.com")
        >>= assertStatus 200
      assertBackendLookup domain fixtures.backendDomain "https://wire.example.com" "https://webapp.wire.example.com"

      Brig.verifyDomain domain fixtures.challengeDomain fixtures.challengeId fixtures.challengeToken >>= assertStatus 200
      Brig.verifyDomain domain fixtures.challengeDomain fixtures.challengeId fixtures.challengeToken >>= assertStatus 404

      assertBackendRegistration domain fixtures.legacyBackendDomain "https://legacy.example.com" "https://webapp.legacy.example.com" "not-allowed"
      assertRegistration domain fixtures.dualWriteDomain "pre-authorized" "allowed"
      assertSsoTeamRegistration domain fixtures.activeMigrationDomain activeMigrationSsoCode activeMigrationTeamId

      assertRegistration domain fixtures.lockedDomain "locked" "allowed"
      BrigInternal.domainRegistrationUnlock domain fixtures.lockedDomain >>= assertStatus 204
      assertRegistration domain fixtures.lockedDomain "none" "allowed"

      assertRegisteredDomains fixtures.teamOwner fixtures.teamId [fixtures.teamDomain]
      Brig.updateTeamInvite fixtures.teamOwner fixtures.teamDomain (object ["team_invite" .= "team", "team" .= fixtures.teamId])
        >>= assertStatus 200
      assertRegistration domain fixtures.teamDomain "none" "team"

legacyBackendUpdate :: Value
legacyBackendUpdate =
  object
    [ "domain_redirect" .= "backend",
      "backend"
        .= object
          [ "config_url" .= "https://legacy.example.com",
            "webapp_url" .= "https://webapp.legacy.example.com"
          ],
      "team_invite" .= "not-allowed"
    ]

activeMigrationUpdate :: Value
activeMigrationUpdate =
  object
    [ "domain_redirect" .= "sso",
      "sso_code" .= activeMigrationSsoCode,
      "team_invite" .= "team",
      "team" .= activeMigrationTeamId
    ]

activeMigrationSsoCode :: String
activeMigrationSsoCode = "f82bad56-df61-49c0-bc9a-dc45c8ee1000"

activeMigrationTeamId :: String
activeMigrationTeamId = "3bc23f21-dc03-4922-9563-c3beedf895db"

enableDomainRegistrationFeature :: (HasCallStack, MakesValue user) => user -> String -> App ()
enableDomainRegistrationFeature owner tid = do
  GalleyInternal.setTeamFeatureLockStatus owner tid "domainRegistration" "unlocked"
  assertSuccess =<< GalleyInternal.setTeamFeatureStatus owner tid "domainRegistration" "enabled"

assertRegistration :: (HasCallStack) => String -> String -> String -> String -> App ()
assertRegistration domain emailDomain expectedRedirect expectedInvite =
  bindResponse (BrigInternal.getDomainRegistration domain emailDomain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` emailDomain
    resp.json %. "domain_redirect" `shouldMatch` expectedRedirect
    resp.json %. "team_invite" `shouldMatch` expectedInvite

assertBackendRegistration :: (HasCallStack) => String -> String -> String -> String -> String -> App ()
assertBackendRegistration domain emailDomain expectedConfigUrl expectedWebappUrl expectedInvite =
  bindResponse (BrigInternal.getDomainRegistration domain emailDomain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` emailDomain
    resp.json %. "domain_redirect" `shouldMatch` "backend"
    resp.json %. "team_invite" `shouldMatch` expectedInvite
    resp.json %. "backend.config_url" `shouldMatch` expectedConfigUrl
    resp.json %. "backend.webapp_url" `shouldMatch` expectedWebappUrl

assertSsoTeamRegistration :: (HasCallStack) => String -> String -> String -> String -> App ()
assertSsoTeamRegistration domain emailDomain expectedSsoCode expectedTeam =
  bindResponse (BrigInternal.getDomainRegistration domain emailDomain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` emailDomain
    resp.json %. "domain_redirect" `shouldMatch` "sso"
    resp.json %. "team_invite" `shouldMatch` "team"
    resp.json %. "sso_code" `shouldMatch` expectedSsoCode
    resp.json %. "team" `shouldMatch` expectedTeam

assertBackendLookup :: (HasCallStack) => String -> String -> String -> String -> App ()
assertBackendLookup domain emailDomain expectedConfigUrl expectedWebappUrl =
  bindResponse (Brig.getDomainRegistrationFromEmail domain Versioned ("user@" <> emailDomain)) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain_redirect" `shouldMatch` "backend"
    resp.json %. "backend.config_url" `shouldMatch` expectedConfigUrl
    resp.json %. "backend.webapp_url" `shouldMatch` expectedWebappUrl

assertRegisteredDomains :: (HasCallStack, MakesValue user) => user -> String -> [String] -> App ()
assertRegisteredDomains owner tid expectedDomains =
  bindResponse (Brig.getRegisteredDomainsByTeam owner tid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    actualDomains <- resp.json %. "registered_domains" & asList >>= traverse (asString . (%. "domain"))
    actualDomains `shouldMatchSet` expectedDomains

conf :: String -> Bool -> ServiceOverrides
conf db runMigration =
  def
    { brigCfg = setField "postgresMigration.domainRegistration" db,
      backgroundWorkerCfg =
        setField "postgresMigration.domainRegistration" db
          >=> setField "migrateDomainRegistration" runMigration
    }

counterName :: String
counterName = "^wire_domain_registration_migration_finished"
