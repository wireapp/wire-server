module Test.Migration.DomainRegistration (testDomainRegistrationMigration) where

import qualified API.Brig as Brig
import qualified API.BrigInternal as BrigInternal
import API.Common
import qualified API.GalleyInternal as GalleyInternal
import Control.Error (MaybeT (..))
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers
import Test.DNSMock
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

data DomainRegistrationTestCase = TeamFlow TeamStep | OnPremFlow OnPremStep

type EmailDomain = String

type AuthToken = String

type TeamId = String

type Owner = Value

type Config = Value

type OwnershipToken = String

data OnPremStep
  = PreAuthorization EmailDomain
  | SetupChallenge EmailDomain
  | VerifyDomain EmailDomain ChallengeSetup
  | PostConfig EmailDomain AuthToken Config
  | OnPremVerify EmailDomain Config
  | OnPremSuccess EmailDomain Config

data TeamStep
  = TeamSetupChallenge (Owner, TeamId) EmailDomain
  | TeamVerifyDomain (Owner, TeamId) EmailDomain ChallengeSetup
  | TeamAuthorizeTeam (Owner, TeamId) EmailDomain OwnershipToken
  | TeamUpdateConfig (Owner, TeamId) EmailDomain
  | TeamSuccess (Owner, TeamId) EmailDomain

testDomainRegistrationMigration :: (HasCallStack) => App ()
testDomainRegistrationMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain
    let initTestCases = do
          [t1, t2, t3, t4] <- replicateM 4 $ OnPremFlow . PreAuthorization <$> randomDomain
          [t5, t6, t7, t8] <- replicateM 4 $ do
            (owner, tid, _) <- createTeam domain 1
            GalleyInternal.setTeamFeatureLockStatus owner tid "domainRegistration" "unlocked"
            GalleyInternal.setTeamFeatureStatus owner tid "domainRegistration" "enabled" >>= assertSuccess
            TeamFlow . TeamSetupChallenge (owner, tid) <$> randomDomain

          sequence
            [ pure t1,
              runStep domain t2,
              runStep domain t3 >>= runStep domain,
              runStep domain t4 >>= runStep domain >>= runStep domain,
              pure t5,
              runStep domain t6,
              runStep domain t7 >>= runStep domain,
              runStep domain t8 >>= runStep domain >>= runStep domain
            ]

    testCases1 <- runCodensity (startDynamicBackend backend (conf "cassandra" False)) . const $ do
      testCases0 <- initTestCases
      nextStepCases <- for testCases0 (runStep domain)
      newCases <- initTestCases
      pure $ nextStepCases <> newCases

    testCases2 <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) . const $ do
      nextStepCases <- for testCases1 (runStep domain)
      newCases <- initTestCases
      pure $ nextStepCases <> newCases

    testCases3 <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) . const $ do
      nextStepCases <- for testCases2 (runStep domain)
      newCases <- initTestCases
      waitForMigration domain counterName

      nextStepCases' <- for (nextStepCases <> newCases) (runStep domain)
      newCases' <- initTestCases
      pure $ nextStepCases' <> newCases'

    runCodensity (startDynamicBackend backend (conf "postgresql" False)) . const $ do
      for_ testCases3 (runAll domain)
  where
    runStep :: (HasCallStack) => String -> DomainRegistrationTestCase -> App DomainRegistrationTestCase
    -- TEAM FLOW
    runStep domain (TeamFlow (TeamSetupChallenge team emailDomain)) = do
      challenge <- setupChallenge domain emailDomain
      registerTechnitiumRecord challenge.technitiumToken emailDomain ("wire-domain." <> emailDomain) "TXT" challenge.dnsToken
      pure $ TeamFlow $ TeamVerifyDomain team emailDomain challenge
    runStep _ (TeamFlow (TeamVerifyDomain team@(owner, _) emailDomain challenge)) = do
      token <- bindResponse (Brig.verifyDomainForTeam owner emailDomain challenge.challengeId challenge.challengeToken) $ \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "domain_ownership_token" & asString
      pure $ TeamFlow $ TeamAuthorizeTeam team emailDomain token
    runStep _ (TeamFlow (TeamAuthorizeTeam team@(owner, _) emailDomain token)) = do
      Brig.authorizeTeam owner emailDomain token >>= assertStatus 200
      pure $ TeamFlow $ TeamUpdateConfig team emailDomain
    runStep domain (TeamFlow (TeamUpdateConfig team@(owner, tid) emailDomain)) = do
      bindResponse (Brig.updateTeamInvite owner emailDomain (object ["team_invite" .= "team", "team" .= tid])) $ \res -> do
        res.status `shouldMatchInt` 200
      verifyTeamConfig domain tid emailDomain
      pure $ TeamFlow $ TeamSuccess team emailDomain
    runStep domain (TeamFlow (TeamSuccess team@(_, tid) emailDomain)) = do
      verifyTeamConfig domain tid emailDomain
      pure $ TeamFlow $ TeamSuccess team emailDomain
    -- ON PREM FLOW
    runStep domain (OnPremFlow (PreAuthorization emailDomain)) = do
      BrigInternal.domainRegistrationPreAuthorize domain emailDomain >>= assertStatus 204
      pure $ OnPremFlow $ SetupChallenge emailDomain
    runStep domain (OnPremFlow (SetupChallenge emailDomain)) = do
      challenge <- setupChallenge domain emailDomain
      registerTechnitiumRecord challenge.technitiumToken emailDomain ("wire-domain." <> emailDomain) "TXT" challenge.dnsToken
      pure $ OnPremFlow $ VerifyDomain emailDomain challenge
    runStep domain (OnPremFlow (VerifyDomain emailDomain challenge)) = do
      bindResponse (BrigInternal.getDomainRegistration domain emailDomain) $ \res -> do
        res.status `shouldMatchInt` 200
      token <- bindResponse (Brig.verifyDomain domain emailDomain challenge.challengeId challenge.challengeToken) $ \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "domain_ownership_token" & asString
      let config = mkDomainRedirectBackend "https://wire.example.com" "https://webapp.wire.example.com"
      pure $ OnPremFlow $ PostConfig emailDomain token config
    runStep domain (OnPremFlow (PostConfig emailDomain token config)) = do
      Brig.updateDomainRedirect domain Versioned emailDomain (Just token) config
        >>= assertStatus 200
      pure $ OnPremFlow (OnPremVerify emailDomain config)
    runStep domain (OnPremFlow (OnPremVerify emailDomain config)) = do
      verifyOnPremConfig domain emailDomain config
      pure $ OnPremFlow $ OnPremSuccess emailDomain config
    runStep domain success@(OnPremFlow (OnPremSuccess emailDomain config)) = do
      verifyOnPremConfig domain emailDomain config
      pure success

    verifyOnPremConfig :: (HasCallStack) => String -> String -> Value -> App ()
    verifyOnPremConfig domain emailDomain config =
      bindResponse (Brig.getDomainRegistrationFromEmail domain Versioned ("ruffy@" ++ emailDomain)) \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "domain_redirect" `shouldMatch` (config %. "domain_redirect")
        let backendUrl v = runMaybeT $ lookupFieldM v "backend" >>= flip lookupFieldM "config_url"
            webappUrl v = runMaybeT $ lookupFieldM v "backend" >>= flip lookupFieldM "webapp_url"
        backendUrl resp.json `shouldMatch` backendUrl config
        webappUrl resp.json `shouldMatch` webappUrl config

    verifyTeamConfig :: (HasCallStack) => String -> String -> String -> App ()
    verifyTeamConfig domain tid emailDomain = do
      bindResponse (BrigInternal.getDomainRegistration domain emailDomain) $ \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "domain" `shouldMatch` emailDomain
        resp.json %. "domain_redirect" `shouldMatch` "none"
        resp.json %. "team_invite" `shouldMatch` "team"
        resp.json %. "team" `shouldMatch` tid

      bindResponse (Brig.getDomainRegistrationFromEmail domain Versioned ("ruffy@" ++ emailDomain)) \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "domain_redirect" `shouldMatch` "none"

    runAll :: (HasCallStack) => String -> DomainRegistrationTestCase -> App ()
    runAll domain success@(OnPremFlow (OnPremSuccess _ _)) = void $ runStep domain success
    runAll domain success@(TeamFlow (TeamSuccess _ _)) = void $ runStep domain success
    runAll domain inProgress = runAll domain =<< runStep domain inProgress

    mkDomainRedirectBackend :: String -> String -> Value
    mkDomainRedirectBackend configUrl webappUrl =
      object
        [ "domain_redirect" .= "backend",
          "backend" .= object ["config_url" .= configUrl, "webapp_url" .= webappUrl]
        ]

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
