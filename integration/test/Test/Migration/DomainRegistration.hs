module Test.Migration.DomainRegistration (testDomainRegistrationMigration) where

import qualified API.Brig as Brig
import qualified API.BrigInternal as BrigInternal
import API.Common
import Control.Error (MaybeT (..))
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers
import Test.DNSMock
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

data DomainRegistrationTestCase = TeamFlow | OnPremFlow OnPremStep

data OnPremStep
  = PreAuthorization String
  | SetupChallenge String
  | VerifyDomain String ChallengeSetup
  | PostConfig String String Value
  | OnPremVerify String Value
  | OnPremSuccess String Value

testDomainRegistrationMigration :: (HasCallStack) => App ()
testDomainRegistrationMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain
    let initOnPremTestCases = do
          [t1, t2, t3, t4] <- replicateM 4 $ OnPremFlow . PreAuthorization <$> randomDomain
          sequence
            [ pure t1,
              runStep domain t2,
              runStep domain t3 >>= runStep domain,
              runStep domain t4 >>= runStep domain >>= runStep domain
            ]

    testCases1 <- runCodensity (startDynamicBackend backend (conf "cassandra" False)) . const $ do
      testCases0 <- initOnPremTestCases
      nextStepCases <- for testCases0 (runStep domain)
      newCases <- initOnPremTestCases
      pure $ nextStepCases <> newCases

    testCases2 <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) . const $ do
      nextStepCases <- for testCases1 (runStep domain)
      newCases <- initOnPremTestCases
      pure $ nextStepCases <> newCases

    testCases3 <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) . const $ do
      nextStepCases <- for testCases2 (runStep domain)
      newCases <- initOnPremTestCases
      waitForMigration domain counterName

      nextStepCases' <- for (nextStepCases <> newCases) (runStep domain)
      newCases' <- initOnPremTestCases
      pure $ nextStepCases' <> newCases'

    runCodensity (startDynamicBackend backend (conf "postgresql" False)) . const $ do
      for_ testCases3 (runAll domain)
  where
    runStep :: (HasCallStack) => String -> DomainRegistrationTestCase -> App DomainRegistrationTestCase
    runStep _ TeamFlow = undefined
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
      verifyConfig domain emailDomain config
      pure $ OnPremFlow $ OnPremSuccess emailDomain config
    runStep domain success@(OnPremFlow (OnPremSuccess emailDomain config)) = do
      verifyConfig domain emailDomain config
      pure success

    verifyConfig :: (HasCallStack) => String -> String -> Value -> App ()
    verifyConfig domain emailDomain config =
      bindResponse (Brig.getDomainRegistrationFromEmail domain Versioned ("ruffy@" ++ emailDomain)) \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "domain_redirect" `shouldMatch` (config %. "domain_redirect")
        let backendUrl v = runMaybeT $ lookupFieldM v "backend" >>= flip lookupFieldM "config_url"
            webappUrl v = runMaybeT $ lookupFieldM v "backend" >>= flip lookupFieldM "webapp_url"
        backendUrl resp.json `shouldMatch` backendUrl config
        webappUrl resp.json `shouldMatch` webappUrl config

    runAll :: (HasCallStack) => String -> DomainRegistrationTestCase -> App ()
    runAll _ TeamFlow = undefined
    runAll domain success@(OnPremFlow (OnPremSuccess _ _)) = void $ runStep domain success
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
