module Test.Migration.TeamFeatures where

import qualified API.Galley as Public
import qualified API.GalleyInternal as Internal
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude hiding (pairs)
import Testlib.ResourcePool

testTeamFeaturesMigration :: (HasCallStack) => App ()
testTeamFeaturesMigration = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let preMigration = runCodensity (startDynamicBackend backend (conf "cassandra" False)) . const
        switchToMigratingInterpreter = runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) . const
        startMigration = runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) . const
        stopMigration = runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) . const
        switchToPostgresInterpreter = runCodensity (startDynamicBackend backend (conf "postgresql" False)) . const

    (teams, unmodifiedFeatures) <-
      preMigration $ do
        teams@((owner1, tid1, _) : _) <- replicateM 3 $ createTeam backend.berDomain 2
        -- unmodified features are the same for each team
        unmodifiedFeatures <- Public.getTeamFeatures owner1 tid1 >>= getJSON 200
        for_ teams $ \(owner, tid, _) -> enableFeatures owner tid unlockableFeatures
        pure (teams, unmodifiedFeatures)

    expectedModifiedFeatures <- mkExpectedModifiedFeatures unmodifiedFeatures

    switchToMigratingInterpreter $ pure ()

    startMigration $ waitForMigration backend.berDomain counterName

    stopMigration $ pure ()

    switchToPostgresInterpreter $ do
      assertModifiedFeatures teams expectedModifiedFeatures
  where
    unlockableFeatures :: [String]
    unlockableFeatures =
      [ "fileSharing",
        "conferenceCalling",
        "selfDeletingMessages",
        "conversationGuestLinks",
        "sndFactorPasswordChallenge",
        "mls",
        "outlookCalIntegration",
        "mlsE2EId",
        "mlsMigration",
        "enforceFileDownloadLocation",
        "domainRegistration",
        "channels",
        "cells",
        "consumableNotifications",
        "chatBubbles",
        "apps",
        "simplifiedUserConnectionRequestQRCode",
        "stealthUsers",
        "meetings",
        "meetingsPremium"
      ]

    assertModifiedFeatures  :: [(Value, String, [Value])] -> Value -> App ()
    assertModifiedFeatures teams expectedModifiedFeatures =
      for_ teams $ \(owner, tid, _) ->
        bindResponse (Public.getTeamFeatures owner tid) $ \resp -> do
          resp.status `shouldMatchInt` 200
          for_ unlockableFeatures $ \feat -> do
            resp.json %. feat %. "status" `shouldMatch` "enabled"
            resp.json %. feat %. "lockStatus" `shouldMatch` "unlocked"
          resp.json `shouldMatch` expectedModifiedFeatures

    enableFeatures :: Value -> String -> [String] -> App ()
    enableFeatures owner tid features = do
      for_ features $ \name -> do
        Internal.setTeamFeatureLockStatus owner tid name "unlocked"
        assertSuccess =<< Internal.setTeamFeatureStatus owner tid name "enabled"

    mkExpectedModifiedFeatures :: Value -> App Value
    mkExpectedModifiedFeatures features =
      foldl (flip update) (pure features) unlockableFeatures
      where
        update feat =
          setField (feat <> ".status") "enabled"
            >=> setField (feat <> ".lockStatus") "unlocked"

    conf :: String -> Bool -> ServiceOverrides
    conf db runMigration =
      def
        { galleyCfg = setField "postgresMigration.teamFeatures" db,
          backgroundWorkerCfg = setField "migrateTeamFeatures" runMigration
        }

counterName :: String
counterName = "^wire_team_features_migration_finished"
