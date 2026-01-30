module Test.Migration.TeamFeatures where

import qualified API.Galley as Public
import qualified API.GalleyInternal as Internal
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers
import Test.FeatureFlags.Util
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
        domain = backend.berDomain

    (teams0, teams1) <-
      preMigration $ do
        teams0 <- replicateM 3 $ createTeam domain 2
        teams1@(team1 : _) <- replicateM 5 $ createTeam domain 1
        for_ teams0 $ \(owner, tid, _) -> enableFeatures owner tid unlockableFeatures
        testSetFeatures team1
        testGetFeatures team1
        pure (teams0, teams1)

    team1 : team2 : team3 : team4 : team5 : _ <- pure teams1

    switchToMigratingInterpreter $ do
      assertModifiedFeatures domain teams0
      testSetFeatures team2
      testGetFeatures team1
      testGetFeatures team2

    startMigration $ do
      assertModifiedFeatures domain teams0
      testSetFeatures team3
      testGetFeatures team1
      testGetFeatures team2
      testGetFeatures team3
      waitForMigration domain counterName

    stopMigration $ do
      assertModifiedFeatures domain teams0
      testSetFeatures team4
      testGetFeatures team1
      testGetFeatures team2
      testGetFeatures team3
      testGetFeatures team4

    switchToPostgresInterpreter $ do
      assertModifiedFeatures domain teams0
      testSetFeatures team5
      testGetFeatures team1
      testGetFeatures team2
      testGetFeatures team3
      testGetFeatures team4
      testGetFeatures team5
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

    assertModifiedFeatures :: String -> [(Value, String, [Value])] -> App ()
    assertModifiedFeatures domain teams = do
      expectedModifiedFeatures <-
        mkExpectedModifiedFeatures defAllFeatures
          >>= setField "classifiedDomains.config.domains" [domain]
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

    testSetFeatures :: (HasCallStack) => (Value, String, [Value]) -> App ()
    testSetFeatures (owner, tid, _) = do
      Internal.setTeamFeatureLockStatus owner tid "channels" "unlocked"
      Internal.setTeamFeatureLockStatus owner tid "enforceFileDownloadLocation" "unlocked"
      assertSuccess =<< Internal.setTeamFeatureConfig owner tid "channels" channelsConfig
      assertSuccess =<< Internal.setTeamFeatureConfig owner tid "enforceFileDownloadLocation" enforceDownloadLocationConfig
      where
        channelsConfig :: Value
        channelsConfig =
          object
            [ "status" .= "enabled",
              "config"
                .= object
                  [ "allowed_to_create_channels" .= "team-members",
                    "allowed_to_open_channels" .= "admins"
                  ]
            ]

        enforceDownloadLocationConfig :: Value
        enforceDownloadLocationConfig =
          object
            [ "status" .= "enabled",
              "config" .= object ["enforcedDownloadLocation" .= "/tmp/migration-test"]
            ]

    testGetFeatures :: (HasCallStack) => (Value, String, [Value]) -> App ()
    testGetFeatures (owner, tid, _) = do
      expectedChannels <- expectedChannelsConfig
      expectedDownloadLocation <- expectedEnforceDownloadLocationConfig
      bindResponse (Public.getTeamFeature owner tid "channels") $ \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json `shouldMatch` expectedChannels
      bindResponse (Public.getTeamFeature owner tid "enforceFileDownloadLocation") $ \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json `shouldMatch` expectedDownloadLocation
      where
        expectedChannelsConfig :: App Value
        expectedChannelsConfig = do
          defChannels <- defAllFeatures %. "channels"
          defChannels
            & setField "lockStatus" "unlocked"
              >>= setField "status" "enabled"
              >>= setField "config.allowed_to_create_channels" "team-members"
              >>= setField "config.allowed_to_open_channels" "admins"

        expectedEnforceDownloadLocationConfig :: App Value
        expectedEnforceDownloadLocationConfig = do
          defFeature <- defAllFeatures %. "enforceFileDownloadLocation"
          defFeature
            & setField "lockStatus" "unlocked"
              >>= setField "status" "enabled"
              >>= setField "config.enforcedDownloadLocation" "/tmp/migration-test"

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
