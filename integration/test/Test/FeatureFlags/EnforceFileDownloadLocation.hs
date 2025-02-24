module Test.FeatureFlags.EnforceFileDownloadLocation where

import qualified API.GalleyInternal as Internal
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchEnforceFileDownloadLocation :: (HasCallStack) => FeatureTable -> App ()
testPatchEnforceFileDownloadLocation table = do
  checkPatchWithTable table OwnDomain "enforceFileDownloadLocation"
    $ object ["lockStatus" .= "unlocked"]
  checkPatchWithTable table OwnDomain "enforceFileDownloadLocation"
    $ object ["status" .= "enabled"]
  checkPatchWithTable table OwnDomain "enforceFileDownloadLocation"
    $ object ["lockStatus" .= "unlocked", "status" .= "enabled"]
  checkPatchWithTable table OwnDomain "enforceFileDownloadLocation"
    $ object ["lockStatus" .= "locked", "config" .= object []]
  checkPatchWithTable table OwnDomain "enforceFileDownloadLocation"
    $ object ["config" .= object ["enforcedDownloadLocation" .= "/tmp"]]

  do
    (user, tid, _) <- createTeam OwnDomain 0
    updateMigrationState OwnDomain tid table
    bindResponse
      ( Internal.patchTeamFeature
          user
          tid
          "enforceFileDownloadLocation"
          (object ["config" .= object ["enforcedDownloadLocation" .= ""]])
      )
      $ \resp -> do
        resp.status `shouldMatchInt` 400
        resp.json %. "label" `shouldMatch` "empty-download-location"

testEnforceDownloadLocation :: (HasCallStack) => FeatureTable -> APIAccess -> App ()
testEnforceDownloadLocation table access = do
  mkFeatureTests
    "enforceFileDownloadLocation"
    & addUpdate
      ( object
          [ "status" .= "enabled",
            "config" .= object ["enforcedDownloadLocation" .= "/tmp"]
          ]
      )
    & addUpdate
      (object ["status" .= "disabled", "config" .= object []])
    & addInvalidUpdate
      ( object
          [ "status" .= "enabled",
            "config"
              .= object
                [ "enforcedDownloadLocation" .= object []
                ]
          ]
      )
    & setTable table
    & runFeatureTests OwnDomain access
