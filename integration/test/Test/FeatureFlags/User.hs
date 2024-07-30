module Test.FeatureFlags.User where

import qualified API.BrigInternal as I
import API.Galley
import qualified API.GalleyInternal as I
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

testFeatureConferenceCallingForUser :: App ()
testFeatureConferenceCallingForUser = do
  (alice, tid, _) <- createTeam OwnDomain 0 -- team user
  bob <- randomUser OwnDomain def -- non-team user
  let featureName = "conferenceCalling"

  -- set initial value at the team level
  let initial =
        confCalling
          def
            { status = "enabled",
              sft = toJSON True
            }
  assertSuccess =<< I.setTeamFeatureConfig OwnDomain tid featureName initial

  -- set user value for both users
  for_ [alice, bob] $ \u -> do
    void $ I.putFeatureForUser u featureName (object ["status" .= "disabled"]) >>= getBody 200
    config <- I.getFeatureForUser u featureName >>= getJSON 200
    config %. "status" `shouldMatch` "disabled"

    -- this config is just made up by brig, it does not reflect the actual value
    -- that will be returned to the user
    config %. "config.useSFTForOneToOneCalls" `shouldMatch` False

  -- alice
  do
    features <- getFeaturesForUser alice >>= getJSON 200
    config <- features %. featureName
    -- alice is a team user, so her config reflects that of the team
    config %. "status" `shouldMatch` "enabled"
    config %. "config.useSFTForOneToOneCalls" `shouldMatch` True

  do
    void $ I.deleteFeatureForUser alice featureName >>= getBody 200
    features <- getFeaturesForUser alice >>= getJSON 200
    config <- features %. featureName
    config %. "status" `shouldMatch` "enabled"
    config %. "config.useSFTForOneToOneCalls" `shouldMatch` True

  -- bob
  do
    features <- getFeaturesForUser bob >>= getJSON 200
    config <- features %. featureName
    -- bob is not in a team, so we get his own personal settings here
    config %. "status" `shouldMatch` "disabled"
    -- but only for status, config is the server defaults
    config %. "config.useSFTForOneToOneCalls" `shouldMatch` False
  do
    void $ I.deleteFeatureForUser bob featureName >>= getBody 200
    features <- getFeaturesForUser bob >>= getJSON 200
    config <- features %. featureName
    config %. "status" `shouldMatch` "enabled"
    config %. "config.useSFTForOneToOneCalls" `shouldMatch` False
