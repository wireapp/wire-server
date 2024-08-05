module Test.FeatureFlags.User where

import qualified API.BrigInternal as I
import API.Galley
import qualified API.GalleyInternal as I
import SetupHelpers
import Testlib.Prelude

testFeatureConferenceCallingForUser :: App ()
testFeatureConferenceCallingForUser = do
  (alice, tid, _) <- createTeam OwnDomain 0 -- team user
  bob <- randomUser OwnDomain def -- non-team user
  let featureName = "conferenceCalling"

  -- set initial value at the team level
  let patch =
        object
          [ "lockStatus" .= "unlocked",
            "status" .= "enabled",
            "config" .= object ["useSFTForOneToOneCalls" .= True]
          ]

  assertSuccess =<< I.patchTeamFeatureConfig OwnDomain tid featureName patch

  -- set user value for both users
  for_ [alice, bob] $ \u -> do
    void
      $ I.putFeatureForUser
        u
        featureName
        ( object
            [ "status" .= "disabled"
            ]
        )
      >>= getBody 200
    I.getFeatureForUser u featureName `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      config <- resp.json
      config %. "status" `shouldMatch` "disabled"

      -- this config is just made up by brig, it does not reflect the actual value
      -- that will be returned to the user
      config %. "config.useSFTForOneToOneCalls" `shouldMatch` False

  -- alice
  do
    getFeaturesForUser alice `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      config <- resp.json %. featureName

      -- alice is a team user, so her config reflects that of the team
      config %. "status" `shouldMatch` "enabled"
      config %. "config.useSFTForOneToOneCalls" `shouldMatch` True

  do
    void $ I.deleteFeatureForUser alice featureName >>= getBody 200
    getFeaturesForUser alice `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      config <- resp.json %. featureName
      config %. "status" `shouldMatch` "enabled"
      config %. "config.useSFTForOneToOneCalls" `shouldMatch` True

  -- bob
  do
    getFeaturesForUser bob `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      config <- resp.json %. featureName
      -- bob is not in a team, so we get his own personal settings here
      config %. "status" `shouldMatch` "disabled"
      -- but only for status, config is the server defaults
      config %. "config.useSFTForOneToOneCalls" `shouldMatch` False
  do
    void $ I.deleteFeatureForUser bob featureName >>= getBody 200
    getFeaturesForUser bob `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      config <- resp.json %. featureName
      config %. "status" `shouldMatch` "disabled"
      config %. "config.useSFTForOneToOneCalls" `shouldMatch` False
