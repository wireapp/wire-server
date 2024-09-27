module Test.FeatureFlags.ConferenceCalling where

import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchConferenceCalling :: (HasCallStack) => App ()
testPatchConferenceCalling = do
  let defCfg = confCalling def {lockStatus = Just "locked"}
  for_
    [ object ["lockStatus" .= "locked"],
      object ["status" .= "disabled"],
      object ["lockStatus" .= "locked", "status" .= "disabled"],
      object
        [ "lockStatus" .= "unlocked",
          "config" .= object ["useSFTForOneToOneCalls" .= toJSON True]
        ]
    ]
    $ \patch ->
      checkPatch OwnDomain "conferenceCalling" True defCfg patch

testConferenceCalling :: (HasCallStack) => APIAccess -> App ()
testConferenceCalling access = do
  runFeatureTests OwnDomain access
    $ mkFeatureTests "conferenceCalling"
    & addUpdate (confCalling def {sft = toJSON True})
    & addUpdate (confCalling def {sft = toJSON False})
    & addInvalidUpdate (confCalling def {sft = toJSON (0 :: Int)})
