module Test.FeatureFlags.ConferenceCalling where

import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchConferenceCalling :: (HasCallStack) => App ()
testPatchConferenceCalling = do
  checkPatch OwnDomain "conferenceCalling"
    $ object ["lockStatus" .= "locked"]
  checkPatch OwnDomain "conferenceCalling"
    $ object ["status" .= "disabled"]
  checkPatch OwnDomain "conferenceCalling"
    $ object ["lockStatus" .= "locked", "status" .= "disabled"]
  checkPatch OwnDomain "conferenceCalling"
    $ object
      [ "lockStatus" .= "unlocked",
        "config" .= object ["useSFTForOneToOneCalls" .= toJSON True]
      ]

testConferenceCalling :: (HasCallStack) => APIAccess -> App ()
testConferenceCalling access = do
  runFeatureTests OwnDomain access
    $ mkFeatureTests "conferenceCalling"
    & addUpdate (confCalling def {sft = toJSON True})
    & addUpdate (confCalling def {sft = toJSON False})
    & addInvalidUpdate (confCalling def {sft = toJSON (0 :: Int)})

testPatchConferenceCallingReadOnly :: (HasCallStack) => App ()
testPatchConferenceCallingReadOnly = do
  checkPatchReadOnly OwnDomain "conferenceCalling"
    $ object ["lockStatus" .= "locked"]

testConferenceCallingReadOnlyDuringMigration :: (HasCallStack) => APIAccess -> App ()
testConferenceCallingReadOnlyDuringMigration access = do
  runFeatureTestsReadOnly OwnDomain access
    $ mkFeatureTests "conferenceCalling"
    & addUpdate (confCalling def {sft = toJSON True})
    & addUpdate (confCalling def {sft = toJSON False})
