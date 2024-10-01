module Test.FeatureFlags.SelfDeletingMessages where

import qualified Data.Aeson.Types as A
import Test.FeatureFlags.Util
import Testlib.Prelude

feature :: (ToJSON timeout) => [A.Pair] -> timeout -> Value
feature ps timeout =
  object
    ( ps
        <> [ "ttl" .= "unlimited",
             "config" .= object ["enforcedTimeoutSeconds" .= toJSON timeout]
           ]
    )

testSelfDeletingMessages :: (HasCallStack) => APIAccess -> App ()
testSelfDeletingMessages access =
  mkFeatureTests "selfDeletingMessages"
    & addUpdate (feature ["status" .= "disabled"] (0 :: Int))
    & addUpdate (feature ["status" .= "enabled"] (30 :: Int))
    & addInvalidUpdate (feature ["status" .= "enabled"] "")
    & runFeatureTests OwnDomain access

testPatchSelfDeletingMessages :: (HasCallStack) => App ()
testPatchSelfDeletingMessages = do
  checkPatch OwnDomain "selfDeletingMessages"
    $ object ["lockStatus" .= "locked"]
  checkPatch OwnDomain "selfDeletingMessages"
    $ object ["status" .= "disabled"]
  checkPatch OwnDomain "selfDeletingMessages"
    $ object ["lockStatus" .= "locked", "status" .= "disabled"]
  checkPatch OwnDomain "selfDeletingMessages"
    $ object ["lockStatus" .= "unlocked", "config" .= object ["enforcedTimeoutSeconds" .= A.Number 30]]
  checkPatch OwnDomain "selfDeletingMessages"
    $ object ["config" .= object ["enforcedTimeoutSeconds" .= A.Number 60]]
