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

testSelfDeletingMessages :: (HasCallStack) => FeatureTable -> APIAccess -> App ()
testSelfDeletingMessages table access =
  mkFeatureTests "selfDeletingMessages"
    & addUpdate (feature ["status" .= "disabled"] (0 :: Int))
    & addUpdate (feature ["status" .= "enabled"] (30 :: Int))
    & addInvalidUpdate (feature ["status" .= "enabled"] "")
    & setTable table
    & runFeatureTests OwnDomain access

testPatchSelfDeletingMessages :: (HasCallStack) => FeatureTable -> App ()
testPatchSelfDeletingMessages table = do
  checkPatchWithTable table OwnDomain "selfDeletingMessages"
    $ object ["lockStatus" .= "locked"]
  checkPatchWithTable table OwnDomain "selfDeletingMessages"
    $ object ["status" .= "disabled"]
  checkPatchWithTable table OwnDomain "selfDeletingMessages"
    $ object ["lockStatus" .= "locked", "status" .= "disabled"]
  checkPatchWithTable table OwnDomain "selfDeletingMessages"
    $ object ["lockStatus" .= "unlocked", "config" .= object ["enforcedTimeoutSeconds" .= A.Number 30]]
  checkPatchWithTable table OwnDomain "selfDeletingMessages"
    $ object ["config" .= object ["enforcedTimeoutSeconds" .= A.Number 60]]
