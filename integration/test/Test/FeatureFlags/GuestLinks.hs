module Test.FeatureFlags.GuestLinks where

import Test.FeatureFlags.Util
import Testlib.Prelude

testConversationGuestLinks :: (HasCallStack) => APIAccess -> App ()
testConversationGuestLinks access =
  mkFeatureTests "conversationGuestLinks"
    & addUpdate disabled
    & addUpdate enabled
    & runFeatureTests OwnDomain access

testPatchGuestLinks :: (HasCallStack) => App ()
testPatchGuestLinks = checkPatch OwnDomain "conversationGuestLinks" True enabled disabled
