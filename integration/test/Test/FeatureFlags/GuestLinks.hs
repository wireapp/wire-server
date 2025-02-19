module Test.FeatureFlags.GuestLinks where

import Test.FeatureFlags.Util
import Testlib.Prelude

testConversationGuestLinks :: (HasCallStack) => FeatureTable -> APIAccess -> App ()
testConversationGuestLinks table access =
  mkFeatureTests "conversationGuestLinks"
    & addUpdate disabled
    & addUpdate enabled
    & setTable table
    & runFeatureTests OwnDomain access

testPatchGuestLinks :: (HasCallStack) => FeatureTable -> App ()
testPatchGuestLinks table = checkPatchWithTable table OwnDomain "conversationGuestLinks" disabled

testConversationGuestLinksReadOnly :: (HasCallStack) => APIAccess -> App ()
testConversationGuestLinksReadOnly access =
  runFeatureTestsReadOnly OwnDomain access
    $ mkFeatureTests "conversationGuestLinks"
    & addUpdate disabled
    & addUpdate enabled

testPatchGuestLinksReadOnly :: (HasCallStack) => App ()
testPatchGuestLinksReadOnly = checkPatchReadOnly OwnDomain "conversationGuestLinks" disabled
