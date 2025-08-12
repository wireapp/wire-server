module Test.FeatureFlags.ChatBubbles where

import Test.FeatureFlags.Util
import Testlib.Prelude

testConversationChatBubbles :: (HasCallStack) => APIAccess -> App ()
testConversationChatBubbles access =
  mkFeatureTests "chatBubbles"
    & addUpdate enabled
    & addUpdate disabled
    & runFeatureTests OwnDomain access

testPatchChatBubbles :: (HasCallStack) => App ()
testPatchChatBubbles = checkPatch OwnDomain "chatBubbles" disabled
