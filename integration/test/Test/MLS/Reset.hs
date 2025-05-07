module Test.MLS.Reset where

import API.Galley
import MLS.Util
import SetupHelpers
import Testlib.Prelude

testResetGroupConversation :: (HasCallStack) => Domain -> App ()
testResetGroupConversation domain = do
  [alice, bob, charlie] <- createAndConnectUsers [make OwnDomain, make domain, make OwnDomain]
  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
  void $ uploadNewKeyPackage def bob1
  conv <- createNewGroup def alice1
  void $ createAddCommit alice1 conv [bob] >>= sendAndConsumeCommitBundle
  mlsConv <- getMLSConv conv

  resetConversation alice mlsConv.groupId 0 >>= assertStatus 409
  resetConversation bob mlsConv.groupId 0 >>= assertStatus 409
  resetConversation charlie mlsConv.groupId mlsConv.epoch >>= assertStatus 403
  resetConversation bob mlsConv.groupId mlsConv.epoch >>= assertStatus 200

  conv' <- getConversation alice conv >>= getJSON 200
  conv' %. "group_id" `shouldNotMatch` (mlsConv.groupId :: String)
  conv' %. "epoch" `shouldMatchInt` 0
  otherMember <- assertOne =<< asList (conv' %. "members.others")
  otherMember %. "qualified_id" `shouldMatch` (bob %. "qualified_id")
