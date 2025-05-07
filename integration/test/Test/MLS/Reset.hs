module Test.MLS.Reset where

import API.Galley
import MLS.Util
import SetupHelpers
import Testlib.Prelude

testResetGroupConversation :: (HasCallStack) => Domain -> App ()
testResetGroupConversation domain = do
  own <- make OwnDomain
  other <- make domain
  [alice, bob, charlie] <- createAndConnectUsers [own, other, own]
  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
  void $ uploadNewKeyPackage def bob1
  conv <- createNewGroup def alice1
  void $ createAddCommit alice1 conv [bob] >>= sendAndConsumeCommitBundle
  mlsConv <- getMLSConv conv

  bindResponse (resetConversation alice mlsConv.groupId 0) $ \resp -> do
    resp.status `shouldMatchInt` 409

  bindResponse (resetConversation charlie mlsConv.groupId 0) $ \resp -> do
    resp.status `shouldMatchInt` 403

  bindResponse (resetConversation bob mlsConv.groupId mlsConv.epoch) $ \resp -> do
    resp.status `shouldMatchInt` 200
  conv' <- getConversation alice conv >>= getJSON 200
  conv' %. "group_id" `shouldNotMatch` (mlsConv.groupId :: String)
  conv' %. "epoch" `shouldMatchInt` 0
  otherMember <- assertOne =<< asList (conv' %. "members.others")
  otherMember %. "qualified_id" `shouldMatch` (bob %. "qualified_id")
