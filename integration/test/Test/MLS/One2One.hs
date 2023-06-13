module Test.MLS.One2One where

import API.Galley
import SetupHelpers
import Testlib.Prelude

testGetMLSOne2One :: HasCallStack => Domain -> App ()
testGetMLSOne2One otherDomain = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, otherDomain]

  conv <- getMLSOne2OneConversation alice bob >>= getJSON 200

  conv %. "type" `shouldMatchInt` 2
  others <- conv %. "members.others" & asList
  other <- assertOne others
  other %. "conversation_role" `shouldMatch` "wire_member"
  other %. "qualified_id" `shouldMatch` (bob %. "qualified_id")

  conv %. "members.self.conversation_role" `shouldMatch` "wire_member"
  conv %. "members.self.qualified_id" `shouldMatch` (alice %. "qualified_id")

  convId <- conv %. "qualified_id"

  -- check that the conversation has the same ID on the other side
  conv2 <- bindResponse (getMLSOne2OneConversation bob alice) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json

  conv2 %. "type" `shouldMatchInt` 2
  conv2 %. "qualified_id" `shouldMatch` convId
  conv2 %. "epoch" `shouldMatch` (conv %. "epoch")

testGetMLSOne2OneUnconnected :: HasCallStack => Domain -> App ()
testGetMLSOne2OneUnconnected otherDomain = do
  [alice, bob] <- for [OwnDomain, otherDomain] $ \domain -> randomUser domain def

  bindResponse (getMLSOne2OneConversation alice bob) $ \resp ->
    resp.status `shouldMatchInt` 403

testGetMLSOne2OneSameTeam :: App ()
testGetMLSOne2OneSameTeam = do
  (alice, _) <- createTeam OwnDomain
  bob <- addUserToTeam alice
  void $ getMLSOne2OneConversation alice bob >>= getJSON 200
