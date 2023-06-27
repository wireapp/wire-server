module Test.MLS.One2One where

import API.Galley
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as B8
import MLS.Util
import SetupHelpers
import Testlib.Prelude

testGetMLSOne2One :: HasCallStack => Domain -> App ()
testGetMLSOne2One otherDomain = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, otherDomain]

  conv <- getMLSOne2OneConversation alice bob >>= getJSON 200
  conv %. "type" `shouldMatchInt` 2
  shouldBeEmpty (conv %. "members.others")

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

data One2OneScenario
  = -- | Both users are local
    One2OneScenarioLocal
  | -- | One user is remote, conversation is local
    One2OneScenarioLocalConv
  | -- | One user is remote, conversation is remote
    One2OneScenarioRemoteConv

instance HasTests x => HasTests (One2OneScenario -> x) where
  mkTests m n s f x =
    mkTests m (n <> "[domain=own]") s f (x One2OneScenarioLocal)
      <> mkTests m (n <> "[domain=other;conv=own]") s f (x One2OneScenarioLocalConv)
      <> mkTests m (n <> "[domain=other;conv=other]") s f (x One2OneScenarioRemoteConv)

one2OneScenarioDomain :: One2OneScenario -> Domain
one2OneScenarioDomain One2OneScenarioLocal = OwnDomain
one2OneScenarioDomain _ = OtherDomain

one2OneScenarioConvDomain :: One2OneScenario -> Domain
one2OneScenarioConvDomain One2OneScenarioLocal = OwnDomain
one2OneScenarioConvDomain One2OneScenarioLocalConv = OwnDomain
one2OneScenarioConvDomain One2OneScenarioRemoteConv = OtherDomain

testMLSOne2One :: HasCallStack => One2OneScenario -> App ()
testMLSOne2One scenario = do
  alice <- randomUser OwnDomain def
  let otherDomain = one2OneScenarioDomain scenario
      convDomain = one2OneScenarioConvDomain scenario
  bob <- createMLSOne2OnePartner otherDomain alice convDomain
  [alice1, bob1] <- traverse createMLSClient [alice, bob]
  traverse_ uploadNewKeyPackage [bob1]

  conv <- getMLSOne2OneConversation alice bob >>= getJSON 200
  resetGroup alice1 conv

  -- TODO: check that bob receives a welcome message
  void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle

  withWebSocket bob1 $ \ws -> do
    mp <- createApplicationMessage alice1 "hello, world"
    void $ sendAndConsumeMessage mp
    let isMessage n = nPayload n %. "type" `isEqual` "conversation.mls-message-add"
    n <- awaitMatch 3 isMessage ws
    nPayload n %. "data" `shouldMatch` B8.unpack (Base64.encode (mp.message))
