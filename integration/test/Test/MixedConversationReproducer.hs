{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Test.MixedConversationReproducer where

import API.Brig (addClient)
import API.Galley
import Control.Lens ((.~), (^?!))
import qualified Data.Aeson as A
import qualified Data.ProtoLens as Proto
import Data.ProtoLens.Labels ()
import MLS.Util (createMLSClient)
import Notifications
import Numeric.Lens (hex)
import qualified Proto.Otr as Proto
import qualified Proto.Otr_Fields as Proto
import SetupHelpers
import Testlib.Prelude

-- copied from Test.FeatureFlags.MlsMigration to keep this reproducer self-contained
mlsEnableConfig :: Value
mlsEnableConfig =
  object
    [ "protocolToggleUsers" .= ([] :: [String]),
      "defaultProtocol" .= "mls",
      "supportedProtocols" .= ["mls"],
      "allowedCipherSuites" .= ([1] :: [Int]),
      "defaultCipherSuite" .= A.Number 1
    ]

mlsEnable :: Value
mlsEnable =
  object
    [ "status" .= "enabled",
      "config" .= mlsEnableConfig
    ]

-- | Reproducer for bug-report.md: "Not receiving messages in mixed and
-- migrated groups with MLS protocol if hosted on another backend".
--
-- Steps, following bug-report.md:
--   1. Team A on backend A: user A (admin), user C (no MLS device)
--   2. Team B on backend B: user B (admin)
--   3. Connect A<->B, C<->B
--   4. User B creates a *proteus* group with A and C
--   5. Enable MLS for team A and team B
--   6. Refresh/create clients for A and B (C intentionally stays without an
--      MLS client, so the group cannot fully migrate to MLS)
--   7. Start migration for team B (protocol -> mixed), don't finalise
--   8. Start migration for team A too, don't finalise
--   9. Send a message as B, assert A receives it
--  10. Send a message as A, assert B receives it
--
-- Per bug-report.md's "Additional Notes", step 10 is where the asymmetry
-- is expected to show: B stops receiving messages sent by A in this
-- conversation, even though A keeps receiving messages sent by B.
testFederatedMixedProtocolMessageDelivery :: (HasCallStack) => Domain -> App ()
testFederatedMixedProtocolMessageDelivery secondDomain = do
  -- Step 1: Team A on backend A with user A (admin) and user C
  (userA, tidA, _) <- createTeam OwnDomain 1
  userC <- randomUser OwnDomain def

  -- Step 2: Team B on backend B with user B (admin)
  (userB, tidB, _) <- createTeam secondDomain 1

  -- Step 3: connect everyone
  connectUsers [userA, userB, userC]

  -- legacy proteus clients, used to send/receive proteus messages
  clientA <- addClient userA def >>= getJSON 201 >>= objId
  clientB <- addClient userB def >>= getJSON 201 >>= objId
  clientC <- addClient userC def >>= getJSON 201 >>= objId

  -- Step 4: User B creates a proteus group with A and C
  convId <-
    postConversation
      userB
      defProteus {qualifiedUsers = [userA, userC], team = Just tidB}
      >>= getJSON 201
      >>= objConvId

  bindResponse (getConversation userB (convIdToQidObject convId)) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "protocol" `shouldMatch` "proteus"

  -- Step 5: enable the MLS team feature for both teams
  void $ setTeamFeatureConfig userA tidA "mls" mlsEnable >>= getJSON 200
  void $ setTeamFeatureConfig userB tidB "mls" mlsEnable >>= getJSON 200

  -- Step 6: refresh/create MLS clients for A and B; C stays without one so
  -- the conversation cannot fully migrate to MLS and remains mixed.
  void $ createMLSClient def userA
  void $ createMLSClient def userB

  -- Step 7: start migration for team B (creator/owner of the conversation)
  bindResponse (putConversationProtocol userB convId "mixed") $ \resp ->
    resp.status `shouldMatchInt` 200

  -- Step 8: start migration for team A too, without finalising
  bindResponse (putConversationProtocol userA convId "mixed") $ \resp ->
    resp.status `shouldMatchOneOf` [Number 200, Number 204]

  -- The conversation should now be mixed protocol from both sides
  bindResponse (getConversation userA (convIdToQidObject convId)) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "protocol" `shouldMatch` "mixed"
  bindResponse (getConversation userB (convIdToQidObject convId)) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "protocol" `shouldMatch` "mixed"

  -- Step 9: B sends a message; A should receive it
  -- userA and userC are both on OwnDomain, so they share one recipient entry.
  withWebSocket userA $ \wsA -> do
    msgFromB <-
      mkProteusRecipients
        userA
        [(userA, [clientA]), (userC, [clientC])]
        "message from B"
    let protoMsgFromB =
          Proto.defMessage @Proto.QualifiedNewOtrMessage
            & #sender . Proto.client .~ (clientB ^?! hex)
            & #recipients .~ [msgFromB]
            & #ignoreAll .~ Proto.defMessage
    bindResponse (postProteusMessage userB (convIdToQidObject convId) protoMsgFromB) $ \resp ->
      resp.status `shouldMatchInt` 201

    n <- awaitMatch isNewMessageNotif wsA
    n %. "payload.0.qualified_conversation" `shouldMatch` convIdToQidObject convId

  -- Step 10: A sends a message; B should receive it -- expected failure
  -- point for the bug being reproduced.
  -- userB is on secondDomain, userC is on OwnDomain -- different domains,
  -- so each needs its own qualified recipient entry.
  withWebSocket userB $ \wsB -> do
    msgFromAToB <- mkProteusRecipients userB [(userB, [clientB])] "message from A"
    msgFromAToC <- mkProteusRecipients userC [(userC, [clientC])] "message from A"
    let protoMsgFromA =
          Proto.defMessage @Proto.QualifiedNewOtrMessage
            & #sender . Proto.client .~ (clientA ^?! hex)
            & #recipients .~ [msgFromAToB, msgFromAToC]
            & #ignoreAll .~ Proto.defMessage
    bindResponse (postProteusMessage userA (convIdToQidObject convId) protoMsgFromA) $ \resp ->
      resp.status `shouldMatchInt` 201

    -- If the bug is present, this awaitMatch times out, proving the
    -- asymmetric delivery failure described in bug-report.md.
    n <- awaitMatch isNewMessageNotif wsB
    n %. "payload.0.qualified_conversation" `shouldMatch` convIdToQidObject convId
