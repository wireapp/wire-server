-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module Test.LegalHold where

import API.Brig
import API.BrigCommon
import qualified API.BrigInternal as BrigI
import API.Common
import API.Galley
import API.GalleyInternal
import Control.Lens ((.~), (^?!))
import qualified Data.Map as Map
import qualified Data.ProtoLens as Proto
import Data.ProtoLens.Labels ()
import qualified Data.Set as Set
import GHC.Stack
import Numeric.Lens (hex)
import qualified Proto.Otr as Proto
import qualified Proto.Otr_Fields as Proto
import SetupHelpers
import Testlib.MockIntegrationService
import Testlib.Prekeys
import Testlib.Prelude

testLHPreventAddingNonConsentingUsers :: App ()
testLHPreventAddingNonConsentingUsers = do
  startDynamicBackends [mempty] $ \[dom] -> do
    withMockServer lhMockApp $ \lhPort _chan -> do
      (owner, tid, [alice, alex]) <- createTeam dom 3

      void $ legalholdWhitelistTeam owner tid >>= assertSuccess
      void $ legalholdIsTeamInWhitelist owner tid >>= assertSuccess
      void $ postLegalHoldSettings owner tid (mkLegalHoldSettings lhPort) >>= getJSON 201

      george <- randomUser dom def
      georgeQId <- george %. "qualified_id"
      connectUsers =<< forM [alice, george] make
      connectUsers =<< forM [alex, george] make
      conv <- postConversation alice (defProteus {qualifiedUsers = [alex], team = Just tid}) >>= getJSON 201

      -- the guest should be added to the conversation
      bindResponse (addMembers alice conv def {users = [georgeQId]}) $ \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "type" `shouldMatch` "conversation.member-join"

      -- assert that the guest is in the conversation
      checkConvHasOtherMembers conv alice [alex, george]

      -- now request legalhold for alex (but not alice)
      requestLegalHoldDevice tid owner alex >>= assertSuccess

      -- the guest should be removed from the conversation
      checkConvHasOtherMembers conv alice [alex]

      -- it should not be possible neither for alex nor for alice to add the guest back
      bindResponse (addMembers alex conv def {users = [georgeQId]}) $ \resp -> do
        resp.status `shouldMatchInt` 403
        resp.json %. "label" `shouldMatch` "not-connected"

      bindResponse (addMembers alice conv def {users = [georgeQId]}) $ \resp -> do
        resp.status `shouldMatchInt` 403
        resp.json %. "label" `shouldMatch` "missing-legalhold-consent"
  where
    checkConvHasOtherMembers :: HasCallStack => Value -> Value -> [Value] -> App ()
    checkConvHasOtherMembers conv u us =
      bindResponse (getConversation u conv) $ \resp -> do
        resp.status `shouldMatchInt` 200
        mems <-
          resp.json %. "members.others" & asList >>= traverse \m -> do
            m %. "qualified_id"
        mems `shouldMatchSet` forM us (\m -> m %. "qualified_id")

testLHMessageExchange ::
  HasCallStack =>
  TaggedBool "clients1New" ->
  TaggedBool "clients2New" ->
  TaggedBool "consentFrom1" ->
  TaggedBool "consentFrom2" ->
  App ()
testLHMessageExchange (TaggedBool clients1New) (TaggedBool clients2New) (TaggedBool consentFrom1) (TaggedBool consentFrom2) = do
  startDynamicBackends [mempty] $ \[dom] -> do
    withMockServer lhMockApp $ \lhPort _chan -> do
      (owner, tid, [mem1, mem2]) <- createTeam dom 3

      let clientSettings :: Bool -> AddClient
          clientSettings allnew =
            if allnew
              then def -- (`{acapabilities = Just ["legalhold-implicit-consent"]}` is the default)
              else def {acapabilities = Nothing}
      client1 <- objId $ addClient (mem1 %. "qualified_id") (clientSettings clients1New) >>= getJSON 201
      _client2 <- objId $ addClient (mem2 %. "qualified_id") (clientSettings clients2New) >>= getJSON 201

      void $ legalholdWhitelistTeam owner tid >>= assertSuccess
      void $ legalholdIsTeamInWhitelist owner tid >>= assertSuccess
      void $ postLegalHoldSettings owner tid (mkLegalHoldSettings lhPort) >>= getJSON 201

      conv <- postConversation mem1 (defProteus {qualifiedUsers = [mem2], team = Just tid}) >>= getJSON 201

      requestLegalHoldDevice tid owner mem1 >>= assertSuccess
      requestLegalHoldDevice tid owner mem2 >>= assertSuccess
      when consentFrom1 $ do
        approveLegalHoldDevice tid (mem1 %. "qualified_id") defPassword >>= assertSuccess
      when consentFrom2 $ do
        approveLegalHoldDevice tid (mem2 %. "qualified_id") defPassword >>= assertSuccess

      let getCls :: Value -> App [String]
          getCls mem = do
            res <- getClientsQualified mem dom mem
            val <- getJSON 200 res
            cls <- asList val
            objId `mapM` cls
      cs1 :: [String] <- getCls mem1 -- it's ok to include the sender, backend will filter it out.
      cs2 :: [String] <- getCls mem2

      length cs1 `shouldMatchInt` if consentFrom1 then 2 else 1
      length cs2 `shouldMatchInt` if consentFrom2 then 2 else 1

      void $ do
        successfulMsgForOtherUsers <- mkProteusRecipients mem1 [(mem1, cs1), (mem2, cs2)] "hey there"
        let successfulMsg =
              Proto.defMessage @Proto.QualifiedNewOtrMessage
                & #sender . Proto.client .~ (client1 ^?! hex)
                & #recipients .~ [successfulMsgForOtherUsers]
                & #reportAll .~ Proto.defMessage
        bindResponse (postProteusMessage mem1 (conv %. "qualified_id") successfulMsg) $ \resp -> do
          let check :: HasCallStack => Int -> Maybe String -> App ()
              check status Nothing = do
                resp.status `shouldMatchInt` status
              check status (Just label) = do
                resp.status `shouldMatchInt` status
                resp.json %. "label" `shouldMatch` label

          let -- there are two equally valid ways to write this down (feel free to remove one if it gets in your way):
              _oneWay = case (clients1New, clients2New, consentFrom1, consentFrom2) of
                (_, _, False, False) ->
                  -- no LH in the picture
                  check 201 Nothing
                (True, True, _, _) ->
                  if consentFrom1 /= consentFrom2
                    then -- no old clients, but users disagree on LH
                      check 403 (Just "missing-legalhold-consent")
                    else -- everybody likes LH
                      check 201 Nothing
                _ ->
                  -- everything else
                  check 403 (Just "missing-legalhold-consent-old-clients")

              theOtherWay = case (clients1New, clients2New, consentFrom1, consentFrom2) of
                -- NB: "consent" always implies "has an active LH device"
                (False, False, False, False) ->
                  -- no LH in the picture
                  check 201 Nothing
                (False, False, False, True) ->
                  -- sender has old clients and not given consent, recipient has LH device
                  check 403 (Just "missing-legalhold-consent-old-clients")
                (False, False, True, False) ->
                  -- recipient has old clients and not given consent, sender has LH device
                  check 403 (Just "missing-legalhold-consent-old-clients")
                (False, False, True, True) ->
                  -- both sender, recipient have has old clients and LH devices, but given consent
                  check 403 (Just "missing-legalhold-consent-old-clients")
                (False, True, False, False) ->
                  -- no LH in the picture
                  check 201 Nothing
                (False, True, False, True) ->
                  -- sender has old clients and not given consent, recipient has LH device (and only new clients)
                  check 403 (Just "missing-legalhold-consent-old-clients")
                (False, True, True, False) ->
                  -- sender has old clients but given consent and LH device; recipient has not given consent
                  check 403 (Just "missing-legalhold-consent-old-clients")
                (False, True, True, True) ->
                  -- sender has old clients but given consent and LH device; recipient has LH device (and only new clients)
                  check 403 (Just "missing-legalhold-consent-old-clients")
                (True, False, False, False) ->
                  -- no LH in the picture
                  check 201 Nothing
                (True, False, False, True) ->
                  -- recipient has given consent and LH device, but old clients (and sender has not given consent)
                  check 403 (Just "missing-legalhold-consent-old-clients")
                (True, False, True, False) ->
                  -- recipient has old clients and not given consent, sender has LH device
                  check 403 (Just "missing-legalhold-consent-old-clients")
                (True, False, True, True) ->
                  -- old clients with recipient, LH devices by all
                  check 403 (Just "missing-legalhold-consent-old-clients")
                (True, True, False, False) ->
                  -- no LH in the picture
                  check 201 Nothing
                (True, True, False, True) ->
                  -- all clients new, no consent from sender, recipient has LH device
                  check 403 (Just "missing-legalhold-consent")
                (True, True, True, False) ->
                  -- all clients new, no consent from recipient, sender has LH device
                  check 403 (Just "missing-legalhold-consent")
                (True, True, True, True) ->
                  -- everybody happy with LH
                  check 201 Nothing

          -- _oneWay -- run this if you want to make sure both ways are equivalent, but please don't commit!
          theOtherWay

data TestClaimKeys
  = TCKConsentMissing -- (team not whitelisted, that is)
  | TCKOldClient
  | TCKConsentAndNewClients
  deriving (Show, Bounded, Enum)

-- | Cannot fetch prekeys of LH users if requester has not given consent or has old clients.
testLHClaimKeys :: WithBoundedEnumArg TestClaimKeys (App ())
testLHClaimKeys = WithBoundedEnumArg $ \testmode -> do
  startDynamicBackends [mempty] $ \[dom] -> do
    withMockServer lhMockApp $ \lhPort _chan -> do
      (lowner, ltid, [lmem]) <- createTeam dom 2
      (powner, ptid, [pmem]) <- createTeam dom 2

      legalholdWhitelistTeam lowner ltid >>= assertSuccess
      legalholdIsTeamInWhitelist lowner ltid >>= assertSuccess
      void $ postLegalHoldSettings lowner ltid (mkLegalHoldSettings lhPort) >>= getJSON 201

      requestLegalHoldDevice ltid lowner lmem >>= assertSuccess
      approveLegalHoldDevice ltid (lmem %. "qualified_id") defPassword >>= assertSuccess

      let addc caps = addClient pmem (settings caps) >>= assertSuccess
          settings caps =
            def
              { prekeys = Just $ take 10 somePrekeysRendered,
                lastPrekey = Just $ head someLastPrekeysRendered,
                acapabilities = caps
              }
       in case testmode of
            TCKConsentMissing ->
              addc $ Just ["legalhold-implicit-consent"]
            TCKOldClient -> do
              addc Nothing
              void $ legalholdWhitelistTeam powner ptid >>= assertSuccess
              void $ legalholdIsTeamInWhitelist powner ptid >>= assertSuccess
            TCKConsentAndNewClients -> do
              addc $ Just ["legalhold-implicit-consent"]
              void $ legalholdWhitelistTeam powner ptid >>= assertSuccess
              void $ legalholdIsTeamInWhitelist powner ptid >>= assertSuccess

      llhdev :: String <- do
        let getCls :: Value -> App [String]
            getCls mem = do
              res <- getClientsQualified mem dom mem
              val <- getJSON 200 res
              cls <- asList val
              objId `mapM` cls
        getCls lmem <&> \case
          [d] -> d
          bad -> error $ show bad

      let assertResp :: HasCallStack => Response -> App ()
          assertResp resp = case testmode of
            TCKConsentMissing -> do
              resp.status `shouldMatchInt` 403
              resp.json %. "label" `shouldMatch` "missing-legalhold-consent"
            TCKOldClient -> do
              resp.status `shouldMatchInt` 403
              resp.json %. "label" `shouldMatch` "missing-legalhold-consent-old-clients"
            TCKConsentAndNewClients -> do
              resp.status `shouldMatchInt` 200

      bindResponse (getUsersPrekeysClient pmem (lmem %. "qualified_id") llhdev) $ assertResp
      bindResponse (getUsersPrekeyBundle pmem (lmem %. "qualified_id")) $ assertResp

      slmemdom <- asString $ lmem %. "qualified_id.domain"
      slmemid <- asString $ lmem %. "qualified_id.id"
      let userClients = Map.fromList [(slmemdom, Map.fromList [(slmemid, Set.fromList [llhdev])])]
      bindResponse (getMultiUserPrekeyBundle pmem userClients) $ assertResp

testLHAddClientManually :: App ()
testLHAddClientManually = do
  (_owner, _tid, [mem1]) <- createTeam OwnDomain 2
  bindResponse (addClient mem1 def {ctype = "legalhold"}) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "client-error"
    -- we usually don't test the human-readable "message", but in this case it is important to
    -- make sure the reason is the right one, and not eg. "LH service not present", or some
    -- other unspecific client error.
    resp.json %. "message" `shouldMatch` "LegalHold clients cannot be added manually. LegalHold must be enabled on this user by an admin"

testLHDeleteClientManually :: App ()
testLHDeleteClientManually = do
  (_owner, _tid, [mem1]) <- createTeam OwnDomain 2
  cid <- bindResponse (BrigI.addClient mem1 def {ctype = "legalhold"}) $ \resp -> do
    resp.status `shouldMatchInt` 201
    asString =<< resp.json %. "id"
  bindResponse (deleteClient mem1 cid) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "client-error"
    -- we usually don't test the human-readable "message", but in this case it is important to
    -- make sure the reason is the right one, and not eg. "LH service not present", or some
    -- other unspecific client error.
    resp.json %. "message" `shouldMatch` "LegalHold clients cannot be deleted. LegalHold must be disabled on this user by an admin"
