{-# OPTIONS_GHC -Wwarn #-}

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
import API.BrigCommon as BrigC
import qualified API.BrigInternal as BrigI
import API.Common
import API.Galley
import API.GalleyInternal
import Control.Error (MaybeT (MaybeT), runMaybeT)
import Control.Lens ((.~), (^?!))
import Control.Monad.Trans.Class (lift)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.Map as Map
import qualified Data.ProtoLens as Proto
import Data.ProtoLens.Labels ()
import qualified Data.Set as Set
import GHC.Stack
import Notifications (awaitNotification, isUserClientAddNotif, isUserLegalholdEnabledNotif, isUserLegalholdRequestNotif)
import Numeric.Lens (hex)
import qualified Proto.Otr as Proto
import qualified Proto.Otr_Fields as Proto
import SetupHelpers
import Testlib.MockIntegrationService
import Testlib.Prekeys
import Testlib.Prelude
import UnliftIO (Chan, readChan, timeout)

testLHPreventAddingNonConsentingUsers :: App ()
testLHPreventAddingNonConsentingUsers = do
  startDynamicBackends [mempty] $ \[dom] -> do
    withMockServer lhMockApp $ \lhPort _chan -> do
      (owner, tid, [alice, alex]) <- createTeam dom 3

      legalholdWhitelistTeam tid owner >>= assertSuccess
      legalholdIsTeamInWhitelist tid owner >>= assertSuccess
      postLegalHoldSettings tid owner (mkLegalHoldSettings lhPort) >>= assertStatus 201

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
      addMembers alex conv def {users = [georgeQId]}
        >>= assertLabel 403 "not-connected"

      addMembers alice conv def {users = [georgeQId]}
        >>= assertLabel 403 "missing-legalhold-consent"
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

      legalholdWhitelistTeam tid owner >>= assertSuccess
      legalholdIsTeamInWhitelist tid owner >>= assertSuccess
      postLegalHoldSettings tid owner (mkLegalHoldSettings lhPort) >>= assertStatus 201

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

      do
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
                (False, True, False, False) ->
                  -- no LH in the picture
                  check 201 Nothing
                (True, False, False, False) ->
                  -- no LH in the picture
                  check 201 Nothing
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
                _ -> pure ()

          -- _oneWay -- run this if you want to make sure both ways are equivalent, but please don't commit!
          theOtherWay

data TestClaimKeys
  = TCKConsentMissing -- (team not whitelisted, that is)
  | TCKConsentAndNewClients
  deriving (Show, Bounded, Enum)

-- | Cannot fetch prekeys of LH users if requester has not given consent or has old clients.
testLHClaimKeys :: WithBoundedEnumArg TestClaimKeys (App ())
testLHClaimKeys = WithBoundedEnumArg $ \testmode -> do
  startDynamicBackends [mempty] $ \[dom] -> do
    withMockServer lhMockApp $ \lhPort _chan -> do
      (lowner, ltid, [lmem]) <- createTeam dom 2
      (powner, ptid, [pmem]) <- createTeam dom 2

      legalholdWhitelistTeam ltid lowner >>= assertSuccess
      legalholdIsTeamInWhitelist ltid lowner >>= assertSuccess
      postLegalHoldSettings ltid lowner (mkLegalHoldSettings lhPort) >>= assertStatus 201

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
            TCKConsentAndNewClients -> do
              addc $ Just ["legalhold-implicit-consent"]
              legalholdWhitelistTeam ptid powner >>= assertSuccess
              legalholdIsTeamInWhitelist ptid powner >>= assertSuccess

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
    assertLabel 400 "client-error" resp
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

testLHRequestDevice :: App ()
testLHRequestDevice =
  startDynamicBackends [mempty] $ \[dom] -> do
    (alice, tid, [bob]) <- createTeam dom 2
    let reqNotEnabled requester requestee =
          requestLegalHoldDevice tid requester requestee
            >>= assertLabel 403 "legalhold-not-enabled"

    reqNotEnabled alice bob

    lpk <- getLastPrekey
    pks <- replicateM 3 getPrekey

    withMockServer (lhMockApp' $ Just (lpk, pks)) \lhPort _chan -> do
      let statusShouldbe :: String -> App ()
          statusShouldbe status =
            legalholdUserStatus tid alice bob `bindResponse` \resp -> do
              resp.status `shouldMatchInt` 200
              resp.json %. "status" `shouldMatch` status

      -- the user has not agreed to be under legalhold
      for_ [alice, bob] \requester -> do
        reqNotEnabled requester bob
        statusShouldbe "no_consent"

      legalholdWhitelistTeam tid alice >>= assertSuccess
      postLegalHoldSettings tid alice (mkLegalHoldSettings lhPort) >>= assertSuccess

      statusShouldbe "disabled"

      requestLegalHoldDevice tid alice bob >>= assertStatus 201
      statusShouldbe "pending"

      -- FIXME(mangoiv): we send two notifications to the client
      -- which I'm pretty sure is not correct

      -- requesting twice should be idempotent wrt the approval
      requestLegalHoldDevice tid alice bob >>= assertStatus 204
      statusShouldbe "pending"

      -- TODO(mangoiv): test if prekeys are in cassandra?

      [bobc1, bobc2] <- replicateM 2 do
        objId $ addClient bob def `bindResponse` getJSON 201
      for_ [bobc1, bobc2] \client ->
        awaitNotification bob client noValue isUserLegalholdRequestNotif >>= \notif -> do
          notif %. "payload.0.last_prekey" `shouldMatch` lpk
          notif %. "payload.0.id" `shouldMatch` objId bob

-- | pops a channel until it finds an event that returns a 'Just'
--   upon running the matcher function
checkChan :: HasCallStack => Chan t -> (t -> App (Maybe a)) -> App a
checkChan chan match =
  maybe (assertFailure "checkChan: timed out") pure =<< timeout 5_000_000 do
    let go = readChan chan >>= match >>= maybe go pure
    go

-- | like 'checkChan' but throws away the request and decodes the body
checkChanVal :: HasCallStack => Chan (t, LazyByteString) -> (Value -> MaybeT App a) -> App a
checkChanVal chan match = checkChan chan \(_, bs) -> runMaybeT do
  MaybeT (pure (decode bs)) >>= match

testLHApproveDevice :: App ()
testLHApproveDevice = do
  startDynamicBackends [mempty] \[dom] -> do
    -- team users
    -- alice (boss) and bob and charlie (member)
    (alice, tid, [bob, charlie]) <- createTeam dom 3

    -- ollie the outsider
    -- ollie <- do
    --   o <- randomUser dom def
    --   connectTwoUsers o alice
    --   pure o

    -- sandy the stranger
    -- sandy <- randomUser dom def
    --
    -- for sandy and ollie see below

    legalholdWhitelistTeam tid alice >>= assertStatus 200
    -- TODO(mangoiv): it seems like correct behaviour to throw a 412
    -- here, as we can only approve a device if we're in the pending
    -- state. however, the old tests passed with a 403 which makes
    -- this suspicious.
    approveLegalHoldDevice tid (bob %. "qualified_id") defPassword
      >>= assertLabel 412 "legalhold-not-pending"

    withMockServer lhMockApp \lhPort chan -> do
      legalholdWhitelistTeam tid alice
        >>= assertStatus 200
      postLegalHoldSettings tid alice (mkLegalHoldSettings lhPort)
        >>= assertStatus 201
      requestLegalHoldDevice tid alice bob
        >>= assertStatus 201

      let uidsAndTidMatch val = do
            actualTid <-
              MaybeT (lookupField val "team_id")
                >>= lift . asString
            actualUid <-
              MaybeT (lookupField val "user_id")
                >>= lift . asString
            bobUid <- lift $ objId bob

            -- we pass the check on equality
            unless ((actualTid, actualUid) == (tid, bobUid)) do
              mzero

      checkChanVal chan uidsAndTidMatch

      -- the team owner cannot approve for bob
      approveLegalHoldDevice' tid alice bob defPassword
        >>= assertLabel 403 "access-denied"
      -- bob needs to provide a password
      approveLegalHoldDevice tid bob "wrong-password"
        >>= assertLabel 403 "access-denied"
      -- now bob finally found his password
      approveLegalHoldDevice tid bob defPassword
        >>= assertStatus 200

      let matchAuthToken val =
            MaybeT (val `lookupField` "refresh_token")
              >>= lift . asString

      checkChanVal chan matchAuthToken
        >>= renewToken bob
        >>= assertStatus 200

      -- TODO(mangoiv): more CQL checks?
      -- also look at whether it makes sense to check the client id of the
      -- legalhold device...
      legalholdUserStatus tid alice bob `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "status" `shouldMatch` "enabled"

      replicateM 2 do
        objId $ addClient bob def `bindResponse` getJSON 201
        >>= traverse_ \client ->
          awaitNotification bob client noValue isUserClientAddNotif >>= \notif -> do
            notif %. "payload.0.client.type" `shouldMatch` "legalhold"
            notif %. "payload.0.client.class" `shouldMatch` "legalhold"

      -- the other team members receive a notification about the
      -- legalhold device being approved in their team
      for_ [alice, charlie] \user -> do
        client <- objId $ addClient user def `bindResponse` getJSON 201
        printJSON =<< objId bob

        awaitNotification user client noValue isUserLegalholdEnabledNotif >>= \notif -> do
          notif %. "payload.0.id" `shouldMatch` objId bob

-- TODO(mangoiv): there's no reasonable check that sandy and ollie don't get any notifs
-- as we never know when to timeout as we don't have any consistency guarantees
