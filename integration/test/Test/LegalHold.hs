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
import Control.Monad.Reader (asks, local)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.Map as Map
import qualified Data.ProtoLens as Proto
import Data.ProtoLens.Labels ()
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Stack
import Network.Wai (Request (pathInfo, requestMethod))
import Notifications
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
  deriving (Show, Generic)

-- | Cannot fetch prekeys of LH users if requester has not given consent or has old clients.
testLHClaimKeys :: TestClaimKeys -> App ()
testLHClaimKeys testmode = do
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

    withMockServer (lhMockAppWithPrekeys MkCreateMock {nextLastPrey = pure lpk, somePrekeys = pure pks}) \lhPort _chan -> do
      let statusShouldBe :: String -> App ()
          statusShouldBe status =
            legalholdUserStatus tid alice bob `bindResponse` \resp -> do
              resp.status `shouldMatchInt` 200
              resp.json %. "status" `shouldMatch` status

      -- the user has not agreed to be under legalhold
      for_ [alice, bob] \requester -> do
        reqNotEnabled requester bob
        statusShouldBe "no_consent"

      legalholdWhitelistTeam tid alice >>= assertSuccess
      postLegalHoldSettings tid alice (mkLegalHoldSettings lhPort) >>= assertSuccess

      statusShouldBe "disabled"

      requestLegalHoldDevice tid alice bob >>= assertStatus 201
      statusShouldBe "pending"

      -- requesting twice should be idempotent wrt the approval
      -- mind that requesting twice means two "user.legalhold-request" notifications
      -- for the clients of the user under legalhold (bob)
      requestLegalHoldDevice tid alice bob >>= assertStatus 204
      statusShouldBe "pending"

      [bobc1, bobc2] <- replicateM 2 do
        objId $ addClient bob def `bindResponse` getJSON 201
      for_ [bobc1, bobc2] \client ->
        awaitNotification bob client noValue isUserLegalholdRequestNotif >>= \notif -> do
          notif %. "payload.0.last_prekey" `shouldMatch` lpk
          notif %. "payload.0.id" `shouldMatch` objId bob

-- | pops a channel until it finds an event that returns a 'Just'
--   upon running the matcher function
checkChan :: HasCallStack => Chan t -> (t -> App (Maybe a)) -> App a
checkChan chan match = do
  tSecs <- asks ((* 1_000_000) . timeOutSeconds)

  maybe (assertFailure "checkChan: timed out") pure =<< timeout tSecs do
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
    ollie <- do
      o <- randomUser dom def
      connectTwoUsers o alice
      pure o

    -- sandy the stranger
    sandy <- randomUser dom def

    legalholdWhitelistTeam tid alice >>= assertStatus 200
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
              lookupFieldM val "team_id"
                >>= lift . asString
            actualUid <-
              lookupFieldM val "user_id"
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
            lookupFieldM val "refresh_token"
              >>= lift . asString

      checkChanVal chan matchAuthToken
        >>= renewToken bob
        >>= assertStatus 200

      lhdId <- lhDeviceIdOf bob

      legalholdUserStatus tid alice bob `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "client.id" `shouldMatch` lhdId
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
        awaitNotification user client noValue isUserLegalholdEnabledNotif >>= \notif -> do
          notif %. "payload.0.id" `shouldMatch` objId bob
      for_ [ollie, sandy] \outsider -> do
        outsiderClient <- objId $ addClient outsider def `bindResponse` getJSON 201
        assertNoNotifications outsider outsiderClient Nothing isUserLegalholdEnabledNotif

testLHGetDeviceStatus :: App ()
testLHGetDeviceStatus =
  startDynamicBackends [mempty] \[dom] -> do
    -- team users
    -- alice (team owner) and bob (member)
    (alice, tid, [bob]) <- createTeam dom 2
    for_ [alice, bob] \user -> do
      legalholdUserStatus tid alice user `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "status" `shouldMatch` "no_consent"

    lpk <- getLastPrekey
    pks <- replicateM 3 getPrekey

    withMockServer
      do lhMockAppWithPrekeys MkCreateMock {nextLastPrey = pure lpk, somePrekeys = pure pks}
      \lhPort _chan -> do
        legalholdWhitelistTeam tid alice
          >>= assertStatus 200

        legalholdUserStatus tid alice bob `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json %. "status" `shouldMatch` "disabled"
          lookupField resp.json "last_prekey"
            >>= assertNothing
          runMaybeT (lookupFieldM resp.json "client" >>= flip lookupFieldM "id")
            >>= assertNothing

        -- the status messages for these have already been tested
        postLegalHoldSettings tid alice (mkLegalHoldSettings lhPort)
          >>= assertStatus 201

        requestLegalHoldDevice tid alice bob
          >>= assertStatus 201

        approveLegalHoldDevice tid bob defPassword
          >>= assertStatus 200

        lhdId <- lhDeviceIdOf bob
        legalholdUserStatus tid alice bob `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json %. "status" `shouldMatch` "enabled"
          resp.json %. "last_prekey" `shouldMatch` lpk
          resp.json %. "client.id" `shouldMatch` lhdId

        requestLegalHoldDevice tid alice bob
          >>= assertLabel 409 "legalhold-already-enabled"

-- | this sets the timeout to a higher number; we need
--   this because the SQS queue on the brig is super slow
--   and that's why client.remove events arrive really late
--
--   FUTUREWORK(mangoiv): improve the speed of internal
--   event queuing
setTimeoutTo :: Int -> Env -> Env
setTimeoutTo tSecs env = env {timeOutSeconds = tSecs}

testLHDisableForUser :: App ()
testLHDisableForUser =
  startDynamicBackends [mempty] \[dom] -> do
    -- team users
    -- alice (team owner) and bob (member)
    (alice, tid, [bob]) <- createTeam dom 2

    withMockServer lhMockApp \lhPort chan -> do
      setUpLHDevice tid alice bob lhPort

      bobc <- objId $ addClient bob def `bindResponse` getJSON 201

      awaitNotification bob bobc noValue isUserClientAddNotif >>= \notif -> do
        notif %. "payload.0.client.type" `shouldMatch` "legalhold"
        notif %. "payload.0.client.class" `shouldMatch` "legalhold"

      -- only an admin can disable legalhold
      disableLegalHold tid bob bob defPassword
        >>= assertLabel 403 "operation-denied"

      disableLegalHold tid alice bob "fix ((\"the password always is \" <>) . show)"
        >>= assertLabel 403 "access-denied"

      disableLegalHold tid alice bob defPassword
        >>= assertStatus 200

      checkChan chan \(req, _) -> runMaybeT do
        unless
          do
            BS8.unpack req.requestMethod == "POST"
              && req.pathInfo == (T.pack <$> ["legalhold", "remove"])
          mzero

      void $ local (setTimeoutTo 90) do
        awaitNotification bob bobc noValue isUserClientRemoveNotif
          *> awaitNotification bob bobc noValue isUserLegalholdDisabledNotif

      bobId <- objId bob
      lhClients <-
        BrigI.getClientsFull bob [bobId] `bindResponse` \resp -> do
          resp.json %. bobId
            & asList
            >>= filterM \val -> (== "legalhold") <$> (val %. "type" & asString)

      shouldBeEmpty lhClients

testLHEnablePerTeam :: App ()
testLHEnablePerTeam = do
  startDynamicBackends [mempty] \[dom] -> do
    -- team users
    -- alice (team owner) and bob (member)
    (alice, tid, [bob]) <- createTeam dom 2
    legalholdIsEnabled tid alice `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "lockStatus" `shouldMatch` "unlocked"
      resp.json %. "status" `shouldMatch` "disabled"

    withMockServer lhMockApp \lhPort _chan -> do
      setUpLHDevice tid alice bob lhPort

      legalholdUserStatus tid alice bob `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "status" `shouldMatch` "enabled"

        putLegalholdStatus tid alice "disabled"
          `bindResponse` assertLabel 403 "legalhold-whitelisted-only"

      -- the put doesn't have any influence on the status being "enabled"
      legalholdUserStatus tid alice bob `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "status" `shouldMatch` "enabled"

testLHGetMembersIncludesStatus :: App ()
testLHGetMembersIncludesStatus = do
  startDynamicBackends [mempty] \[dom] -> do
    -- team users
    -- alice (team owner) and bob (member)
    (alice, tid, [bob]) <- createTeam dom 2

    let statusShouldBe :: String -> App ()
        statusShouldBe status = do
          getTeamMembers alice tid `bindResponse` \resp -> do
            resp.status `shouldMatchInt` 200
            [bobMember] <-
              resp.json %. "members" & asList >>= filterM \u -> do
                (==) <$> asString (u %. "user") <*> objId bob
            bobMember %. "legalhold_status" `shouldMatch` status

    statusShouldBe "no_consent"
    withMockServer lhMockApp \lhPort _chan -> do
      statusShouldBe "no_consent"

      legalholdWhitelistTeam tid alice
        >>= assertStatus 200

      -- the status messages for these have already been tested
      postLegalHoldSettings tid alice (mkLegalHoldSettings lhPort)
        >>= assertStatus 201

      -- legalhold has been requested but is disabled
      statusShouldBe "disabled"

      requestLegalHoldDevice tid alice bob
        >>= assertStatus 201

      -- legalhold has been set to pending after requesting device
      statusShouldBe "pending"

      approveLegalHoldDevice tid bob defPassword
        >>= assertStatus 200

      -- bob has accepted the legalhold device
      statusShouldBe "enabled"

type TB s = TaggedBool s

testLHNoConsentBlockOne2OneConv :: TB "connect first" -> TB "team peer" -> TB "approve LH" -> TB "test pending connection" -> App ()
testLHNoConsentBlockOne2OneConv
  (MkTagged connectFirst)
  (MkTagged teampeer)
  (MkTagged approveLH)
  (MkTagged testPendingConnection) =
    startDynamicBackends [mempty] \[dom1] -> do
      -- team users
      -- alice (team owner) and bob (member)
      (alice, tid, []) <- createTeam dom1 1
      bob <-
        if teampeer
          then do
            (walice, _tid, []) <- createTeam dom1 1
            -- FUTUREWORK(mangoiv): creating a team on a second backend
            -- causes this bug: https://wearezeta.atlassian.net/browse/WPB-6640
            pure walice
          else randomUser dom1 def

      legalholdWhitelistTeam tid alice
        >>= assertStatus 200

      let doEnableLH :: HasCallStack => App (Maybe String)
          doEnableLH = do
            -- alice requests a legalhold device for herself
            requestLegalHoldDevice tid alice alice
              >>= assertStatus 201

            when approveLH do
              approveLegalHoldDevice tid alice defPassword
                >>= assertStatus 200
            legalholdUserStatus tid alice alice `bindResponse` \resp -> do
              resp.status `shouldMatchInt` 200
              resp.json %. "status" `shouldMatch` if approveLH then "enabled" else "pending"
            if approveLH
              then Just <$> lhDeviceIdOf alice
              else pure Nothing

          doDisableLH :: HasCallStack => App ()
          doDisableLH =
            disableLegalHold tid alice alice defPassword
              >>= assertStatus 200

      withMockServer lhMockApp \lhPort _chan -> do
        postLegalHoldSettings tid alice (mkLegalHoldSettings lhPort)
          >>= assertStatus 201

        if not connectFirst
          then do
            void doEnableLH
            postConnection alice bob
              >>= assertLabel 403 "missing-legalhold-consent"

            postConnection bob alice
              >>= assertLabel 403 "missing-legalhold-consent"
          else do
            alicec <- objId $ addClient alice def >>= getJSON 201
            bobc <- objId $ addClient bob def >>= getJSON 201

            postConnection alice bob
              >>= assertStatus 201
            mbConvId <-
              if testPendingConnection
                then pure Nothing
                else
                  Just
                    <$> do
                      putConnection bob alice "accepted"
                        >>= getJSON 200
                      %. "qualified_conversation"

            -- we need to take away the pending/ sent status for the connections
            [lastNotifAlice, lastNotifBob] <- for [(alice, alicec), (bob, bobc)] \(user, client) -> do
              -- we get two events if bob accepts alice's request
              let numEvents = if testPendingConnection then 1 else 2
              last <$> awaitNotifications user client Nothing numEvents isUserConnectionNotif

            mbLHDevice <- doEnableLH

            let assertConnectionsMissingLHConsent =
                  for_ [(bob, alice), (alice, bob)] \(a, b) ->
                    getConnections a `bindResponse` \resp -> do
                      resp.status `shouldMatchInt` 200
                      conn <- assertOne =<< do resp.json %. "connections" & asList
                      conn %. "status" `shouldMatch` "missing-legalhold-consent"
                      conn %. "from" `shouldMatch` objId a
                      conn %. "to" `shouldMatch` objId b

            assertConnectionsMissingLHConsent

            [lastNotifAlice', lastNotifBob'] <- for [(alice, alicec, lastNotifAlice), (bob, bobc, lastNotifBob)] \(user, client, lastNotif) -> do
              awaitNotification user client (Just lastNotif) isUserConnectionNotif >>= \notif ->
                notif %. "payload.0.connection.status" `shouldMatch` "missing-legalhold-consent"
                  $> notif

            for_ [(bob, alice), (alice, bob)] \(a, b) ->
              putConnection a b "accepted"
                >>= assertLabel 403 "bad-conn-update"

            -- putting the connection to "accepted" with 403 doesn't change the
            -- connection status
            assertConnectionsMissingLHConsent

            bobc2 <- objId $ addClient bob def >>= getJSON 201

            let -- \| we send a message from bob to alice, but only if
                -- we have a conversation id and a legalhold device
                -- we first create a message that goes to recipients
                -- chosen by the first callback passed
                -- then send the message using proteus
                -- and in the end running the assertino callback to
                -- verify the result
                sendMessageFromBobToAlice ::
                  HasCallStack =>
                  (String -> [String]) ->
                  -- \^ if we have the legalhold device registered, this
                  --   callback will be passed the lh device
                  (Response -> App ()) ->
                  -- \^ the callback to verify our response (an assertion)
                  App ()
                sendMessageFromBobToAlice recipients assertion =
                  for_ ((,) <$> mbConvId <*> mbLHDevice) \(convId, device) -> do
                    successfulMsgForOtherUsers <-
                      mkProteusRecipients
                        bob -- bob is the sender
                        [(alice, recipients device), (bob, [bobc])]
                        -- we send to clients of alice, maybe the legalhold device
                        -- we need to send to our other clients (bobc)
                        "hey alice (and eve)" -- the message
                    let bobaliceMessage =
                          Proto.defMessage @Proto.QualifiedNewOtrMessage
                            & #sender . Proto.client .~ (bobc2 ^?! hex)
                            & #recipients .~ [successfulMsgForOtherUsers]
                            & #reportAll .~ Proto.defMessage
                    -- make sure that `convId` is not just the `convId` but also
                    -- contains the domain because `postProteusMessage` will take the
                    -- comain from the `convId` json object
                    postProteusMessage bob convId bobaliceMessage
                      `bindResponse` assertion

            sendMessageFromBobToAlice (\device -> [alicec, device]) \resp -> do
              resp.status `shouldMatchInt` 404

            -- now we disable legalhold
            doDisableLH

            for_ mbLHDevice \lhd ->
              local (setTimeoutTo 90) $
                awaitNotification alice alicec noValue isUserClientRemoveNotif >>= \notif ->
                  notif %. "payload.0.client.id" `shouldMatch` lhd

            let assertStatusFor user status =
                  getConnections user `bindResponse` \resp -> do
                    resp.status `shouldMatchInt` 200
                    conn <- assertOne =<< do resp.json %. "connections" & asList
                    conn %. "status" `shouldMatch` status

            if testPendingConnection
              then do
                assertStatusFor alice "sent"
                assertStatusFor bob "pending"
              else do
                assertStatusFor alice "accepted"
                assertStatusFor bob "accepted"

            for_ [(alice, alicec, lastNotifAlice'), (bob, bobc, lastNotifBob')] \(user, client, lastNotif) -> do
              awaitNotification user client (Just lastNotif) isUserConnectionNotif >>= \notif ->
                notif %. "payload.0.connection.status" `shouldMatchOneOf` ["sent", "pending", "accepted"]

            sendMessageFromBobToAlice (const [alicec]) \resp -> do
              resp.status `shouldMatchInt` 201

            sendMessageFromBobToAlice (\device -> [device]) \resp -> do
              resp.status `shouldMatchInt` 412
