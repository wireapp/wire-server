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
import Control.Lens ((.~), (^?), (^?!))
import Control.Monad.Reader (asks, local)
import Control.Monad.Trans.Class (lift)
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.Map as Map
import qualified Data.ProtoLens as Proto
import Data.ProtoLens.Labels ()
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Stack
import MLS.Util
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
  withMockServer def lhMockApp $ \lhDomAndPort _chan -> do
    (owner, tid, [alice, alex]) <- createTeam OwnDomain 3

    legalholdWhitelistTeam tid owner >>= assertSuccess
    legalholdIsTeamInWhitelist tid owner >>= assertSuccess
    postLegalHoldSettings tid owner (mkLegalHoldSettings lhDomAndPort) >>= assertStatus 201

    george <- randomUser OwnDomain def
    georgeQId <- objQidObject george
    hannes <- randomUser OwnDomain def
    hannesQId <- objQidObject hannes

    connectUsers [alice, george, hannes]
    connectUsers [alex, george, hannes]
    conv <- postConversation alice (defProteus {qualifiedUsers = [alex], team = Just tid}) >>= getJSON 201

    -- the guest should be added to the conversation
    bindResponse (addMembers alice conv def {users = [georgeQId]}) $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "type" `shouldMatch` "conversation.member-join"

    -- assert that the guest is in the conversation
    checkConvHasOtherMembers conv alice [alex, george]

    -- now request legalhold for alex (but not alice)
    requestLegalHoldDevice tid owner alex >>= assertSuccess

    -- the guest should not be removed from the conversation before approving
    checkConvHasOtherMembers conv alice [alex, george]

    -- it should be possible to add the another guest while the LH device is not approved
    addMembers alex conv def {users = [hannesQId]} `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "type" `shouldMatch` "conversation.member-join"
    checkConvHasOtherMembers conv alice [alex, george, hannes]

    approveLegalHoldDevice tid alex defPassword >>= assertSuccess
    -- the guest should be removed from the conversation
    checkConvHasOtherMembers conv alice [alex]

    -- it should not be possible neither for alex nor for alice to add the guest back
    addMembers alex conv def {users = [georgeQId]}
      >>= assertLabel 403 "not-connected"

    addMembers alice conv def {users = [georgeQId]}
      >>= assertLabel 403 "missing-legalhold-consent"
  where
    checkConvHasOtherMembers :: (HasCallStack) => Value -> Value -> [Value] -> App ()
    checkConvHasOtherMembers conv u us =
      bindResponse (getConversation u conv) $ \resp -> do
        resp.status `shouldMatchInt` 200
        mems <-
          resp.json %. "members.others"
            & asList >>= traverse \m -> do
              m %. "qualified_id"
        mems `shouldMatchSet` forM us (\m -> m %. "qualified_id")

testLHMessageExchange ::
  (HasCallStack) =>
  TaggedBool "clients1New" ->
  TaggedBool "clients2New" ->
  App ()
testLHMessageExchange (TaggedBool clients1New) (TaggedBool clients2New) = do
  -- We used to throw LegalholdConflictsOldClients if clients didn't have LH capability, but we
  -- don't do that any more because that broke things.
  -- Related: https://github.com/wireapp/wire-server/pull/4056
  withMockServer def lhMockApp $ \lhDomAndPort _chan -> do
    (owner, tid, [mem1, mem2]) <- createTeam OwnDomain 3

    let clientSettings :: Bool -> AddClient
        clientSettings allnew =
          if allnew
            then def {acapabilities = Just ["legalhold-implicit-consent"]} -- (is should be the default)
            else def {acapabilities = Nothing}
    void $ addClient (mem1 %. "qualified_id") (clientSettings clients1New) >>= getJSON 201
    void $ addClient (mem2 %. "qualified_id") (clientSettings clients2New) >>= getJSON 201

    legalholdWhitelistTeam tid owner >>= assertSuccess
    legalholdIsTeamInWhitelist tid owner >>= assertSuccess
    postLegalHoldSettings tid owner (mkLegalHoldSettings lhDomAndPort) >>= assertStatus 201

    conv <- postConversation mem1 (defProteus {qualifiedUsers = [mem2], team = Just tid}) >>= getJSON 201

    let getClients :: Value -> App [Value]
        getClients mem = do
          res <- getClientsQualified mem OwnDomain mem
          val <- getJSON 200 res
          asList val

        assertMessageSendingWorks :: (HasCallStack) => App ()
        assertMessageSendingWorks = do
          clients1 <- getClients mem1
          clients2 <- getClients mem2

          clientIds1 <- traverse objId clients1
          clientIds2 <- traverse objId clients2

          proteusRecipients <- mkProteusRecipients mem1 [(mem1, clientIds1), (mem2, clientIds2)] "hey there"

          let proteusMsg senderClient =
                Proto.defMessage @Proto.QualifiedNewOtrMessage
                  & #sender . Proto.client .~ (senderClient ^?! hex)
                  & #recipients .~ [proteusRecipients]
                  & #reportAll .~ Proto.defMessage

              sender clients =
                let senderClient = head $ filter (\c -> c ^? key (fromString "type") /= Just (toJSON "legalhold")) clients
                 in T.unpack $ senderClient ^?! key (fromString "id") . _String
          postProteusMessage mem1 (conv %. "qualified_id") (proteusMsg (sender clients1)) >>= assertSuccess
          postProteusMessage mem2 (conv %. "qualified_id") (proteusMsg (sender clients2)) >>= assertSuccess

    assertMessageSendingWorks

    requestLegalHoldDevice tid owner mem1 >>= assertSuccess
    assertMessageSendingWorks

    requestLegalHoldDevice tid owner mem2 >>= assertSuccess
    assertMessageSendingWorks

    approveLegalHoldDevice tid (mem1 %. "qualified_id") defPassword >>= assertSuccess
    fmap length (getClients mem1) `shouldMatchInt` 2
    assertMessageSendingWorks

    approveLegalHoldDevice tid (mem2 %. "qualified_id") defPassword >>= assertSuccess
    fmap length (getClients mem2) `shouldMatchInt` 2
    assertMessageSendingWorks

data TestClaimKeys
  = TCKConsentMissing -- (team not whitelisted, that is)
  | TCKConsentAndNewClients
  deriving (Show, Generic)

data LHApprovedOrPending
  = LHApproved
  | LHPending
  deriving (Show, Generic)

-- | Cannot fetch prekeys of LH users if requester has not given consent or has old clients.
testLHClaimKeys :: LHApprovedOrPending -> TestClaimKeys -> App ()
testLHClaimKeys approvedOrPending testmode = do
  withMockServer def lhMockApp $ \lhDomAndPort _chan -> do
    (lowner, ltid, [lmem]) <- createTeam OwnDomain 2
    (powner, ptid, [pmem]) <- createTeam OwnDomain 2

    legalholdWhitelistTeam ltid lowner >>= assertSuccess
    legalholdIsTeamInWhitelist ltid lowner >>= assertSuccess
    postLegalHoldSettings ltid lowner (mkLegalHoldSettings lhDomAndPort) >>= assertStatus 201

    requestLegalHoldDevice ltid lowner lmem >>= assertSuccess
    case approvedOrPending of
      LHApproved -> approveLegalHoldDevice ltid (lmem %. "qualified_id") defPassword >>= assertSuccess
      LHPending -> pure ()

    let addc caps = addClient pmem (settings caps) >>= assertSuccess
        settings caps =
          def
            { prekeys = Just $ take 10 somePrekeysRendered,
              lastPrekey = Just $ head someLastPrekeysRendered,
              acapabilities = caps
            }
     in addc $ Just ["legalhold-implicit-consent"]

    case testmode of
      TCKConsentMissing -> pure ()
      TCKConsentAndNewClients -> do
        legalholdWhitelistTeam ptid powner >>= assertSuccess
        legalholdIsTeamInWhitelist ptid powner >>= assertSuccess

    llhdevs :: [String] <- do
      let getCls :: Value -> App [String]
          getCls mem = do
            res <- getClientsQualified mem OwnDomain mem
            val <- getJSON 200 res
            cls <- asList val
            objId `mapM` cls
      getCls lmem

    let assertResp :: (HasCallStack) => Response -> App ()
        assertResp resp = case (testmode, llhdevs) of
          (TCKConsentMissing, (_ : _)) -> do
            resp.status `shouldMatchInt` 403
            resp.json %. "label" `shouldMatch` "missing-legalhold-consent"
          (TCKConsentAndNewClients, (_ : _)) -> do
            resp.status `shouldMatchInt` 200
          (_, []) -> do
            -- no lh devices: no reason to be shy!
            resp.status `shouldMatchInt` 200

    bindResponse (getUsersPrekeyBundle pmem (lmem %. "qualified_id")) assertResp
    case llhdevs of
      [llhdev] ->
        -- retrieve lh client if /a
        bindResponse (getUsersPrekeysClient pmem (lmem %. "qualified_id") llhdev) assertResp
      [] ->
        -- we're probably doing the LHPending thing right now
        pure ()
      bad@(_ : _ : _) ->
        -- fail if there is more than one.
        assertFailure ("impossible -- more than one LH device: " <> show bad)

    slmemdom <- asString $ lmem %. "qualified_id.domain"
    slmemid <- asString $ lmem %. "qualified_id.id"
    let userClients = Map.fromList [(slmemdom, Map.fromList [(slmemid, Set.fromList llhdevs)])]
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
testLHRequestDevice = do
  (alice, tid, [bob]) <- createTeam OwnDomain 2
  let reqNotEnabled requester requestee =
        requestLegalHoldDevice tid requester requestee
          >>= assertLabel 403 "legalhold-not-enabled"

  reqNotEnabled alice bob

  lpk <- getLastPrekey
  pks <- replicateM 3 getPrekey

  withMockServer def (lhMockAppWithPrekeys V0 MkCreateMock {nextLastPrey = pure lpk, somePrekeys = pure pks}) \lhDomAndPort _chan -> do
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
    postLegalHoldSettings tid alice (mkLegalHoldSettings lhDomAndPort) >>= assertSuccess

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
checkChan :: (HasCallStack) => Chan t -> (t -> App (Maybe a)) -> App a
checkChan chan match = do
  tSecs <- asks ((* 1_000_000) . timeOutSeconds)

  maybe (assertFailure "checkChan: timed out") pure =<< timeout tSecs do
    let go = readChan chan >>= match >>= maybe go pure
    go

-- | like 'checkChan' but throws away the request and decodes the body
checkChanVal :: (HasCallStack) => Chan (t, LazyByteString) -> (Value -> MaybeT App a) -> App a
checkChanVal chan match = checkChan chan \(_, bs) -> runMaybeT do
  MaybeT (pure (decode bs)) >>= match

testLHApproveDevice :: App ()
testLHApproveDevice = do
  -- team users
  -- alice (boss) and bob and charlie (member)
  (alice, tid, [bob, charlie]) <- createTeam OwnDomain 3

  -- ollie the outsider
  ollie <- do
    o <- randomUser OwnDomain def
    connectTwoUsers o alice
    pure o

  -- sandy the stranger
  sandy <- randomUser OwnDomain def

  legalholdWhitelistTeam tid alice >>= assertStatus 200
  approveLegalHoldDevice tid (bob %. "qualified_id") defPassword
    >>= assertLabel 412 "legalhold-not-pending"

  withMockServer def lhMockApp \lhDomAndPort chan -> do
    legalholdWhitelistTeam tid alice
      >>= assertStatus 200
    postLegalHoldSettings tid alice (mkLegalHoldSettings lhDomAndPort)
      >>= assertStatus 201
    requestLegalHoldDevice tid alice bob
      >>= assertStatus 201

    let uidsAndTidMatch val = do
          actualTid <-
            lookupFieldM val "team_id"
              >>= lift
              . asString
          actualUid <-
            lookupFieldM val "user_id"
              >>= lift
              . asString
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
            >>= lift
            . asString

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
testLHGetDeviceStatus = do
  -- team users
  -- alice (team owner) and bob (member)
  (alice, tid, [bob]) <- createTeam OwnDomain 2
  for_ [alice, bob] \user -> do
    legalholdUserStatus tid alice user `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "status" `shouldMatch` "no_consent"

  lpk <- getLastPrekey
  pks <- replicateM 3 getPrekey

  withMockServer
    def
    do lhMockAppWithPrekeys V0 MkCreateMock {nextLastPrey = pure lpk, somePrekeys = pure pks}
    \lhDomAndPort _chan -> do
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
      postLegalHoldSettings tid alice (mkLegalHoldSettings lhDomAndPort)
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
testLHDisableForUser = do
  (alice, tid, [bob]) <- createTeam OwnDomain 2

  withMockServer def lhMockApp \lhDomAndPort chan -> do
    setUpLHDevice tid alice bob lhDomAndPort

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
          BS8.unpack req.requestMethod
            == "POST"
            && req.pathInfo
            == (T.pack <$> ["legalhold", "remove"])
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
  -- team users
  -- alice (team owner) and bob (member)
  (alice, tid, [bob]) <- createTeam OwnDomain 2
  legalholdIsEnabled tid alice `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "lockStatus" `shouldMatch` "unlocked"
    resp.json %. "status" `shouldMatch` "disabled"

  withMockServer def lhMockApp \lhDomAndPort _chan -> do
    setUpLHDevice tid alice bob lhDomAndPort

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
  -- team users
  -- alice (team owner) and bob (member)
  (alice, tid, [bob]) <- createTeam OwnDomain 2

  let statusShouldBe :: String -> App ()
      statusShouldBe status = do
        getTeamMembers alice tid `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          [bobMember] <-
            resp.json %. "members"
              & asList >>= filterM \u -> do
                (==) <$> asString (u %. "user") <*> objId bob
          bobMember %. "legalhold_status" `shouldMatch` status

  statusShouldBe "no_consent"
  withMockServer def lhMockApp \lhDomAndPort _chan -> do
    statusShouldBe "no_consent"

    legalholdWhitelistTeam tid alice
      >>= assertStatus 200

    -- the status messages for these have already been tested
    postLegalHoldSettings tid alice (mkLegalHoldSettings lhDomAndPort)
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

testLHConnectionsWithNonConsentingUsers :: App ()
testLHConnectionsWithNonConsentingUsers = do
  (alice, tid, []) <- createTeam OwnDomain 1
  bob <- randomUser OwnDomain def
  carl <- randomUser OwnDomain def
  dee <- randomUser OwnDomain def

  legalholdWhitelistTeam tid alice
    >>= assertStatus 200

  withMockServer def lhMockApp \lhDomAndPort _chan -> do
    postLegalHoldSettings tid alice (mkLegalHoldSettings lhDomAndPort)
      >>= assertStatus 201

    requestLegalHoldDevice tid alice alice
      >>= assertStatus 201

    -- Connections are not blocked before LH is approved by alice
    connectTwoUsers alice bob
    bobConvId <- getConnection alice bob `bindResponse` \resp -> resp.json %. "qualified_conversation"

    postConnection dee alice >>= assertSuccess
    deeConvId <- getConnection alice dee `bindResponse` \resp -> resp.json %. "qualified_conversation"

    approveLegalHoldDevice tid alice defPassword
      >>= assertStatus 200

    -- Connections with bob and dee are now in missing-legalhold-consent state
    -- and the 1:1 convs are broken
    assertConnection alice bob "missing-legalhold-consent"
    assertConnection bob alice "missing-legalhold-consent"
    getConversation bob bobConvId
      >>= assertLabel 403 "access-denied"

    assertConnection alice dee "missing-legalhold-consent"
    assertConnection dee alice "missing-legalhold-consent"
    getConversation dee deeConvId
      >>= assertLabel 403 "access-denied"

    -- Connections are blocked after alice approves the LH device
    postConnection carl alice
      >>= assertLabel 403 "missing-legalhold-consent"
    postConnection alice carl
      >>= assertLabel 403 "missing-legalhold-consent"

    disableLegalHold tid alice alice defPassword
      >>= assertStatus 200

    -- Disabling LH restores connection status and 1:1 convs
    assertConnection alice bob "accepted"
    assertConnection bob alice "accepted"
    getConversation bob bobConvId `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "members.others.0.qualified_id" `shouldMatch` objQidObject alice

    assertConnection alice dee "pending"
    assertConnection dee alice "sent"
    getConversation dee deeConvId `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "members.others.0.qualified_id" `shouldMatch` objQidObject alice

testLHConnectionsWithConsentingUsers :: App ()
testLHConnectionsWithConsentingUsers = do
  (alice, teamA, []) <- createTeam OwnDomain 1
  (bob, teamB, [barbara]) <- createTeam OwnDomain 2

  legalholdWhitelistTeam teamA alice
    >>= assertStatus 200
  legalholdWhitelistTeam teamB bob
    >>= assertStatus 200

  withMockServer def lhMockApp \lhDomAndPort _chan -> do
    postLegalHoldSettings teamA alice (mkLegalHoldSettings lhDomAndPort)
      >>= assertStatus 201

    requestLegalHoldDevice teamA alice alice
      >>= assertStatus 201

    -- Connections are not blocked before LH is approved by alice
    connectTwoUsers alice bob

    approveLegalHoldDevice teamA alice defPassword
      >>= assertStatus 200

    -- Connection with bob is now in whatever state
    getConnection bob alice `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "status" `shouldMatch` "accepted"

    -- Connections are not blocked after alice approves the LH device because
    -- teamB has implicit consent
    connectTwoUsers alice barbara

data GroupConvAdmin
  = LegalholderIsAdmin
  | PeerIsAdmin
  | BothAreAdmins
  deriving (Show, Generic)

-- | If a member of an existing conversation is assigned a LH device, users are removed from
-- the conversation until policy conflicts are resolved.
--
-- As to who gets to stay:
-- - admins will stay over members
-- - local members will stay over remote members.
testLHNoConsentRemoveFromGroup :: LHApprovedOrPending -> GroupConvAdmin -> App ()
testLHNoConsentRemoveFromGroup approvedOrPending admin = do
  (alice, tidAlice, []) <- createTeam OwnDomain 1
  (bob, tidBob, []) <- createTeam OwnDomain 1
  legalholdWhitelistTeam tidAlice alice >>= assertStatus 200
  withMockServer def lhMockApp \lhDomAndPort _chan -> do
    postLegalHoldSettings tidAlice alice (mkLegalHoldSettings lhDomAndPort) >>= assertStatus 201
    withWebSockets [alice, bob] \[aws, bws] -> do
      connectTwoUsers alice bob
      (convId, qConvId) <- do
        let (inviter, tidInviter, invitee, inviteeRole) = case admin of
              LegalholderIsAdmin -> (alice, tidAlice, bob, "wire_member")
              BothAreAdmins -> (alice, tidAlice, bob, "wire_admin")
              PeerIsAdmin -> (bob, tidBob, alice, "wire_member")

        let createConv = defProteus {qualifiedUsers = [invitee], newUsersRole = inviteeRole, team = Just tidInviter}
        postConversation inviter createConv `bindResponse` \resp -> do
          resp.json %. "members.self.conversation_role" `shouldMatch` "wire_admin"
          resp.json %. "members.others.0.conversation_role" `shouldMatch` case admin of
            BothAreAdmins -> "wire_admin"
            PeerIsAdmin -> "wire_member"
            LegalholderIsAdmin -> "wire_member"
          (,) <$> resp.json %. "id" <*> resp.json %. "qualified_id"
      for_ [aws, bws] \ws -> do
        awaitMatch isConvCreateNotifNotSelf ws >>= \pl -> pl %. "payload.0.conversation" `shouldMatch` convId

      for_ [alice, bob] \user ->
        getConversation user qConvId >>= assertStatus 200

      requestLegalHoldDevice tidAlice alice alice >>= assertStatus 201
      case approvedOrPending of
        LHApproved -> approveLegalHoldDevice tidAlice alice defPassword >>= assertStatus 200
        LHPending -> pure ()

      legalholdUserStatus tidAlice alice alice `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "status" `shouldMatch` case approvedOrPending of
          LHApproved -> "enabled"
          LHPending -> "pending"

      case admin of
        LegalholderIsAdmin -> do
          case approvedOrPending of
            LHApproved -> for_ [aws, bws] do awaitMatch (isConvLeaveNotifWithLeaver bob)
            LHPending -> pure ()
          getConversation alice qConvId >>= assertStatus 200
          getConversation bob qConvId >>= case approvedOrPending of
            LHApproved -> assertLabel 403 "access-denied"
            LHPending -> assertStatus 200
        PeerIsAdmin -> do
          case approvedOrPending of
            LHApproved -> for_ [aws, bws] do awaitMatch (isConvLeaveNotifWithLeaver alice)
            LHPending -> pure ()
          getConversation bob qConvId >>= assertStatus 200
          getConversation alice qConvId >>= case approvedOrPending of
            LHApproved -> assertLabel 403 "access-denied"
            LHPending -> assertStatus 200
        BothAreAdmins -> do
          case approvedOrPending of
            LHApproved -> for_ [aws, bws] do awaitMatch (isConvLeaveNotifWithLeaver bob)
            LHPending -> pure ()
          getConversation alice qConvId >>= assertStatus 200
          getConversation bob qConvId >>= case approvedOrPending of
            LHApproved -> assertLabel 403 "access-denied"
            LHPending -> assertStatus 200

testLHHappyFlow :: App ()
testLHHappyFlow = do
  (alice, tid, [bob]) <- createTeam OwnDomain 2
  let statusShouldBe :: String -> App ()
      statusShouldBe status =
        legalholdUserStatus tid alice bob `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json %. "status" `shouldMatch` status

  legalholdWhitelistTeam tid alice >>= assertStatus 200
  lpk <- getLastPrekey
  pks <- replicateM 3 getPrekey

  withMockServer def (lhMockAppWithPrekeys V0 MkCreateMock {nextLastPrey = pure lpk, somePrekeys = pure pks}) \lhDomAndPort _chan -> do
    postLegalHoldSettings tid alice (mkLegalHoldSettings lhDomAndPort) >>= assertStatus 201

    -- implicit consent
    statusShouldBe "disabled"
    -- whitelisting is idempotent
    legalholdWhitelistTeam tid alice >>= assertStatus 200
    statusShouldBe "disabled"

    -- memmbers cannot request LH devices
    requestLegalHoldDevice tid bob alice >>= assertLabel 403 "operation-denied"

    -- owners can; bob should now have a pending request
    requestLegalHoldDevice tid alice bob >>= assertStatus 201
    statusShouldBe "pending"

    -- owner cannot approve on behalf on user under legalhold
    approveLegalHoldDevice' tid alice bob defPassword >>= assertLabel 403 "access-denied"

    -- user can approve the request, however
    approveLegalHoldDevice tid bob defPassword `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200

    legalholdUserStatus tid alice bob `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "status" `shouldMatch` "enabled"
      _ <-
        resp.json
          `lookupField` "client.id"
          >>= assertJust "client id is present"
      resp.json %. "last_prekey" `shouldMatch` lpk

testLHGetStatus :: App ()
testLHGetStatus = do
  (alice, tid, [bob]) <- createTeam OwnDomain 2
  (charlie, _tidCharlie, [debora]) <- createTeam OwnDomain 2
  emil <- randomUser OwnDomain def

  let check :: (HasCallStack) => (MakesValue getter, MakesValue target) => getter -> target -> String -> App ()
      check getter target status = do
        profile <- getUser getter target >>= getJSON 200
        pStatus <- profile %. "legalhold_status" & asString
        status `shouldMatch` pStatus

  for_ [alice, bob, charlie, debora, emil] \u -> do
    check u bob "no_consent"
    check u emil "no_consent"
  legalholdWhitelistTeam tid alice >>= assertStatus 200
  withMockServer def lhMockApp \lhDomAndPort _chan -> do
    postLegalHoldSettings tid alice (mkLegalHoldSettings lhDomAndPort) >>= assertStatus 201
    for_ [alice, bob, charlie, debora, emil] \u -> do
      check u bob "disabled"
    requestLegalHoldDevice tid alice bob >>= assertStatus 201
    check debora bob "pending"
    approveLegalHoldDevice tid bob defPassword >>= assertStatus 200
    check debora bob "enabled"

testLHCannotCreateGroupWithUsersInConflict :: App ()
testLHCannotCreateGroupWithUsersInConflict = do
  (alice, tidAlice, [bob]) <- createTeam OwnDomain 2
  (charlie, _tidCharlie, [debora]) <- createTeam OwnDomain 2
  legalholdWhitelistTeam tidAlice alice >>= assertStatus 200
  connectTwoUsers bob charlie
  connectTwoUsers bob debora
  withMockServer def lhMockApp \lhDomAndPort _chan -> do
    postLegalHoldSettings tidAlice alice (mkLegalHoldSettings lhDomAndPort) >>= assertStatus 201
    postConversation bob defProteus {qualifiedUsers = [charlie, alice], newUsersRole = "wire_member", team = Just tidAlice}
      >>= assertStatus 201

    requestLegalHoldDevice tidAlice alice alice >>= assertStatus 201
    approveLegalHoldDevice tidAlice alice defPassword >>= assertStatus 200
    legalholdUserStatus tidAlice alice alice `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "status" `shouldMatch` "enabled"

    postConversation bob defProteus {qualifiedUsers = [debora, alice], newUsersRole = "wire_member", team = Just tidAlice}
      >>= assertLabel 403 "missing-legalhold-consent"

testLHNoConsentCannotBeInvited :: (HasCallStack) => App ()
testLHNoConsentCannotBeInvited = do
  -- team that is legalhold whitelisted
  (legalholder, tidLH, userLHNotActivated : _) <- createTeam OwnDomain 2
  legalholdWhitelistTeam tidLH legalholder >>= assertStatus 200

  -- team without legalhold
  (peer, _tidPeer, [peer2, peer3]) <- createTeam OwnDomain 3

  connectUsers [peer, userLHNotActivated]
  connectUsers [peer2, userLHNotActivated]

  withMockServer def lhMockApp \lhDomAndPort _chan -> do
    postLegalHoldSettings tidLH legalholder (mkLegalHoldSettings lhDomAndPort) >>= assertStatus 201
    cid <- postConversation userLHNotActivated defProteus {qualifiedUsers = [legalholder], newUsersRole = "wire_admin", team = Just tidLH} >>= getJSON 201
    addMembers userLHNotActivated cid (def {users = [peer], role = Just "wire_admin"}) >>= assertSuccess

    -- activate legalhold for legalholder
    requestLegalHoldDevice tidLH legalholder legalholder >>= assertSuccess
    legalholdUserStatus tidLH legalholder legalholder `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "status" `shouldMatch` "pending"

    addMembers userLHNotActivated cid (def {users = [peer2]}) >>= assertSuccess

    approveLegalHoldDevice tidLH (legalholder %. "qualified_id") defPassword >>= assertSuccess
    legalholdUserStatus tidLH legalholder legalholder `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "status" `shouldMatch` "enabled"

    addMembers userLHNotActivated cid (def {users = [peer3]}) >>= assertLabel 403 "not-connected"

testLHDisableBeforeApproval :: (HasCallStack) => App ()
testLHDisableBeforeApproval = do
  (alice, tid, [bob]) <- createTeam OwnDomain 2
  legalholdWhitelistTeam tid alice >>= assertStatus 200

  withMockServer def lhMockApp \lhDomAndPort _chan -> do
    postLegalHoldSettings tid alice (mkLegalHoldSettings lhDomAndPort) >>= assertStatus 201

    -- alice requests a legalhold device for bob and sets his status to "pending"
    requestLegalHoldDevice tid alice bob >>= assertSuccess
    let getBob'sStatus = (getUser bob bob >>= getJSON 200) %. "legalhold_status" & asString
    getBob'sStatus `shouldMatch` "pending"

    -- alice disables legalhold. the status for bob should now not be pending anymore
    disableLegalHold tid alice bob defPassword
      >>= assertStatus 200
    getBob'sStatus `shouldMatch` "disabled"

-- ---------
-- WPB-10783
-- ---------
testBlockLHForMLSUsers :: (HasCallStack) => App ()
testBlockLHForMLSUsers = do
  -- scenario 1:
  -- if charlie is in any MLS conversation, he cannot approve to be put under legalhold
  (charlie, tid, []) <- createTeam OwnDomain 1
  [charlie1] <- traverse (createMLSClient def) [charlie]
  void $ createNewGroup charlie1
  void $ createAddCommit charlie1 [charlie] >>= sendAndConsumeCommitBundle

  legalholdWhitelistTeam tid charlie >>= assertStatus 200
  withMockServer def lhMockApp \lhDomAndPort _chan -> do
    postLegalHoldSettings tid charlie (mkLegalHoldSettings lhDomAndPort) >>= assertStatus 201
    requestLegalHoldDevice tid charlie charlie `bindResponse` do
      assertLabel 409 "mls-legal-hold-not-allowed"

-- ---------
-- WPB-10772
-- ---------

--  | scenario 2.1:
-- charlie first is put under legalhold and after that wants to join an MLS conversation
-- claiming a keypackage of charlie to add them to a conversation should not be possible
testBlockClaimingKeyPackageForLHUsers :: (HasCallStack) => App ()
testBlockClaimingKeyPackageForLHUsers = do
  (alice, tid, [charlie]) <- createTeam OwnDomain 2
  [alice1, charlie1] <- traverse (createMLSClient def) [alice, charlie]
  _ <- uploadNewKeyPackage charlie1
  _ <- createNewGroup alice1
  legalholdWhitelistTeam tid alice >>= assertStatus 200
  withMockServer def lhMockApp \lhDomAndPort _chan -> do
    postLegalHoldSettings tid alice (mkLegalHoldSettings lhDomAndPort) >>= assertStatus 201
    requestLegalHoldDevice tid alice charlie >>= assertSuccess
    approveLegalHoldDevice tid (charlie %. "qualified_id") defPassword >>= assertSuccess
    profile <- getUser alice charlie >>= getJSON 200
    pStatus <- profile %. "legalhold_status" & asString
    pStatus `shouldMatch` "enabled"

    mls <- getMLSState
    claimKeyPackages mls.ciphersuite alice1 charlie
      `bindResponse` assertLabel 409 "mls-legal-hold-not-allowed"

-- | scenario 2.2:
-- charlie is put under legalhold but creates an MLS Group himself
-- since he doesn't need to claim his own keypackage to do so, this would succeed
-- we need to check upon group creation if the user is under legalhold and reject
-- the operation if they are
testBlockCreateMLSConvForLHUsers :: (HasCallStack) => App ()
testBlockCreateMLSConvForLHUsers = do
  (alice, tid, [charlie]) <- createTeam OwnDomain 2
  [alice1, charlie1] <- traverse (createMLSClient def) [alice, charlie]
  _ <- uploadNewKeyPackage alice1
  legalholdWhitelistTeam tid alice >>= assertStatus 200
  withMockServer def lhMockApp \lhDomAndPort _chan -> do
    postLegalHoldSettings tid alice (mkLegalHoldSettings lhDomAndPort) >>= assertStatus 201
    requestLegalHoldDevice tid alice charlie >>= assertSuccess
    approveLegalHoldDevice tid (charlie %. "qualified_id") defPassword >>= assertSuccess
    profile <- getUser alice charlie >>= getJSON 200
    pStatus <- profile %. "legalhold_status" & asString
    pStatus `shouldMatch` "enabled"

    -- charlie tries to create a group and should fail when POSTing the add commit
    _ <- createNewGroup charlie1

    void
      -- we try to add alice since adding charlie himself would trigger 2.1
      -- since he'd try to claim his own keypackages
      $ createAddCommit charlie1 [alice]
      >>= \mp ->
        postMLSCommitBundle mp.sender (mkBundle mp)
          `bindResponse` assertLabel 409 "mls-legal-hold-not-allowed"

    -- (unsurprisingly) this same thing should also work in the one2one case

    respJson <- getMLSOne2OneConversation alice charlie >>= getJSON 200
    resetGroup alice1 (respJson %. "conversation")

    void
      -- we try to add alice since adding charlie himself would trigger 2.1
      -- since he'd try to claim his own keypackages
      $ createAddCommit charlie1 [alice]
      >>= \mp ->
        postMLSCommitBundle mp.sender (mkBundle mp)
          `bindResponse` assertLabel 409 "mls-legal-hold-not-allowed"

testLHApproveDeviceV1 :: App ()
testLHApproveDeviceV1 = do
  (alice, tid, [bob, _charlie]) <- createTeam OwnDomain 3

  legalholdWhitelistTeam tid alice >>= assertStatus 200

  withMockServer def lhMockAppV1 \lhDomAndPort chan -> do
    legalholdWhitelistTeam tid alice
      >>= assertStatus 200
    postLegalHoldSettings tid alice (mkLegalHoldSettings lhDomAndPort)
      >>= assertStatus 201

    checkChan chan \(req, _) -> runMaybeT . lift $ do
      BS8.unpack req.requestMethod `shouldMatch` "GET"
      req.pathInfo `shouldMatch` (T.pack <$> ["legalhold", "status"])

    requestLegalHoldDevice tid alice bob
      >>= assertStatus 201

    checkChan chan \(req, body) -> runMaybeT . lift $ do
      BS8.unpack req.requestMethod `shouldMatch` "POST"
      req.pathInfo `shouldMatch` (T.pack <$> ["legalhold", "v1", "initiate"])
      let (Just (value :: Value)) = decode body
      value %. "team_id" `shouldMatch` tid
      value %. "user_id" `shouldMatch` objQid bob
