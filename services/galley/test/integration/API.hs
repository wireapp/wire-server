{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module API
  ( tests,
  )
where

import qualified API.CustomBackend as CustomBackend
import qualified API.IdMapping as IdMapping
import qualified API.MessageTimer as MessageTimer
import qualified API.Roles as Roles
import API.SQS
import qualified API.Teams as Teams
import qualified API.Teams.Feature as TeamFeature
import qualified API.Teams.LegalHold as Teams.LegalHold
import API.Util
import Bilge hiding (timeout)
import Bilge.Assert
import Brig.Types
import qualified Control.Concurrent.Async as Async
import Control.Lens (view)
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import qualified Data.Code as Code
import Data.Id
import Data.List1
import qualified Data.List1 as List1
import qualified Data.Map.Strict as Map
import Data.Range
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Ascii as Ascii
import Galley.Types hiding (InternalMember (..), Member)
import Galley.Types.Conversations.Roles
import qualified Galley.Types.Teams as Teams
import Gundeck.Types.Notification
import Imports
import Network.Wai.Utilities.Error
import Test.Tasty
import Test.Tasty.Cannon ((#), TimeoutUnit (..))
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Wire.API.Conversation.Member (Member (..))

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "Galley integration tests"
    [ Teams.LegalHold.tests s,
      mainTests,
      Teams.tests s,
      MessageTimer.tests s,
      Roles.tests s,
      CustomBackend.tests s,
      TeamFeature.tests s,
      IdMapping.tests s
    ]
  where
    mainTests =
      testGroup
        "Main API"
        [ test s "status" status,
          test s "metrics" metrics,
          test s "create conversation" postConvOk,
          test s "get empty conversations" getConvsOk,
          test s "get conversations by ids" getConvsOk2,
          test s "fail to get >100 conversations" getConvsFailMaxSize,
          test s "get conversation ids" getConvIdsOk,
          test s "paginate through conversation ids" paginateConvIds,
          test s "fail to get >1000 conversation ids" getConvIdsFailMaxSize,
          test s "page through conversations" getConvsPagingOk,
          test s "fail to create conversation when not connected" postConvFailNotConnected,
          test s "M:N conversation creation must have <N members" postConvFailNumMembers,
          test s "fail to create conversation when blocked" postConvFailBlocked,
          test s "create self conversation" postSelfConvOk,
          test s "create 1:1 conversation" postO2OConvOk,
          test s "fail to create 1:1 conversation with yourself" postConvO2OFailWithSelf,
          test s "create connect conversation" postConnectConvOk,
          test s "create connect conversation with email" postConnectConvOk2,
          test s "upgrade connect/invite conversation" putConvAcceptOk,
          test s "upgrade conversation retries" putConvAcceptRetry,
          test s "create mutual connect conversation" postMutualConnectConvOk,
          test s "repeat / cancel connect requests" postRepeatConnectConvCancel,
          test s "block/unblock a connect/1-1 conversation" putBlockConvOk,
          test s "get conversation" getConvOk,
          test s "conversation meta access" accessConvMeta,
          test s "add members" postMembersOk,
          test s "add existing members" postMembersOk2,
          test s "add past members" postMembersOk3,
          test s "fail to add members when not connected" postMembersFail,
          test s "fail to add too many members" postTooManyMembersFail,
          test s "remove members" deleteMembersOk,
          test s "fail to remove members from self conv." deleteMembersFailSelf,
          test s "fail to remove members from 1:1 conv." deleteMembersFailO2O,
          test s "rename conversation" putConvRenameOk,
          test s "member update (otr mute)" putMemberOtrMuteOk,
          test s "member update (otr archive)" putMemberOtrArchiveOk,
          test s "member update (hidden)" putMemberHiddenOk,
          test s "member update (everything b)" putMemberAllOk,
          test s "conversation receipt mode update" putReceiptModeOk,
          test s "send typing indicators" postTypingIndicators,
          test s "leave connect conversation" leaveConnectConversation,
          test s "post cryptomessage 1" postCryptoMessage1,
          test s "post cryptomessage 2" postCryptoMessage2,
          test s "post cryptomessage 3" postCryptoMessage3,
          test s "post cryptomessage 4" postCryptoMessage4,
          test s "post cryptomessage 5" postCryptoMessage5,
          test s "join conversation" postJoinConvOk,
          test s "join code-access conversation" postJoinCodeConvOk,
          test s "convert invite to code-access conversation" postConvertCodeConv,
          test s "convert code to team-access conversation" postConvertTeamConv,
          test s "cannot join private conversation" postJoinConvFail,
          test s "remove user" removeUser
        ]

-------------------------------------------------------------------------------
-- API Tests

status :: TestM ()
status = do
  g <- view tsGalley
  get (g . path "/i/status")
    !!! const 200 === statusCode

metrics :: TestM ()
metrics = do
  g <- view tsGalley
  get (g . path "/i/metrics") !!! do
    const 200 === statusCode
    -- Should contain the request duration metric in its output
    const (Just "TYPE http_request_duration_seconds histogram") =~= responseBody

postConvOk :: TestM ()
postConvOk = do
  c <- view tsCannon
  alice <- randomUser
  bob <- randomUser
  jane <- randomUser
  connectUsers alice (list1 bob [jane])
  -- Ensure name is within range, max size is 256
  postConv alice [bob, jane] (Just (T.replicate 257 "a")) [] Nothing Nothing
    !!! const 400 === statusCode
  let nameMaxSize = T.replicate 256 "a"
  WS.bracketR3 c alice bob jane $ \(wsA, wsB, wsJ) -> do
    rsp <-
      postConv alice [bob, jane] (Just nameMaxSize) [] Nothing Nothing
        <!! const 201 === statusCode
    cid <- assertConv rsp RegularConv alice alice [bob, jane] (Just nameMaxSize) Nothing
    cvs <- mapM (convView cid) [alice, bob, jane]
    liftIO $ mapM_ WS.assertSuccess =<< Async.mapConcurrently (checkWs alice) (zip cvs [wsA, wsB, wsJ])
  where
    convView cnv usr = responseJsonUnsafeWithMsg "conversation" <$> getConv usr cnv
    checkWs alice (cnv, ws) = WS.awaitMatch (5 # Second) ws $ \n -> do
      ntfTransient n @?= False
      let e = List1.head (WS.unpackPayload n)
      evtConv e @?= cnvId cnv
      evtType e @?= ConvCreate
      evtFrom e @?= alice
      case evtData e of
        Just (EdConversation c') -> assertConvEquals cnv c'
        _ -> assertFailure "Unexpected event data"

postCryptoMessage1 :: TestM ()
postCryptoMessage1 = do
  c <- view tsCannon
  (alice, ac) <- randomUserWithClient (someLastPrekeys !! 0)
  (bob, bc) <- randomUserWithClient (someLastPrekeys !! 1)
  (eve, ec) <- randomUserWithClient (someLastPrekeys !! 2)
  connectUsers alice (list1 bob [eve])
  conv <- decodeConvId <$> postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
  -- WS receive timeout
  let t = 5 # Second
  -- Missing eve
  let m1 = [(bob, bc, "ciphertext1")]
  postOtrMessage id alice ac conv m1 !!! do
    const 412 === statusCode
    assertTrue_ (eqMismatch [(eve, Set.singleton ec)] [] [] . responseJsonUnsafe)
  -- Complete
  WS.bracketR2 c bob eve $ \(wsB, wsE) -> do
    let m2 = [(bob, bc, "ciphertext2"), (eve, ec, "ciphertext2")]
    postOtrMessage id alice ac conv m2 !!! do
      const 201 === statusCode
      assertTrue_ (eqMismatch [] [] [] . responseJsonUnsafe)
    void . liftIO $ WS.assertMatch t wsB (wsAssertOtr conv alice ac bc "ciphertext2")
    void . liftIO $ WS.assertMatch t wsE (wsAssertOtr conv alice ac ec "ciphertext2")
  -- Redundant self
  WS.bracketR3 c alice bob eve $ \(wsA, wsB, wsE) -> do
    let m3 = [(alice, ac, "ciphertext3"), (bob, bc, "ciphertext3"), (eve, ec, "ciphertext3")]
    postOtrMessage id alice ac conv m3 !!! do
      const 201 === statusCode
      assertTrue_ (eqMismatch [] [(alice, Set.singleton ac)] [] . responseJsonUnsafe)
    void . liftIO $ WS.assertMatch t wsB (wsAssertOtr conv alice ac bc "ciphertext3")
    void . liftIO $ WS.assertMatch t wsE (wsAssertOtr conv alice ac ec "ciphertext3")
    -- Alice should not get it
    assertNoMsg wsA (wsAssertOtr conv alice ac ac "ciphertext3")
  -- Deleted eve
  WS.bracketR2 c bob eve $ \(wsB, wsE) -> do
    deleteClient eve ec (Just defPassword) !!! const 200 === statusCode
    let m4 = [(bob, bc, "ciphertext4"), (eve, ec, "ciphertext4")]
    postOtrMessage id alice ac conv m4 !!! do
      const 201 === statusCode
      assertTrue_ (eqMismatch [] [] [(eve, Set.singleton ec)] . responseJsonUnsafe)
    void . liftIO $ WS.assertMatch t wsB (wsAssertOtr conv alice ac bc "ciphertext4")
    -- Eve should not get it
    assertNoMsg wsE (wsAssertOtr conv alice ac ec "ciphertext4")
  -- Deleted eve & redundant self
  WS.bracketR3 c alice bob eve $ \(wsA, wsB, wsE) -> do
    let m5 = [(bob, bc, "ciphertext5"), (eve, ec, "ciphertext5"), (alice, ac, "ciphertext5")]
    postOtrMessage id alice ac conv m5 !!! do
      const 201 === statusCode
      assertTrue_ (eqMismatch [] [(alice, Set.singleton ac)] [(eve, Set.singleton ec)] . responseJsonUnsafe)
    void . liftIO $ WS.assertMatch t wsB (wsAssertOtr conv alice ac bc "ciphertext5")
    -- Neither Alice nor Eve should get it
    assertNoMsg wsA (wsAssertOtr conv alice ac ac "ciphertext5")
    assertNoMsg wsE (wsAssertOtr conv alice ac ec "ciphertext5")
  -- Missing Bob, deleted eve & redundant self
  let m6 = [(eve, ec, "ciphertext6"), (alice, ac, "ciphertext6")]
  postOtrMessage id alice ac conv m6 !!! do
    const 412 === statusCode
    assertTrue_
      ( eqMismatch
          [(bob, Set.singleton bc)]
          [(alice, Set.singleton ac)]
          [(eve, Set.singleton ec)]
          . responseJsonUnsafe
      )
  -- A second client for Bob
  bc2 <- randomClient bob (someLastPrekeys !! 3)
  -- The first client listens for all messages of Bob
  WS.bracketR c bob $ \wsB -> do
    let cipher = "ciphertext7"
    -- The second client listens only for his own messages
    WS.bracketR (c . queryItem "client" (toByteString' bc2)) bob $ \wsB2 -> do
      let m7 = [(bob, bc, cipher), (bob, bc2, cipher)]
      postOtrMessage id alice ac conv m7 !!! do
        const 201 === statusCode
        assertTrue_ (eqMismatch [] [] [] . responseJsonUnsafe)
      -- Bob's first client gets both messages
      void . liftIO $ WS.assertMatch t wsB (wsAssertOtr conv alice ac bc cipher)
      void . liftIO $ WS.assertMatch t wsB (wsAssertOtr conv alice ac bc2 cipher)
      -- Bob's second client gets only the message destined for him
      void . liftIO $ WS.assertMatch t wsB2 (wsAssertOtr conv alice ac bc2 cipher)
      liftIO $ assertBool "unexpected equal clients" (bc /= bc2)
      assertNoMsg wsB2 (wsAssertOtr conv alice ac bc cipher)

postCryptoMessage2 :: TestM ()
postCryptoMessage2 = do
  b <- view tsBrig
  (alice, ac) <- randomUserWithClient (someLastPrekeys !! 0)
  (bob, bc) <- randomUserWithClient (someLastPrekeys !! 1)
  (eve, ec) <- randomUserWithClient (someLastPrekeys !! 2)
  connectUsers alice (list1 bob [eve])
  conv <- decodeConvId <$> postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
  -- Missing eve
  let m = [(bob, bc, "hello bob")]
  r1 <-
    postOtrMessage id alice ac conv m
      <!! const 412 === statusCode
  let x = responseJsonUnsafeWithMsg "ClientMismatch" r1
  liftIO $ assertBool "client mismatch" (eqMismatch [(eve, Set.singleton ec)] [] [] (Just x))
  -- Fetch all missing clients prekeys
  r2 <-
    post (b . path "/users/prekeys" . json (missingClients x))
      <!! const 200 === statusCode
  let p = responseJsonUnsafeWithMsg "prekeys" r2 :: UserClientMap (Maybe Prekey)
  liftIO $ do
    Map.keys (userClientMap p) @=? [makeIdOpaque eve]
    Map.keys <$> Map.lookup (makeIdOpaque eve) (userClientMap p) @=? Just [ec]

postCryptoMessage3 :: TestM ()
postCryptoMessage3 = do
  b <- view tsBrig
  (alice, ac) <- randomUserWithClient (someLastPrekeys !! 0)
  (bob, bc) <- randomUserWithClient (someLastPrekeys !! 1)
  (eve, ec) <- randomUserWithClient (someLastPrekeys !! 2)
  connectUsers alice (list1 bob [eve])
  conv <- decodeConvId <$> postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
  -- Missing eve
  let ciphertext = encodeCiphertext "hello bob"
  let m = otrRecipients [(bob, [(bc, ciphertext)])]
  r1 <-
    postProtoOtrMessage alice ac conv m
      <!! const 412 === statusCode
  let x = responseJsonUnsafeWithMsg "ClientMismatch" r1
  liftIO $ assertBool "client mismatch" (eqMismatch [(eve, Set.singleton ec)] [] [] (Just x))
  -- Fetch all missing clients prekeys
  r2 <-
    post (b . path "/users/prekeys" . json (missingClients x))
      <!! const 200 === statusCode
  let p = responseJsonUnsafeWithMsg "prekeys" r2 :: UserClientMap (Maybe Prekey)
  liftIO $ do
    Map.keys (userClientMap p) @=? [makeIdOpaque eve]
    Map.keys <$> Map.lookup (makeIdOpaque eve) (userClientMap p) @=? Just [ec]

postCryptoMessage4 :: TestM ()
postCryptoMessage4 = do
  alice <- randomUser
  bob <- randomUser
  bc <- randomClient bob (someLastPrekeys !! 0)
  connectUsers alice (list1 bob [])
  conv <- decodeConvId <$> postConv alice [bob] (Just "gossip") [] Nothing Nothing
  -- Unknown client ID => 403
  let ciphertext = encodeCiphertext "hello bob"
  let m = otrRecipients [(bob, [(bc, ciphertext)])]
  postProtoOtrMessage alice (ClientId "172618352518396") conv m
    !!! const 403 === statusCode

postCryptoMessage5 :: TestM ()
postCryptoMessage5 = do
  (alice, ac) <- randomUserWithClient (someLastPrekeys !! 0)
  (bob, bc) <- randomUserWithClient (someLastPrekeys !! 1)
  (eve, ec) <- randomUserWithClient (someLastPrekeys !! 2)
  connectUsers alice (list1 bob [eve])
  conv <- decodeConvId <$> postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
  -- Missing eve
  let m = [(bob, bc, "hello bob")]
  let m' = otrRecipients [(bob, [(bc, encodeCiphertext "hello bob")])]
  -- These three are equivalent (i.e. report all missing clients)
  postOtrMessage id alice ac conv m
    !!! const 412 === statusCode
  postOtrMessage (queryItem "ignore_missing" "false") alice ac conv m
    !!! const 412 === statusCode
  postOtrMessage (queryItem "report_missing" "true") alice ac conv m
    !!! const 412 === statusCode
  -- These two are equivalent (i.e. ignore all missing clients)
  postOtrMessage (queryItem "ignore_missing" "true") alice ac conv m
    !!! const 201 === statusCode
  postOtrMessage (queryItem "report_missing" "false") alice ac conv m
    !!! const 201 === statusCode
  -- Report missing clients of a specific user only
  postOtrMessage (queryItem "report_missing" (toByteString' bob)) alice ac conv m
    !!! const 201 === statusCode
  -- Let's make sure that the same logic using protobuf in the body works too
  postProtoOtrMessage' Nothing (queryItem "report_missing" (toByteString' bob)) alice ac conv m'
    !!! const 201 === statusCode
  -- Body takes precedence
  postOtrMessage' (Just [makeIdOpaque bob]) (queryItem "report_missing" (toByteString' eve)) alice ac conv m
    !!! const 201 === statusCode
  -- Set it only in the body of the message
  postOtrMessage' (Just [makeIdOpaque bob]) id alice ac conv m
    !!! const 201 === statusCode
  -- Let's make sure that protobuf works too, when specified in the body only
  postProtoOtrMessage' (Just [makeIdOpaque bob]) id alice ac conv m'
    !!! const 201 === statusCode
  _rs <-
    postOtrMessage (queryItem "report_missing" (toByteString' eve)) alice ac conv []
      <!! const 412 === statusCode
  let _mm = responseJsonUnsafeWithMsg "ClientMismatch" _rs
  liftIO $ assertBool "client mismatch" (eqMismatch [(eve, Set.singleton ec)] [] [] (Just _mm))
  -- Ignore missing clients of a specific user only
  postOtrMessage (queryItem "ignore_missing" (toByteString' eve)) alice ac conv m
    !!! const 201 === statusCode
  _rs <-
    postOtrMessage (queryItem "ignore_missing" (toByteString' eve)) alice ac conv []
      <!! const 412 === statusCode
  let _mm = responseJsonUnsafeWithMsg "ClientMismatch" _rs
  liftIO $ assertBool "client mismatch" (eqMismatch [(bob, Set.singleton bc)] [] [] (Just _mm))

postJoinConvOk :: TestM ()
postJoinConvOk = do
  c <- view tsCannon
  alice <- randomUser
  bob <- randomUser
  conv <- decodeConvId <$> postConv alice [] (Just "gossip") [InviteAccess, LinkAccess] Nothing Nothing
  WS.bracketR2 c alice bob $ \(wsA, wsB) -> do
    postJoinConv bob conv !!! const 200 === statusCode
    postJoinConv bob conv !!! const 204 === statusCode
    void . liftIO $ WS.assertMatchN (5 # Second) [wsA, wsB] $
      wsAssertMemberJoinWithRole conv bob [bob] roleNameWireMember

postJoinCodeConvOk :: TestM ()
postJoinCodeConvOk = do
  c <- view tsCannon
  alice <- randomUser
  bob <- randomUser
  eve <- ephemeralUser
  dave <- ephemeralUser
  conv <- decodeConvId <$> postConv alice [] (Just "gossip") [CodeAccess] (Just ActivatedAccessRole) Nothing
  cCode <- decodeConvCodeEvent <$> postConvCode alice conv
  -- currently ConversationCode is used both as return type for POST ../code and as body for ../join
  -- POST /code gives code,key,uri
  -- POST /join expects code,key
  -- TODO: Should there be two different types?
  let payload = cCode {conversationUri = Nothing} -- unnecessary step, cCode can be posted as-is also.
      incorrectCode = cCode {conversationCode = Code.Value (unsafeRange (Ascii.encodeBase64Url "incorrect-code"))}
  -- with ActivatedAccess, bob can join, but not eve
  WS.bracketR2 c alice bob $ \(wsA, wsB) -> do
    -- incorrect code/key does not work
    postJoinCodeConv bob incorrectCode !!! const 404 === statusCode
    -- correct code works
    postJoinCodeConv bob payload !!! const 200 === statusCode
    -- test no-op
    postJoinCodeConv bob payload !!! const 204 === statusCode
    -- eve cannot join
    postJoinCodeConv eve payload !!! const 403 === statusCode
    void . liftIO $ WS.assertMatchN (5 # Second) [wsA, wsB] $
      wsAssertMemberJoinWithRole conv bob [bob] roleNameWireMember
    -- changing access to non-activated should give eve access
    let nonActivatedAccess = ConversationAccessUpdate [CodeAccess] NonActivatedAccessRole
    putAccessUpdate alice conv nonActivatedAccess !!! const 200 === statusCode
    postJoinCodeConv eve payload !!! const 200 === statusCode
    -- after removing CodeAccess, no further people can join
    let noCodeAccess = ConversationAccessUpdate [InviteAccess] NonActivatedAccessRole
    putAccessUpdate alice conv noCodeAccess !!! const 200 === statusCode
    postJoinCodeConv dave payload !!! const 404 === statusCode

postConvertCodeConv :: TestM ()
postConvertCodeConv = do
  c <- view tsCannon
  alice <- randomUser
  conv <- decodeConvId <$> postConv alice [] (Just "gossip") [InviteAccess] Nothing Nothing
  -- Cannot do code operations if conversation not in code access
  postConvCode alice conv !!! const 403 === statusCode
  deleteConvCode alice conv !!! const 403 === statusCode
  getConvCode alice conv !!! const 403 === statusCode
  -- cannot change to TeamAccessRole as not a team conversation
  let teamAccess = ConversationAccessUpdate [InviteAccess] TeamAccessRole
  putAccessUpdate alice conv teamAccess !!! const 403 === statusCode
  -- change access
  WS.bracketR c alice $ \wsA -> do
    let nonActivatedAccess = ConversationAccessUpdate [InviteAccess, CodeAccess] NonActivatedAccessRole
    putAccessUpdate alice conv nonActivatedAccess !!! const 200 === statusCode
    -- test no-op
    putAccessUpdate alice conv nonActivatedAccess !!! const 204 === statusCode
    void . liftIO $ WS.assertMatchN (5 # Second) [wsA] $
      wsAssertConvAccessUpdate conv alice nonActivatedAccess
  -- Create/get/update/delete codes
  getConvCode alice conv !!! const 404 === statusCode
  c1 <- decodeConvCodeEvent <$> (postConvCode alice conv <!! const 201 === statusCode)
  postConvCodeCheck c1 !!! const 200 === statusCode
  c1' <- decodeConvCode <$> (getConvCode alice conv <!! const 200 === statusCode)
  liftIO $ assertEqual "c1 c1' codes should match" c1 c1'
  postConvCode alice conv !!! const 200 === statusCode
  c2 <- decodeConvCode <$> (postConvCode alice conv <!! const 200 === statusCode)
  liftIO $ assertEqual "c1 c2 codes should match" c1 c2
  deleteConvCode alice conv !!! const 200 === statusCode
  getConvCode alice conv !!! const 404 === statusCode
  -- create a new code; then revoking CodeAccess should make existing codes invalid
  void $ postConvCode alice conv
  let noCodeAccess = ConversationAccessUpdate [InviteAccess] NonActivatedAccessRole
  putAccessUpdate alice conv noCodeAccess !!! const 200 === statusCode
  getConvCode alice conv !!! const 403 === statusCode

postConvertTeamConv :: TestM ()
postConvertTeamConv = do
  c <- view tsCannon
  -- Create a team conversation with team-alice, team-bob, activated-eve
  -- Non-activated mallory can join
  (alice, tid) <- createBindingTeam
  bob <- view Teams.userId <$> addUserToTeam alice tid
  assertQueue "team member (bob) join" $ tUpdate 2 [alice]
  refreshIndex
  dave <- view Teams.userId <$> addUserToTeam alice tid
  assertQueue "team member (dave) join" $ tUpdate 3 [alice]
  refreshIndex
  eve <- randomUser
  connectUsers alice (singleton eve)
  let acc = Just $ Set.fromList [InviteAccess, CodeAccess]
  -- creating a team-only conversation containing eve should fail
  createTeamConvAccessRaw alice tid [bob, eve] (Just "blaa") acc (Just TeamAccessRole) Nothing Nothing
    !!! const 403 === statusCode
  -- create conversation allowing any type of guest
  conv <- createTeamConvAccess alice tid [bob, eve] (Just "blaa") acc (Just NonActivatedAccessRole) Nothing Nothing
  -- mallory joins by herself
  mallory <- ephemeralUser
  j <- decodeConvCodeEvent <$> postConvCode alice conv
  WS.bracketR3 c alice bob eve $ \(wsA, wsB, wsE) -> do
    postJoinCodeConv mallory j !!! const 200 === statusCode
    void . liftIO $ WS.assertMatchN (5 # Second) [wsA, wsB, wsE] $
      wsAssertMemberJoinWithRole conv mallory [mallory] roleNameWireMember
  WS.bracketRN c [alice, bob, eve, mallory] $ \[wsA, wsB, wsE, wsM] -> do
    let teamAccess = ConversationAccessUpdate [InviteAccess, CodeAccess] TeamAccessRole
    putAccessUpdate alice conv teamAccess !!! const 200 === statusCode
    void . liftIO $ WS.assertMatchN (5 # Second) [wsA, wsB, wsE, wsM] $
      wsAssertConvAccessUpdate conv alice teamAccess
    -- non-team members get kicked out
    void . liftIO $ WS.assertMatchN (5 # Second) [wsA, wsB, wsE, wsM] $
      wsAssertMemberLeave conv alice [eve, mallory]
    -- joining (for mallory) is no longer possible
    postJoinCodeConv mallory j !!! const 403 === statusCode
    -- team members (dave) can still join
    postJoinCodeConv dave j !!! const 200 === statusCode

postJoinConvFail :: TestM ()
postJoinConvFail = do
  alice <- randomUser
  bob <- randomUser
  conv <- decodeConvId <$> postConv alice [] (Just "gossip") [] Nothing Nothing
  void $ postJoinConv bob conv !!! const 403 === statusCode

getConvsOk :: TestM ()
getConvsOk = do
  usr <- randomUser
  getConvs usr Nothing Nothing !!! do
    const 200 === statusCode
    const [toUUID usr] === map (toUUID . cnvId) . decodeConvList

getConvsOk2 :: TestM ()
getConvsOk2 = do
  [alice, bob] <- randomUsers 2
  connectUsers alice (singleton bob)
  -- create & get one2one conv
  cnv1 <- responseJsonUnsafeWithMsg "conversation" <$> postO2OConv alice bob (Just "gossip1")
  getConvs alice (Just $ Left [cnvId cnv1]) Nothing !!! do
    const 200 === statusCode
    const (Just [cnvId cnv1]) === fmap (map cnvId . convList) . responseJsonUnsafe
  -- create & get group conv
  carl <- randomUser
  connectUsers alice (singleton carl)
  cnv2 <- responseJsonUnsafeWithMsg "conversation" <$> postConv alice [bob, carl] (Just "gossip2") [] Nothing Nothing
  getConvs alice (Just $ Left [cnvId cnv2]) Nothing !!! do
    const 200 === statusCode
    const (Just [cnvId cnv2]) === fmap (map cnvId . convList) . responseJsonUnsafe
  -- get both
  rs <- getConvs alice Nothing Nothing <!! const 200 === statusCode
  let cs = convList <$> responseJsonUnsafe rs
  let c1 = cs >>= find ((== cnvId cnv1) . cnvId)
  let c2 = cs >>= find ((== cnvId cnv2) . cnvId)
  liftIO . forM_ [(cnv1, c1), (cnv2, c2)] $ \(expected, actual) -> do
    assertEqual
      "name mismatch"
      (Just $ cnvName expected)
      (cnvName <$> actual)
    assertEqual
      "self member mismatch"
      (Just . cmSelf $ cnvMembers expected)
      (cmSelf . cnvMembers <$> actual)
    assertEqual
      "other members mismatch"
      (Just [])
      ((\c -> cmOthers (cnvMembers c) \\ cmOthers (cnvMembers expected)) <$> actual)

getConvsFailMaxSize :: TestM ()
getConvsFailMaxSize = do
  usr <- randomUser
  getConvs usr Nothing (Just 501)
    !!! const 400 === statusCode

getConvIdsOk :: TestM ()
getConvIdsOk = do
  [alice, bob] <- randomUsers 2
  connectUsers alice (singleton bob)
  void $ postO2OConv alice bob (Just "gossip")
  getConvIds alice Nothing Nothing !!! do
    const 200 === statusCode
    const 2 === length . decodeConvIdList
  getConvIds bob Nothing Nothing !!! do
    const 200 === statusCode
    const 2 === length . decodeConvIdList

paginateConvIds :: TestM ()
paginateConvIds = do
  [alice, bob, eve] <- randomUsers 3
  connectUsers alice (singleton bob)
  connectUsers alice (singleton eve)
  replicateM_ 256 $
    postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
      !!! const 201 === statusCode
  foldM_ (getChunk 16 alice) Nothing [15 .. 0 :: Int]
  where
    getChunk size alice start n = do
      resp <- getConvIds alice start (Just size) <!! const 200 === statusCode
      let c = fromMaybe (ConversationList [] False) (responseJsonUnsafe resp)
      liftIO $ do
        length (convList c) @?= fromIntegral size
        convHasMore c @?= n > 0
      return (Just (Right (last (convList c))))

getConvIdsFailMaxSize :: TestM ()
getConvIdsFailMaxSize = do
  usr <- randomUser
  getConvIds usr Nothing (Just 1001)
    !!! const 400 === statusCode

getConvsPagingOk :: TestM ()
getConvsPagingOk = do
  [ally, bill, carl] <- randomUsers 3
  connectUsers ally (list1 bill [carl])
  replicateM_ 11 $ postConv ally [bill, carl] (Just "gossip") [] Nothing Nothing
  walk ally [3, 3, 3, 3, 2] -- 11 (group) + 2 (1:1) + 1 (self)
  walk bill [3, 3, 3, 3, 1] -- 11 (group) + 1 (1:1) + 1 (self)
  walk carl [3, 3, 3, 3, 1] -- 11 (group) + 1 (1:1) + 1 (self)
  where
    walk u = foldM_ (next u 3) Nothing
    next u step start n = do
      r1 <- getConvIds u (Right <$> start) (Just step) <!! const 200 === statusCode
      let ids1 = convList <$> responseJsonUnsafe r1
      liftIO $ assertEqual "unexpected length (getConvIds)" (Just n) (length <$> ids1)
      r2 <- getConvs u (Right <$> start) (Just step) <!! const 200 === statusCode
      let ids3 = map cnvId . convList <$> responseJsonUnsafe r2
      liftIO $ assertEqual "unexpected length (getConvs)" (Just n) (length <$> ids3)
      liftIO $ assertBool "getConvIds /= getConvs" (ids1 == ids3)
      return $ ids1 >>= listToMaybe . reverse

postConvFailNotConnected :: TestM ()
postConvFailNotConnected = do
  alice <- randomUser
  bob <- randomUser
  jane <- randomUser
  postConv alice [bob, jane] Nothing [] Nothing Nothing !!! do
    const 403 === statusCode
    const (Just "not-connected") === fmap label . responseJsonUnsafe

postConvFailNumMembers :: TestM ()
postConvFailNumMembers = do
  n <- fromIntegral <$> view tsMaxConvSize
  alice <- randomUser
  bob : others <- replicateM n (randomUser)
  connectUsers alice (list1 bob others)
  postConv alice (bob : others) Nothing [] Nothing Nothing !!! do
    const 400 === statusCode
    const (Just "client-error") === fmap label . responseJsonUnsafe

-- | If somebody has blocked a user, that user shouldn't be able to create a
-- group conversation which includes them.
postConvFailBlocked :: TestM ()
postConvFailBlocked = do
  alice <- randomUser
  bob <- randomUser
  jane <- randomUser
  connectUsers alice (list1 bob [jane])
  putConnection jane alice Blocked
    !!! const 200 === statusCode
  postConv alice [bob, jane] Nothing [] Nothing Nothing !!! do
    const 403 === statusCode
    const (Just "not-connected") === fmap label . responseJsonUnsafe

postSelfConvOk :: TestM ()
postSelfConvOk = do
  alice <- randomUser
  m <- postSelfConv alice <!! const 200 === statusCode
  n <- postSelfConv alice <!! const 200 === statusCode
  mId <- assertConv m SelfConv alice alice [] Nothing Nothing
  nId <- assertConv n SelfConv alice alice [] Nothing Nothing
  liftIO $ mId @=? nId

postO2OConvOk :: TestM ()
postO2OConvOk = do
  alice <- randomUser
  bob <- randomUser
  connectUsers alice (singleton bob)
  a <- postO2OConv alice bob (Just "chat") <!! const 200 === statusCode
  c <- postO2OConv alice bob (Just "chat") <!! const 200 === statusCode
  aId <- assertConv a One2OneConv alice alice [bob] (Just "chat") Nothing
  cId <- assertConv c One2OneConv alice alice [bob] (Just "chat") Nothing
  liftIO $ aId @=? cId

postConvO2OFailWithSelf :: TestM ()
postConvO2OFailWithSelf = do
  g <- view tsGalley
  alice <- randomUser
  let inv = NewConvUnmanaged (NewConv [makeIdOpaque alice] Nothing mempty Nothing Nothing Nothing Nothing roleNameWireAdmin)
  post (g . path "/conversations/one2one" . zUser alice . zConn "conn" . zType "access" . json inv) !!! do
    const 403 === statusCode
    const (Just "invalid-op") === fmap label . responseJsonUnsafe

postConnectConvOk :: TestM ()
postConnectConvOk = do
  alice <- randomUser
  bob <- randomUser
  m <-
    postConnectConv alice bob "Alice" "connect with me!" Nothing
      <!! const 201 === statusCode
  n <-
    postConnectConv alice bob "Alice" "connect with me!" Nothing
      <!! const 200 === statusCode
  mId <- assertConv m ConnectConv alice alice [] (Just "Alice") Nothing
  nId <- assertConv n ConnectConv alice alice [] (Just "Alice") Nothing
  liftIO $ mId @=? nId

postConnectConvOk2 :: TestM ()
postConnectConvOk2 = do
  alice <- randomUser
  bob <- randomUser
  m <- decodeConvId <$> request alice bob
  n <- decodeConvId <$> request alice bob
  liftIO $ m @=? n
  where
    request alice bob =
      postConnectConv alice bob "Alice" "connect with me!" (Just "me@me.com")

putConvAcceptOk :: TestM ()
putConvAcceptOk = do
  alice <- randomUser
  bob <- randomUser
  cnv <- decodeConvId <$> postConnectConv alice bob "Alice" "come to zeta!" Nothing
  putConvAccept bob cnv !!! const 200 === statusCode
  getConv alice cnv !!! do
    const 200 === statusCode
    const (Just One2OneConv) === fmap cnvType . responseJsonUnsafe
  getConv bob cnv !!! do
    const 200 === statusCode
    const (Just One2OneConv) === fmap cnvType . responseJsonUnsafe

putConvAcceptRetry :: TestM ()
putConvAcceptRetry = do
  alice <- randomUser
  bob <- randomUser
  connectUsers alice (singleton bob)
  cnv <- decodeConvId <$> postO2OConv alice bob (Just "chat")
  -- If the conversation type is already One2One, everything is 200 OK
  putConvAccept bob cnv !!! const 200 === statusCode

postMutualConnectConvOk :: TestM ()
postMutualConnectConvOk = do
  alice <- randomUser
  bob <- randomUser
  ac <-
    postConnectConv alice bob "A" "a" Nothing
      <!! const 201 === statusCode
  acId <- assertConv ac ConnectConv alice alice [] (Just "A") Nothing
  bc <-
    postConnectConv bob alice "B" "b" Nothing
      <!! const 200 === statusCode
  -- The connect conversation was simply accepted, thus the
  -- conversation name and message sent in Bob's request ignored.
  bcId <- assertConv bc One2OneConv alice bob [alice] (Just "A") Nothing
  liftIO $ acId @=? bcId

postRepeatConnectConvCancel :: TestM ()
postRepeatConnectConvCancel = do
  alice <- randomUser
  bob <- randomUser
  -- Alice wants to connect
  rsp1 <- postConnectConv alice bob "A" "a" Nothing <!! const 201 === statusCode
  let cnv = responseJsonUnsafeWithMsg "conversation" rsp1
  liftIO $ do
    ConnectConv @=? cnvType cnv
    (Just "A") @=? cnvName cnv
    [] @=? cmOthers (cnvMembers cnv)
    privateAccess @=? cnvAccess cnv
  -- Alice blocks / cancels
  cancel alice cnv
  -- Alice makes another connect attempt
  rsp2 <- postConnectConv alice bob "A2" "a2" Nothing <!! const 200 === statusCode
  let cnv2 = responseJsonUnsafeWithMsg "conversation" rsp2
  liftIO $ do
    ConnectConv @=? cnvType cnv2
    (Just "A2") @=? cnvName cnv2
    [] @=? cmOthers (cnvMembers cnv2)
    privateAccess @=? cnvAccess cnv2
  -- Alice blocks / cancels again
  cancel alice cnv
  -- Now Bob attempts to connect
  rsp3 <- postConnectConv bob alice "B" "b" Nothing <!! const 200 === statusCode
  let cnv3 = responseJsonUnsafeWithMsg "conversation" rsp3
  liftIO $ do
    ConnectConv @=? cnvType cnv3
    (Just "B") @=? cnvName cnv3
    privateAccess @=? cnvAccess cnv3
  -- Bob accepting is a no-op, since he is already a member
  putConvAccept bob (cnvId cnv) !!! const 200 === statusCode
  cnvX <- responseJsonUnsafeWithMsg "conversation" <$> getConv bob (cnvId cnv)
  liftIO $ do
    ConnectConv @=? cnvType cnvX
    (Just "B") @=? cnvName cnvX
    privateAccess @=? cnvAccess cnvX
  -- Alice accepts, finally turning it into a 1-1
  putConvAccept alice (cnvId cnv) !!! const 200 === statusCode
  cnv4 <- responseJsonUnsafeWithMsg "conversation" <$> getConv alice (cnvId cnv)
  liftIO $ do
    One2OneConv @=? cnvType cnv4
    (Just "B") @=? cnvName cnv4
    privateAccess @=? cnvAccess cnv4
  where
    cancel u c = do
      g <- view tsGalley
      put (g . paths ["/i/conversations", toByteString' (cnvId c), "block"] . zUser u)
        !!! const 200 === statusCode
      getConv u (cnvId c) !!! const 403 === statusCode

putBlockConvOk :: TestM ()
putBlockConvOk = do
  g <- view tsGalley
  alice <- randomUser
  bob <- randomUser
  conv <- responseJsonUnsafeWithMsg "conversation" <$> postConnectConv alice bob "Alice" "connect with me!" (Just "me@me.com")
  getConv alice (cnvId conv) !!! const 200 === statusCode
  getConv bob (cnvId conv) !!! const 403 === statusCode
  put (g . paths ["/i/conversations", toByteString' (cnvId conv), "block"] . zUser bob)
    !!! const 200 === statusCode
  -- A is still the only member of the 1-1
  getConv alice (cnvId conv) !!! do
    const 200 === statusCode
    const (cnvMembers conv) === cnvMembers . responseJsonUnsafeWithMsg "conversation"
  -- B accepts the conversation by unblocking
  put (g . paths ["/i/conversations", toByteString' (cnvId conv), "unblock"] . zUser bob)
    !!! const 200 === statusCode
  getConv bob (cnvId conv) !!! const 200 === statusCode
  -- B blocks A in the 1-1
  put (g . paths ["/i/conversations", toByteString' (cnvId conv), "block"] . zUser bob)
    !!! const 200 === statusCode
  -- B no longer sees the 1-1
  getConv bob (cnvId conv) !!! const 403 === statusCode
  -- B unblocks A in the 1-1
  put (g . paths ["/i/conversations", toByteString' (cnvId conv), "unblock"] . zUser bob)
    !!! const 200 === statusCode
  -- B sees the blocked 1-1 again
  getConv bob (cnvId conv) !!! do
    const 200 === statusCode

getConvOk :: TestM ()
getConvOk = do
  alice <- randomUser
  bob <- randomUser
  chuck <- randomUser
  connectUsers alice (list1 bob [chuck])
  conv <- decodeConvId <$> postConv alice [bob, chuck] (Just "gossip") [] Nothing Nothing
  getConv alice conv !!! const 200 === statusCode
  getConv bob conv !!! const 200 === statusCode
  getConv chuck conv !!! const 200 === statusCode

accessConvMeta :: TestM ()
accessConvMeta = do
  g <- view tsGalley
  alice <- randomUser
  bob <- randomUser
  chuck <- randomUser
  connectUsers alice (list1 bob [chuck])
  conv <- decodeConvId <$> postConv alice [bob, chuck] (Just "gossip") [] Nothing Nothing
  let meta = ConversationMeta conv RegularConv alice [InviteAccess] ActivatedAccessRole (Just "gossip") Nothing Nothing Nothing
  get (g . paths ["i/conversations", toByteString' conv, "meta"] . zUser alice) !!! do
    const 200 === statusCode
    const (Just meta) === (decode <=< responseBody)

leaveConnectConversation :: TestM ()
leaveConnectConversation = do
  alice <- randomUser
  bob <- randomUser
  bdy <- postConnectConv alice bob "alice" "ni" Nothing <!! const 201 === statusCode
  let c = fromMaybe (error "invalid connect conversation") (cnvId <$> responseJsonUnsafe bdy)
  deleteMember alice alice c !!! const 403 === statusCode

postMembersOk :: TestM ()
postMembersOk = do
  alice <- randomUser
  bob <- randomUser
  chuck <- randomUser
  eve <- randomUser
  connectUsers alice (list1 bob [chuck, eve])
  connectUsers eve (singleton bob)
  conv <- decodeConvId <$> postConv alice [bob, chuck] (Just "gossip") [] Nothing Nothing
  postMembers alice (singleton eve) conv !!! const 200 === statusCode
  -- Check that last_event markers are set for all members
  forM_ [alice, bob, chuck, eve] $ \u -> do
    _ <- getSelfMember u conv <!! const 200 === statusCode
    return ()

postMembersOk2 :: TestM ()
postMembersOk2 = do
  alice <- randomUser
  bob <- randomUser
  chuck <- randomUser
  connectUsers alice (list1 bob [chuck])
  connectUsers bob (singleton chuck)
  conv <- decodeConvId <$> postConv alice [bob, chuck] Nothing [] Nothing Nothing
  postMembers bob (singleton chuck) conv !!! const 204 === statusCode
  chuck' <- responseJsonUnsafe <$> (getSelfMember chuck conv <!! const 200 === statusCode)
  liftIO $
    assertEqual "wrong self member" (Just (makeIdOpaque chuck)) (memId <$> chuck')

postMembersOk3 :: TestM ()
postMembersOk3 = do
  alice <- randomUser
  bob <- randomUser
  eve <- randomUser
  connectUsers alice (list1 bob [eve])
  conv <- decodeConvId <$> postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
  -- Bob leaves
  deleteMember bob bob conv !!! const 200 === statusCode
  -- Fetch bob
  getSelfMember bob conv !!! const 200 === statusCode
  -- Alice re-adds Bob to the conversation
  postMembers alice (singleton bob) conv !!! const 200 === statusCode
  -- Fetch bob again
  getSelfMember bob conv !!! const 200 === statusCode

postMembersFail :: TestM ()
postMembersFail = do
  alice <- randomUser
  bob <- randomUser
  chuck <- randomUser
  dave <- randomUser
  eve <- randomUser
  connectUsers alice (list1 bob [chuck, eve])
  connectUsers eve (singleton bob)
  conv <- decodeConvId <$> postConv alice [bob, chuck] (Just "gossip") [] Nothing Nothing
  postMembers eve (singleton bob) conv !!! const 404 === statusCode
  postMembers alice (singleton eve) conv !!! const 200 === statusCode
  -- Not connected but already there
  postMembers chuck (singleton eve) conv !!! const 204 === statusCode
  postMembers chuck (singleton dave) conv !!! do
    const 403 === statusCode
    const (Just "not-connected") === fmap label . responseJsonUnsafe
  void $ connectUsers chuck (singleton dave)
  postMembers chuck (singleton dave) conv !!! const 200 === statusCode
  postMembers chuck (singleton dave) conv !!! const 204 === statusCode

postTooManyMembersFail :: TestM ()
postTooManyMembersFail = do
  n <- fromIntegral <$> view tsMaxConvSize
  alice <- randomUser
  bob <- randomUser
  chuck <- randomUser
  connectUsers alice (list1 bob [chuck])
  conv <- decodeConvId <$> postConv alice [bob, chuck] (Just "gossip") [] Nothing Nothing
  x : xs <- randomUsers (n - 2)
  postMembers chuck (list1 x xs) conv !!! do
    const 403 === statusCode
    const (Just "too-many-members") === fmap label . responseJsonUnsafe

deleteMembersOk :: TestM ()
deleteMembersOk = do
  alice <- randomUser
  bob <- randomUser
  eve <- randomUser
  connectUsers alice (list1 bob [eve])
  conv <- decodeConvId <$> postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
  deleteMember bob bob conv !!! const 200 === statusCode
  deleteMember bob bob conv !!! const 404 === statusCode
  -- if conversation still exists, don't respond with 404, but with 403.
  getConv bob conv !!! const 403 === statusCode
  deleteMember alice eve conv !!! const 200 === statusCode
  deleteMember alice eve conv !!! const 204 === statusCode
  deleteMember alice alice conv !!! const 200 === statusCode
  deleteMember alice alice conv !!! const 404 === statusCode

deleteMembersFailSelf :: TestM ()
deleteMembersFailSelf = do
  alice <- randomUser
  self <- decodeConvId <$> postSelfConv alice
  deleteMember alice alice self !!! const 403 === statusCode

deleteMembersFailO2O :: TestM ()
deleteMembersFailO2O = do
  alice <- randomUser
  bob <- randomUser
  connectUsers alice (singleton bob)
  o2o <- decodeConvId <$> postO2OConv alice bob (Just "foo")
  deleteMember alice bob o2o !!! const 403 === statusCode

putConvRenameOk :: TestM ()
putConvRenameOk = do
  c <- view tsCannon
  alice <- randomUser
  bob <- randomUser
  connectUsers alice (singleton bob)
  conv <- decodeConvId <$> postO2OConv alice bob (Just "gossip")
  -- This endpoint should be deprecated but clients still use it
  WS.bracketR2 c alice bob $ \(wsA, wsB) -> do
    void $ putConversationName bob conv "gossip++" !!! const 200 === statusCode
    void . liftIO . WS.assertMatchN (5 # Second) [wsA, wsB] $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= conv
      evtType e @?= ConvRename
      evtFrom e @?= bob
      evtData e @?= Just (EdConvRename (ConversationRename "gossip++"))

putMemberOtrMuteOk :: TestM ()
putMemberOtrMuteOk = do
  putMemberOk (memberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Just 0, mupOtrMuteRef = Just "ref"})
  putMemberOk (memberUpdate {mupOtrMute = Just False})

putMemberOtrArchiveOk :: TestM ()
putMemberOtrArchiveOk = do
  putMemberOk (memberUpdate {mupOtrArchive = Just True, mupOtrArchiveRef = Just "ref"})
  putMemberOk (memberUpdate {mupOtrArchive = Just False})

putMemberHiddenOk :: TestM ()
putMemberHiddenOk = do
  putMemberOk (memberUpdate {mupHidden = Just True, mupHiddenRef = Just "ref"})
  putMemberOk (memberUpdate {mupHidden = Just False})

putMemberAllOk :: TestM ()
putMemberAllOk =
  putMemberOk
    ( memberUpdate
        { mupOtrMute = Just True,
          mupOtrMuteStatus = Just 0,
          mupOtrMuteRef = Just "mref",
          mupOtrArchive = Just True,
          mupOtrArchiveRef = Just "aref",
          mupHidden = Just True,
          mupHiddenRef = Just "href"
        }
    )

putMemberOk :: MemberUpdate -> TestM ()
putMemberOk update = do
  c <- view tsCannon
  alice <- randomUser
  bob <- randomUser
  connectUsers alice (singleton bob)
  conv <- decodeConvId <$> postO2OConv alice bob (Just "gossip")
  getConv alice conv !!! const 200 === statusCode
  -- Expected member state
  let memberBob =
        Member
          { memId = makeIdOpaque bob,
            memService = Nothing,
            memOtrMuted = fromMaybe False (mupOtrMute update),
            memOtrMutedStatus = mupOtrMuteStatus update,
            memOtrMutedRef = mupOtrMuteRef update,
            memOtrArchived = fromMaybe False (mupOtrArchive update),
            memOtrArchivedRef = mupOtrArchiveRef update,
            memHidden = fromMaybe False (mupHidden update),
            memHiddenRef = mupHiddenRef update,
            memConvRoleName = fromMaybe roleNameWireAdmin (mupConvRoleName update)
          }
  -- Update member state & verify push notification
  WS.bracketR c bob $ \ws -> do
    putMember bob update conv !!! const 200 === statusCode
    void . liftIO . WS.assertMatch (5 # Second) ws $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= conv
      evtType e @?= MemberStateUpdate
      evtFrom e @?= bob
      case evtData e of
        Just (EdMemberUpdate mis) -> do
          assertEqual "otr_muted" (mupOtrMute update) (misOtrMuted mis)
          assertEqual "otr_muted_ref" (mupOtrMuteRef update) (misOtrMutedRef mis)
          assertEqual "otr_archived" (mupOtrArchive update) (misOtrArchived mis)
          assertEqual "otr_archived_ref" (mupOtrArchiveRef update) (misOtrArchivedRef mis)
          assertEqual "hidden" (mupHidden update) (misHidden mis)
          assertEqual "hidden_ref" (mupHiddenRef update) (misHiddenRef mis)
        x -> assertFailure $ "Unexpected event data: " ++ show x
  -- Verify new member state
  rs <- getConv bob conv <!! const 200 === statusCode
  let bob' = cmSelf . cnvMembers <$> responseJsonUnsafe rs
  liftIO $ do
    assertBool "user" (isJust bob')
    let newBob = fromJust bob'
    assertEqual "id" (memId memberBob) (memId newBob)
    assertEqual "otr_muted" (memOtrMuted memberBob) (memOtrMuted newBob)
    assertEqual "otr_muted_ref" (memOtrMutedRef memberBob) (memOtrMutedRef newBob)
    assertEqual "otr_archived" (memOtrArchived memberBob) (memOtrArchived newBob)
    assertEqual "otr_archived_ref" (memOtrArchivedRef memberBob) (memOtrArchivedRef newBob)
    assertEqual "hidden" (memHidden memberBob) (memHidden newBob)
    assertEqual "hidden__ref" (memHiddenRef memberBob) (memHiddenRef newBob)

putReceiptModeOk :: TestM ()
putReceiptModeOk = do
  c <- view tsCannon
  alice <- randomUser
  bob <- randomUser
  jane <- randomUser
  connectUsers alice (list1 bob [jane])
  cnv <- decodeConvId <$> postConv alice [bob, jane] (Just "gossip") [] Nothing Nothing
  WS.bracketR3 c alice bob jane $ \(_wsA, wsB, _wsJ) -> do
    -- By default, nothing is set
    getConv alice cnv !!! do
      const 200 === statusCode
      const (Just Nothing) === fmap cnvReceiptMode . responseJsonUnsafe
    -- Set receipt mode
    putReceiptMode alice cnv (ReceiptMode 0) !!! const 200 === statusCode
    -- Ensure the field is properly set
    getConv alice cnv !!! do
      const 200 === statusCode
      const (Just $ Just (ReceiptMode 0)) === fmap cnvReceiptMode . responseJsonUnsafe
    void . liftIO $ checkWs alice (cnv, wsB)
    -- No changes
    putReceiptMode alice cnv (ReceiptMode 0) !!! const 204 === statusCode
    -- No event should have been generated
    WS.assertNoEvent (1 # Second) [wsB]
    -- Ensure that the new field remains unchanged
    getConv alice cnv !!! do
      const 200 === statusCode
      const (Just $ Just (ReceiptMode 0)) === fmap cnvReceiptMode . responseJsonUnsafe
  cnv' <- decodeConvId <$> postConvWithReceipt alice [bob, jane] (Just "gossip") [] Nothing Nothing (ReceiptMode 0)
  getConv alice cnv' !!! do
    const 200 === statusCode
    const (Just (Just (ReceiptMode 0))) === fmap cnvReceiptMode . responseJsonUnsafe
  where
    checkWs alice (cnv, ws) = WS.awaitMatch (5 # Second) ws $ \n -> do
      ntfTransient n @?= False
      let e = List1.head (WS.unpackPayload n)
      evtConv e @?= cnv
      evtType e @?= ConvReceiptModeUpdate
      evtFrom e @?= alice
      case evtData e of
        Just (EdConvReceiptModeUpdate (ConversationReceiptModeUpdate (ReceiptMode mode))) ->
          assertEqual "modes should match" mode 0
        _ -> assertFailure "Unexpected event data"

postTypingIndicators :: TestM ()
postTypingIndicators = do
  g <- view tsGalley
  alice <- randomUser
  bob <- randomUser
  connectUsers alice (singleton bob)
  conv <- decodeConvId <$> postO2OConv alice bob Nothing
  post
    ( g
        . paths ["conversations", toByteString' conv, "typing"]
        . zUser bob
        . zConn "conn"
        . zType "access"
        . json (TypingData StartedTyping)
    )
    !!! const 200 === statusCode
  post
    ( g
        . paths ["conversations", toByteString' conv, "typing"]
        . zUser bob
        . zConn "conn"
        . zType "access"
        . json (object ["status" .= ("dummy" :: T.Text)])
    )
    !!! const 400 === statusCode

removeUser :: TestM ()
removeUser = do
  c <- view tsCannon
  alice <- randomUser
  bob <- randomUser
  carl <- randomUser
  connectUsers alice (list1 bob [carl])
  conv1 <- decodeConvId <$> postConv alice [bob] (Just "gossip") [] Nothing Nothing
  conv2 <- decodeConvId <$> postConv alice [bob, carl] (Just "gossip2") [] Nothing Nothing
  conv3 <- decodeConvId <$> postConv alice [carl] (Just "gossip3") [] Nothing Nothing
  WS.bracketR3 c alice bob carl $ \(wsA, wsB, wsC) -> do
    deleteUser bob
    void . liftIO $ WS.assertMatchN (5 # Second) [wsA, wsB] $
      matchMemberLeave conv1 bob
    void . liftIO $ WS.assertMatchN (5 # Second) [wsA, wsB, wsC] $
      matchMemberLeave conv2 bob
  -- Check memberships
  mems1 <- fmap cnvMembers . responseJsonUnsafe <$> getConv alice conv1
  mems2 <- fmap cnvMembers . responseJsonUnsafe <$> getConv alice conv2
  mems3 <- fmap cnvMembers . responseJsonUnsafe <$> getConv alice conv3
  let other u = find ((== makeIdOpaque u) . omId) . cmOthers
  liftIO $ do
    (mems1 >>= other bob) @?= Nothing
    (mems2 >>= other bob) @?= Nothing
    (mems2 >>= other carl) @?= Just (OtherMember (makeIdOpaque carl) Nothing roleNameWireAdmin)
    (mems3 >>= other bob) @?= Nothing
    (mems3 >>= other carl) @?= Just (OtherMember (makeIdOpaque carl) Nothing roleNameWireAdmin)
  where
    matchMemberLeave conv u n = do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= conv
      evtType e @?= MemberLeave
      evtFrom e @?= u
      evtData e @?= Just (EdMembersLeave (UserIdList [u]))
