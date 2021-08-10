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
import qualified API.Federation as Federation
import qualified API.MessageTimer as MessageTimer
import qualified API.Roles as Roles
import API.SQS
import qualified API.Teams as Teams
import qualified API.Teams.Feature as TeamFeature
import qualified API.Teams.LegalHold as Teams.LegalHold
import qualified API.Teams.LegalHold.DisabledByDefault
import API.Util
import Bilge hiding (timeout)
import Bilge.Assert
import Brig.Types
import qualified Cassandra as Cql
import qualified Control.Concurrent.Async as Async
import Control.Lens (at, ix, preview, view, (.~), (?~), (^.))
import Control.Monad.Except (MonadError (throwError))
import Data.Aeson hiding (json)
import qualified Data.ByteString as BS
import Data.ByteString.Conversion
import qualified Data.Code as Code
import Data.Domain (Domain (Domain), domainText)
import Data.Id
import Data.Json.Util (toBase64Text, toUTCTimeMillis)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List1
import qualified Data.List1 as List1
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import qualified Data.Text as T
import qualified Data.Text.Ascii as Ascii
import Data.Time.Clock (getCurrentTime)
import qualified Galley.Data as Cql
import Galley.Options (Opts, optFederator)
import Galley.Types hiding (InternalMember (..))
import Galley.Types.Conversations.Roles
import qualified Galley.Types.Teams as Teams
import Gundeck.Types.Notification
import Imports
import Network.Wai.Utilities.Error
import Servant (ServerError (errBody), err501, err503)
import Servant.Server (Handler)
import Servant.Server.Generic (AsServerT)
import Test.QuickCheck (arbitrary, generate)
import Test.Tasty
import Test.Tasty.Cannon (TimeoutUnit (..), (#))
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Util.Options (Endpoint (Endpoint))
import Wire.API.Conversation
import qualified Wire.API.Federation.API.Brig as FederatedBrig
import Wire.API.Federation.API.Galley (GetConversationsResponse (..))
import qualified Wire.API.Federation.API.Galley as FederatedGalley
import qualified Wire.API.Federation.GRPC.Types as F
import qualified Wire.API.Message as Message
import Wire.API.User.Client
  ( QualifiedUserClients (..),
    UserClientPrekeyMap,
    getUserClientPrekeyMap,
  )
import Wire.API.UserMap (UserMap (..))

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "Galley integration tests"
    [ Teams.LegalHold.tests s,
      API.Teams.LegalHold.DisabledByDefault.tests s,
      mainTests,
      Teams.tests s,
      MessageTimer.tests s,
      Roles.tests s,
      CustomBackend.tests s,
      TeamFeature.tests s,
      Federation.tests s
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
          test s "list-conversations by ids" listConvsOk2,
          test s "fail to get >500 conversations" getConvsFailMaxSize,
          test s "get conversation ids" getConvIdsOk,
          test s "get conversation ids v2" listConvIdsOk,
          test s "paginate through conversation ids" paginateConvIds,
          test s "paginate through /conversations/list-ids" paginateConvListIds,
          test s "paginate through /conversations/list-ids - page ending at locals and remote domain" paginateConvListIdsPageEndingAtLocalsAndDomain,
          test s "fail to get >1000 conversation ids" getConvIdsFailMaxSize,
          test s "page through conversations" getConvsPagingOk,
          test s "fail to create conversation when not connected" postConvFailNotConnected,
          test s "fail to create conversation with qualified users when not connected" postConvQualifiedFailNotConnected,
          test s "M:N conversation creation must have <N members" postConvFailNumMembers,
          test s "M:N conversation creation must have <N qualified members" postConvQualifiedFailNumMembers,
          test s "fail to create conversation when blocked" postConvFailBlocked,
          test s "fail to create conversation when blocked by qualified member" postConvQualifiedFailBlocked,
          test s "fail to create conversation with remote users when remote user's domain doesn't exist" postConvQualifiedNonExistentDomain,
          test s "fail to create conversation with remote users when remote user doesn't exist" postConvQualifiedNonExistentUser,
          test s "fail to create conversation with remote users when federation not configured" postConvQualifiedFederationNotEnabled,
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
          test s "get qualified conversation" getConvQualifiedOk,
          test s "conversation meta access" accessConvMeta,
          test s "add members" postMembersOk,
          test s "add existing members" postMembersOk2,
          test s "add past members" postMembersOk3,
          test s "fail to add members when not connected" postMembersFail,
          test s "fail to add too many members" postTooManyMembersFail,
          test s "add remote members" testAddRemoteMember,
          test s "get and list remote conversations" testGetRemoteConversations,
          test s "add non-existing remote members" testAddRemoteMemberFailure,
          test s "add deleted remote members" testAddDeletedRemoteUser,
          test s "add remote members on invalid domain" testAddRemoteMemberInvalidDomain,
          test s "add remote members when federation isn't enabled" testAddRemoteMemberFederationDisabled,
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
          test s "post conversations/:cnv/otr/message: message delivery and missing clients" postCryptoMessage1,
          test s "post conversations/:cnv/otr/message: mismatch and prekey fetching" postCryptoMessage2,
          test s "post conversations/:cnv/otr/message: mismatch with protobuf" postCryptoMessage3,
          test s "post conversations/:cnv/otr/message: unknown sender client" postCryptoMessage4,
          test s "post conversations/:cnv/otr/message: ignore_missing and report_missing" postCryptoMessage5,
          test s "post message qualified - local owning backend - success" postMessageQualifiedLocalOwningBackendSuccess,
          test s "post message qualified - local owning backend - missing clients" postMessageQualifiedLocalOwningBackendMissingClients,
          test s "post message qualified - local owning backend - redundant and deleted clients" postMessageQualifiedLocalOwningBackendRedundantAndDeletedClients,
          test s "post message qualified - local owning backend - ignore missing" postMessageQualifiedLocalOwningBackendIgnoreMissingClients,
          test s "post message qualified - local owning backend - failed to send clients" postMessageQualifiedLocalOwningBackendFailedToSendClients,
          test s "post message qualified - remote owning backend - federation failure" postMessageQualifiedRemoteOwningBackendFailure,
          test s "post message qualified - remote owning backend - success" postMessageQualifiedRemoteOwningBackendSuccess,
          test s "join conversation" postJoinConvOk,
          test s "get code-access conversation information" testJoinCodeConv,
          test s "join code-access conversation" postJoinCodeConvOk,
          test s "convert invite to code-access conversation" postConvertCodeConv,
          test s "convert code to team-access conversation" postConvertTeamConv,
          test s "cannot join private conversation" postJoinConvFail,
          test s "remove user" removeUser
        ]

emptyFederatedBrig :: FederatedBrig.Api (AsServerT Handler)
emptyFederatedBrig =
  let e :: Text -> Handler a
      e s = throwError err501 {errBody = cs ("mock not implemented: " <> s)}
   in FederatedBrig.Api
        { FederatedBrig.getUserByHandle = \_ -> e "getUserByHandle",
          FederatedBrig.getUsersByIds = \_ -> e "getUsersByIds",
          FederatedBrig.claimPrekey = \_ -> e "claimPrekey",
          FederatedBrig.claimPrekeyBundle = \_ -> e "claimPrekeyBundle",
          FederatedBrig.claimMultiPrekeyBundle = \_ -> e "claimMultiPrekeyBundle",
          FederatedBrig.searchUsers = \_ -> e "searchUsers",
          FederatedBrig.getUserClients = \_ -> e "getUserClients"
        }

emptyFederatedGalley :: FederatedGalley.Api (AsServerT Handler)
emptyFederatedGalley =
  let e :: Text -> Handler a
      e s = throwError err501 {errBody = cs ("mock not implemented: " <> s)}
   in FederatedGalley.Api
        { FederatedGalley.registerConversation = \_ -> e "registerConversation",
          FederatedGalley.getConversations = \_ -> e "getConversations",
          FederatedGalley.updateConversationMemberships = \_ -> e "updateConversationMemberships",
          FederatedGalley.receiveMessage = \_ _ -> e "receiveMessage",
          FederatedGalley.sendMessage = \_ _ -> e "sendMessage"
        }

-------------------------------------------------------------------------------
-- API Tests

status :: TestM ()
status = do
  g <- view tsGalley
  get (g . path "/i/status")
    !!! const 200 === statusCode
  Bilge.head (g . path "/i/status")
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
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice
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
    print rsp
    cid <- assertConv rsp RegularConv alice alice [bob, jane] (Just nameMaxSize) Nothing
    cvs <- mapM (convView cid) [alice, bob, jane]
    liftIO $ mapM_ WS.assertSuccess =<< Async.mapConcurrently (checkWs qalice) (zip cvs [wsA, wsB, wsJ])
  where
    convView cnv usr = responseJsonUnsafeWithMsg "conversation" <$> getConv usr cnv
    checkWs qalice (cnv, ws) = WS.awaitMatch (5 # Second) ws $ \n -> do
      ntfTransient n @?= False
      let e = List1.head (WS.unpackPayload n)
      evtConv e @?= cnvQualifiedId cnv
      evtType e @?= ConvCreate
      evtFrom e @?= qalice
      case evtData e of
        EdConversation c' -> assertConvEquals cnv c'
        _ -> assertFailure "Unexpected event data"

-- | This test verifies whether a message actually gets sent all the way to
-- cannon.
postCryptoMessage1 :: TestM ()
postCryptoMessage1 = do
  localDomain <- viewFederationDomain
  c <- view tsCannon
  (alice, ac) <- randomUserWithClient (someLastPrekeys !! 0)
  (bob, bc) <- randomUserWithClient (someLastPrekeys !! 1)
  (eve, ec) <- randomUserWithClient (someLastPrekeys !! 2)
  connectUsers alice (list1 bob [eve])
  conv <- decodeConvId <$> postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
  let qalice = Qualified alice localDomain
      qconv = Qualified conv localDomain
  -- WS receive timeout
  let t = 5 # Second
  -- Missing eve
  let m1 = [(bob, bc, "ciphertext1")]
  postOtrMessage id alice ac conv m1 !!! do
    const 412 === statusCode
    assertMismatch [(eve, Set.singleton ec)] [] []
  -- Complete
  WS.bracketR2 c bob eve $ \(wsB, wsE) -> do
    let m2 = [(bob, bc, toBase64Text "ciphertext2"), (eve, ec, toBase64Text "ciphertext2")]
    postOtrMessage id alice ac conv m2 !!! do
      const 201 === statusCode
      assertMismatch [] [] []
    void . liftIO $ WS.assertMatch t wsB (wsAssertOtr qconv qalice ac bc (toBase64Text "ciphertext2"))
    void . liftIO $ WS.assertMatch t wsE (wsAssertOtr qconv qalice ac ec (toBase64Text "ciphertext2"))
  -- Redundant self
  WS.bracketR3 c alice bob eve $ \(wsA, wsB, wsE) -> do
    let m3 = [(alice, ac, toBase64Text "ciphertext3"), (bob, bc, toBase64Text "ciphertext3"), (eve, ec, toBase64Text "ciphertext3")]
    postOtrMessage id alice ac conv m3 !!! do
      const 201 === statusCode
      assertMismatch [] [(alice, Set.singleton ac)] []
    void . liftIO $ WS.assertMatch t wsB (wsAssertOtr qconv qalice ac bc (toBase64Text "ciphertext3"))
    void . liftIO $ WS.assertMatch t wsE (wsAssertOtr qconv qalice ac ec (toBase64Text "ciphertext3"))
    -- Alice should not get it
    assertNoMsg wsA (wsAssertOtr qconv qalice ac ac (toBase64Text "ciphertext3"))
  -- Deleted eve
  WS.bracketR2 c bob eve $ \(wsB, wsE) -> do
    deleteClient eve ec (Just defPassword) !!! const 200 === statusCode
    let m4 = [(bob, bc, toBase64Text "ciphertext4"), (eve, ec, toBase64Text "ciphertext4")]
    postOtrMessage id alice ac conv m4 !!! do
      const 201 === statusCode
      assertMismatch [] [] [(eve, Set.singleton ec)]
    void . liftIO $ WS.assertMatch t wsB (wsAssertOtr qconv qalice ac bc (toBase64Text "ciphertext4"))
    -- Eve should not get it
    assertNoMsg wsE (wsAssertOtr qconv qalice ac ec (toBase64Text "ciphertext4"))
  -- Deleted eve & redundant self
  WS.bracketR3 c alice bob eve $ \(wsA, wsB, wsE) -> do
    let m5 = [(bob, bc, toBase64Text "ciphertext5"), (eve, ec, toBase64Text "ciphertext5"), (alice, ac, toBase64Text "ciphertext5")]
    postOtrMessage id alice ac conv m5 !!! do
      const 201 === statusCode
      assertMismatch [] [(alice, Set.singleton ac)] [(eve, Set.singleton ec)]
    void . liftIO $ WS.assertMatch t wsB (wsAssertOtr qconv qalice ac bc (toBase64Text "ciphertext5"))
    -- Neither Alice nor Eve should get it
    assertNoMsg wsA (wsAssertOtr qconv qalice ac ac (toBase64Text "ciphertext5"))
    assertNoMsg wsE (wsAssertOtr qconv qalice ac ec (toBase64Text "ciphertext5"))
  -- Missing Bob, deleted eve & redundant self
  let m6 = [(eve, ec, toBase64Text "ciphertext6"), (alice, ac, toBase64Text "ciphertext6")]
  postOtrMessage id alice ac conv m6 !!! do
    const 412 === statusCode
    assertMismatch
      [(bob, Set.singleton bc)]
      [(alice, Set.singleton ac)]
      [(eve, Set.singleton ec)]
  -- A second client for Bob
  bc2 <- randomClient bob (someLastPrekeys !! 3)
  -- The first client listens for all messages of Bob
  WS.bracketR c bob $ \wsB -> do
    let cipher = toBase64Text "ciphertext7"
    -- The second client listens only for his own messages
    WS.bracketR (c . queryItem "client" (toByteString' bc2)) bob $ \wsB2 -> do
      let m7 = [(bob, bc, cipher), (bob, bc2, cipher)]
      postOtrMessage id alice ac conv m7 !!! do
        const 201 === statusCode
        assertMismatch [] [] []
      -- Bob's first client gets both messages
      void . liftIO $ WS.assertMatch t wsB (wsAssertOtr qconv qalice ac bc cipher)
      void . liftIO $ WS.assertMatch t wsB (wsAssertOtr qconv qalice ac bc2 cipher)
      -- Bob's second client gets only the message destined for him
      void . liftIO $ WS.assertMatch t wsB2 (wsAssertOtr qconv qalice ac bc2 cipher)
      liftIO $ assertBool "unexpected equal clients" (bc /= bc2)
      assertNoMsg wsB2 (wsAssertOtr qconv qalice ac bc cipher)

-- | This test verifies basic mismatch behaviour of the the JSON endpoint.
postCryptoMessage2 :: TestM ()
postCryptoMessage2 = do
  b <- view tsBrig
  (alice, ac) <- randomUserWithClient (someLastPrekeys !! 0)
  (bob, bc) <- randomUserWithClient (someLastPrekeys !! 1)
  (eve, ec) <- randomUserWithClient (someLastPrekeys !! 2)
  connectUsers alice (list1 bob [eve])
  conv <- decodeConvId <$> postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
  -- Missing eve
  let m = [(bob, bc, toBase64Text "hello bob")]
  r1 <-
    postOtrMessage id alice ac conv m <!! do
      const 412 === statusCode
      assertMismatchWithMessage (Just "client mismatch") [(eve, Set.singleton ec)] [] []
  let x = responseJsonUnsafeWithMsg "ClientMismatch" r1
  -- Fetch all missing clients prekeys
  r2 <-
    post (b . zUser alice . path "/users/prekeys" . json (missingClients x))
      <!! const 200 === statusCode
  let p = responseJsonUnsafeWithMsg "prekeys" r2 :: UserClientPrekeyMap
  liftIO $ do
    Map.keys (userClientMap (getUserClientPrekeyMap p)) @=? [eve]
    Map.keys <$> Map.lookup eve (userClientMap (getUserClientPrekeyMap p)) @=? Just [ec]

-- | This test verifies basic mismatch behaviour of the protobuf endpoint.
postCryptoMessage3 :: TestM ()
postCryptoMessage3 = do
  b <- view tsBrig
  (alice, ac) <- randomUserWithClient (someLastPrekeys !! 0)
  (bob, bc) <- randomUserWithClient (someLastPrekeys !! 1)
  (eve, ec) <- randomUserWithClient (someLastPrekeys !! 2)
  connectUsers alice (list1 bob [eve])
  conv <- decodeConvId <$> postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
  -- Missing eve
  let ciphertext = toBase64Text "hello bob"
  let m = otrRecipients [(bob, [(bc, ciphertext)])]
  r1 <-
    postProtoOtrMessage alice ac conv m
      <!! const 412 === statusCode
  let x = responseJsonUnsafeWithMsg "ClientMismatch" r1
  pure r1
    !!! assertMismatchWithMessage (Just "client mismatch") [(eve, Set.singleton ec)] [] []
  -- Fetch all missing clients prekeys
  r2 <-
    post (b . zUser alice . path "/users/prekeys" . json (missingClients x))
      <!! const 200 === statusCode
  let p = responseJsonUnsafeWithMsg "prekeys" r2 :: UserClientPrekeyMap
  liftIO $ do
    Map.keys (userClientMap (getUserClientPrekeyMap p)) @=? [eve]
    Map.keys <$> Map.lookup eve (userClientMap (getUserClientPrekeyMap p)) @=? Just [ec]

-- | This test verfies behaviour when an unknown client posts the message. Only
-- tests the Protobuf endpoint.
postCryptoMessage4 :: TestM ()
postCryptoMessage4 = do
  alice <- randomUser
  bob <- randomUser
  bc <- randomClient bob (someLastPrekeys !! 0)
  connectUsers alice (list1 bob [])
  conv <- decodeConvId <$> postConv alice [bob] (Just "gossip") [] Nothing Nothing
  -- Unknown client ID => 403
  let ciphertext = toBase64Text "hello bob"
  let m = otrRecipients [(bob, [(bc, ciphertext)])]
  postProtoOtrMessage alice (ClientId "172618352518396") conv m
    !!! const 403 === statusCode

-- | This test verifies behaviour under various values of ignore_missing and
-- report_missing. Only tests the JSON endpoint.
postCryptoMessage5 :: TestM ()
postCryptoMessage5 = do
  (alice, ac) <- randomUserWithClient (someLastPrekeys !! 0)
  (bob, bc) <- randomUserWithClient (someLastPrekeys !! 1)
  (chad, cc) <- randomUserWithClient (someLastPrekeys !! 2)
  (eve, ec) <- randomUserWithClient (someLastPrekeys !! 3)
  connectUsers alice (list1 bob [chad, eve])
  conv <- decodeConvId <$> postConv alice [bob, chad, eve] (Just "gossip") [] Nothing Nothing
  -- Missing eve
  let msgMissingChadAndEve = [(bob, bc, toBase64Text "hello bob")]
  let m' = otrRecipients [(bob, [(bc, toBase64Text "hello bob")])]
  -- These three are equivalent (i.e. report all missing clients)
  postOtrMessage id alice ac conv msgMissingChadAndEve
    !!! const 412 === statusCode
  postOtrMessage (queryItem "ignore_missing" "false") alice ac conv msgMissingChadAndEve
    !!! const 412 === statusCode
  postOtrMessage (queryItem "report_missing" "true") alice ac conv msgMissingChadAndEve
    !!! const 412 === statusCode
  -- These two are equivalent (i.e. ignore all missing clients)
  postOtrMessage (queryItem "ignore_missing" "true") alice ac conv msgMissingChadAndEve
    !!! const 201 === statusCode
  postOtrMessage (queryItem "report_missing" "false") alice ac conv msgMissingChadAndEve
    !!! const 201 === statusCode
  -- Report missing clients of a specific user only
  postOtrMessage (queryItem "report_missing" (toByteString' bob)) alice ac conv msgMissingChadAndEve
    !!! const 201 === statusCode
  -- Let's make sure that the same logic using protobuf in the body works too
  postProtoOtrMessage' Nothing (queryItem "report_missing" (toByteString' bob)) alice ac conv m'
    !!! const 201 === statusCode
  -- Body takes precedence
  postOtrMessage' (Just [bob]) (queryItem "report_missing" (listToByteString [eve, chad])) alice ac conv msgMissingChadAndEve
    !!! const 201 === statusCode
  -- Set it only in the body of the message
  postOtrMessage' (Just [bob]) id alice ac conv msgMissingChadAndEve
    !!! const 201 === statusCode
  -- Let's make sure that protobuf works too, when specified in the body only
  postProtoOtrMessage' (Just [bob]) id alice ac conv m'
    !!! const 201 === statusCode
  reportEveAndChad <-
    -- send message with no clients
    postOtrMessage (queryItem "report_missing" (listToByteString [eve, chad])) alice ac conv []
      <!! const 412 === statusCode
  pure reportEveAndChad
    !!! assertMismatchWithMessage (Just "client mismatch") [(eve, Set.singleton ec), (chad, Set.singleton cc)] [] []
  -- Ignore missing clients of a specific user only
  postOtrMessage (queryItem "ignore_missing" (listToByteString [chad, eve])) alice ac conv msgMissingChadAndEve
    !!! const 201 === statusCode
  ignoreEveAndChadButNotBob <-
    postOtrMessage (queryItem "ignore_missing" (listToByteString [chad, eve])) alice ac conv []
      <!! const 412 === statusCode
  pure ignoreEveAndChadButNotBob
    !!! assertMismatchWithMessage (Just "client mismatch") [(bob, Set.singleton bc)] [] []
  where
    listToByteString = BS.intercalate "," . map toByteString'

-- | Sets up a conversation on Backend A known as "owning backend". One of the
-- users from Backend A will send the message, it is expected that message will
-- be sent successfully.
postMessageQualifiedLocalOwningBackendSuccess :: TestM ()
postMessageQualifiedLocalOwningBackendSuccess = do
  -- WS receive timeout
  let t = 5 # Second
  -- Cannon for local users
  cannon <- view tsCannon
  -- Domain which owns the converstaion
  owningDomain <- viewFederationDomain

  (aliceOwningDomain, aliceClient) <- randomUserWithClientQualified (someLastPrekeys !! 0)
  (bobOwningDomain, bobClient) <- randomUserWithClientQualified (someLastPrekeys !! 1)
  bobClient2 <- randomClient (qUnqualified bobOwningDomain) (someLastPrekeys !! 2)
  (chadOwningDomain, chadClient) <- randomUserWithClientQualified (someLastPrekeys !! 3)
  deeId <- randomId
  deeClient <- liftIO $ generate arbitrary
  let remoteDomain = Domain "far-away.example.com"
      deeRemote = Qualified deeId remoteDomain

  let aliceUnqualified = qUnqualified aliceOwningDomain
      bobUnqualified = qUnqualified bobOwningDomain
      chadUnqualified = qUnqualified chadOwningDomain

  connectLocalQualifiedUsers aliceUnqualified (list1 bobOwningDomain [chadOwningDomain])

  -- FUTUREWORK: Do this test with more than one remote domains
  resp <- postConvWithRemoteUser remoteDomain (mkProfile deeRemote (Name "Dee")) aliceUnqualified [bobOwningDomain, chadOwningDomain, deeRemote]
  let convId = (`Qualified` owningDomain) . decodeConvId $ resp

  WS.bracketR2 cannon bobUnqualified chadUnqualified $ \(wsBob, wsChad) -> do
    let message =
          [ (bobOwningDomain, bobClient, "text-for-bob"),
            (bobOwningDomain, bobClient2, "text-for-bob2"),
            (chadOwningDomain, chadClient, "text-for-chad"),
            (deeRemote, deeClient, "text-for-dee")
          ]

    let brigApi =
          emptyFederatedBrig
            { FederatedBrig.getUserClients = \_ ->
                pure $ UserMap (Map.singleton (qUnqualified deeRemote) (Set.singleton (PubClient deeClient Nothing)))
            }
        galleyApi =
          emptyFederatedGalley
            { FederatedGalley.receiveMessage = \_ _ -> pure ()
            }

    (resp2, requests) <- postProteusMessageQualifiedWithMockFederator aliceUnqualified aliceClient convId message "data" Message.MismatchReportAll brigApi galleyApi
    pure resp2 !!! do
      const 201 === statusCode
      assertMismatchQualified mempty mempty mempty mempty

    liftIO $ do
      let expectedRequests =
            [ (F.Brig, "get-user-clients"),
              (F.Galley, "receive-message")
            ]
      forM_ (zip requests expectedRequests) $ \(req, (component, rpcPath)) -> do
        F.domain req @?= domainText (qDomain deeRemote)
        fmap F.component (F.request req) @?= Just component
        fmap F.path (F.request req) @?= Just ("/federation/" <> rpcPath)
      let encodedTextForBob = toBase64Text "text-for-bob"
          encodedTextForChad = toBase64Text "text-for-chad"
          encodedData = toBase64Text "data"
      WS.assertMatch_ t wsBob (wsAssertOtr' encodedData convId aliceOwningDomain aliceClient bobClient encodedTextForBob)
      WS.assertMatch_ t wsChad (wsAssertOtr' encodedData convId aliceOwningDomain aliceClient chadClient encodedTextForChad)

-- | Sets up a conversation on Backend A known as "owning backend". One of the
-- users from Backend A will send the message but have a missing client. It is
-- expected that the message will not be sent.
postMessageQualifiedLocalOwningBackendMissingClients :: TestM ()
postMessageQualifiedLocalOwningBackendMissingClients = do
  -- Cannon for local users
  cannon <- view tsCannon
  -- Domain which owns the converstaion
  owningDomain <- viewFederationDomain

  (aliceOwningDomain, aliceClient) <- randomUserWithClientQualified (someLastPrekeys !! 0)
  (bobOwningDomain, bobClient) <- randomUserWithClientQualified (someLastPrekeys !! 1)
  (chadOwningDomain, chadClient) <- randomUserWithClientQualified (someLastPrekeys !! 3)
  chadClient2 <- randomClient (qUnqualified chadOwningDomain) (someLastPrekeys !! 2)
  deeId <- randomId
  deeClient <- liftIO $ generate arbitrary

  let remoteDomain = Domain "far-away.example.com"
      deeRemote = Qualified deeId remoteDomain

  let aliceUnqualified = qUnqualified aliceOwningDomain
      bobUnqualified = qUnqualified bobOwningDomain
      chadUnqualified = qUnqualified chadOwningDomain

  connectLocalQualifiedUsers aliceUnqualified (list1 bobOwningDomain [chadOwningDomain])

  -- FUTUREWORK: Do this test with more than one remote domains
  resp <- postConvWithRemoteUser remoteDomain (mkProfile deeRemote (Name "Dee")) aliceUnqualified [bobOwningDomain, chadOwningDomain, deeRemote]
  let convId = (`Qualified` owningDomain) . decodeConvId $ resp

  -- Missing Bob, chadClient2 and Dee
  let message = [(chadOwningDomain, chadClient, "text-for-chad")]
  -- FUTUREWORK: Mock federator and ensure that message is not propagated to remotes
  WS.bracketR2 cannon bobUnqualified chadUnqualified $ \(wsBob, wsChad) -> do
    let brigApi =
          emptyFederatedBrig
            { FederatedBrig.getUserClients = \_ ->
                pure $ UserMap (Map.singleton (qUnqualified deeRemote) (Set.singleton (PubClient deeClient Nothing)))
            }
        galleyApi = emptyFederatedGalley

    (resp2, _requests) <- postProteusMessageQualifiedWithMockFederator aliceUnqualified aliceClient convId message "data" Message.MismatchReportAll brigApi galleyApi

    pure resp2 !!! do
      const 412 === statusCode
      let expectedMissing =
            QualifiedUserClients $
              Map.fromList
                [ ( owningDomain,
                    Map.fromList
                      [ (bobUnqualified, Set.singleton bobClient),
                        (chadUnqualified, Set.singleton chadClient2)
                      ]
                  ),
                  ( remoteDomain,
                    Map.singleton (qUnqualified deeRemote) (Set.singleton deeClient)
                  )
                ]
      assertMismatchQualified mempty expectedMissing mempty mempty
    WS.assertNoEvent (1 # Second) [wsBob, wsChad]

-- | Sets up a conversation on Backend A known as "owning backend". One of the
-- users from Backend A will send the message, it is expected that message will
-- be sent successfully.
postMessageQualifiedLocalOwningBackendRedundantAndDeletedClients :: TestM ()
postMessageQualifiedLocalOwningBackendRedundantAndDeletedClients = do
  -- WS receive timeout
  let t = 5 # Second
  -- Cannon for local users
  cannon <- view tsCannon
  -- Domain which owns the converstaion
  owningDomain <- viewFederationDomain
  let remoteDomain = Domain "far-away.example.com"

  (aliceOwningDomain, aliceClient) <- randomUserWithClientQualified (someLastPrekeys !! 0)
  (bobOwningDomain, bobClient) <- randomUserWithClientQualified (someLastPrekeys !! 1)
  (chadOwningDomain, chadClient) <- randomUserWithClientQualified (someLastPrekeys !! 2)
  chadClientNonExistent <- liftIO $ generate arbitrary
  deeRemote <- (`Qualified` remoteDomain) <$> randomId
  deeClient <- liftIO $ generate arbitrary
  (nonMemberOwningDomain, nonMemberOwningDomainClient) <- randomUserWithClientQualified (someLastPrekeys !! 3)
  nonMemberRemote <- (`Qualified` remoteDomain) <$> randomId
  nonMemberRemoteClient <- liftIO $ generate arbitrary
  let aliceUnqualified = qUnqualified aliceOwningDomain
      bobUnqualified = qUnqualified bobOwningDomain
      chadUnqualified = qUnqualified chadOwningDomain
      deeRemoteUnqualified = qUnqualified deeRemote
      nonMemberUnqualified = qUnqualified nonMemberOwningDomain
      nonMemberRemoteUnqualified = qUnqualified nonMemberRemote

  connectLocalQualifiedUsers aliceUnqualified (list1 bobOwningDomain [chadOwningDomain])

  -- FUTUREWORK: Do this test with more than one remote domains
  resp <- postConvWithRemoteUser remoteDomain (mkProfile deeRemote (Name "Dee")) aliceUnqualified [bobOwningDomain, chadOwningDomain, deeRemote]
  let convId = (`Qualified` owningDomain) . decodeConvId $ resp

  WS.bracketR3 cannon bobUnqualified chadUnqualified nonMemberUnqualified $ \(wsBob, wsChad, wsNonMember) -> do
    let message =
          [ (bobOwningDomain, bobClient, "text-for-bob"),
            (chadOwningDomain, chadClient, "text-for-chad"),
            (chadOwningDomain, chadClientNonExistent, "text-for-chad-non-existent"),
            (deeRemote, deeClient, "text-for-dee"),
            (nonMemberOwningDomain, nonMemberOwningDomainClient, "text-for-non-member-owning-domain"),
            (nonMemberRemote, nonMemberRemoteClient, "text-for-non-member-remote")
          ]

    -- FUTUREWORK: Mock federator and ensure that a message to Dee is sent
    let brigApi =
          emptyFederatedBrig
            { FederatedBrig.getUserClients = \getUserClients ->
                let lookupClients uid
                      | uid == deeRemoteUnqualified = Just (uid, Set.fromList [PubClient deeClient Nothing])
                      | uid == nonMemberRemoteUnqualified = Just (uid, Set.fromList [PubClient nonMemberRemoteClient Nothing])
                      | otherwise = Nothing
                 in pure $ UserMap . Map.fromList . mapMaybe lookupClients $ FederatedBrig.gucUsers getUserClients
            }
        galleyApi =
          emptyFederatedGalley
            { FederatedGalley.receiveMessage = \_ _ -> pure ()
            }

    (resp2, _requests) <- postProteusMessageQualifiedWithMockFederator aliceUnqualified aliceClient convId message "data" Message.MismatchReportAll brigApi galleyApi
    pure resp2 !!! do
      const 201 === statusCode
      let expectedRedundant =
            QualifiedUserClients . Map.fromList $
              [ ( owningDomain,
                  Map.fromList
                    [ (nonMemberUnqualified, Set.singleton nonMemberOwningDomainClient)
                    ]
                ),
                ( remoteDomain,
                  Map.fromList
                    [ (nonMemberRemoteUnqualified, Set.singleton nonMemberRemoteClient)
                    ]
                )
              ]
          expectedDeleted =
            QualifiedUserClients . Map.singleton owningDomain . Map.fromList $
              [(chadUnqualified, Set.singleton chadClientNonExistent)]
      assertMismatchQualified mempty mempty expectedRedundant expectedDeleted
    liftIO $ do
      let encodedTextForBob = toBase64Text "text-for-bob"
          encodedTextForChad = toBase64Text "text-for-chad"
          encodedData = toBase64Text "data"
      WS.assertMatch_ t wsBob (wsAssertOtr' encodedData convId aliceOwningDomain aliceClient bobClient encodedTextForBob)
      WS.assertMatch_ t wsChad (wsAssertOtr' encodedData convId aliceOwningDomain aliceClient chadClient encodedTextForChad)
      -- Wait less for no message
      WS.assertNoEvent (1 # Second) [wsNonMember]

-- | Sets up a conversation on Backend A known as "owning backend". One of the
-- users from Backend A will send the message but have a missing client. It is
-- expected that the message will be sent except when it is specifically
-- requested to report on missing clients of a user.
postMessageQualifiedLocalOwningBackendIgnoreMissingClients :: TestM ()
postMessageQualifiedLocalOwningBackendIgnoreMissingClients = do
  -- WS receive timeout
  let t = 5 # Second
  -- Cannon for local users
  cannon <- view tsCannon
  -- Domain which owns the converstaion
  owningDomain <- viewFederationDomain

  (aliceOwningDomain, aliceClient) <- randomUserWithClientQualified (someLastPrekeys !! 0)
  (bobOwningDomain, _bobClient) <- randomUserWithClientQualified (someLastPrekeys !! 1)
  (chadOwningDomain, chadClient) <- randomUserWithClientQualified (someLastPrekeys !! 3)
  chadClient2 <- randomClient (qUnqualified chadOwningDomain) (someLastPrekeys !! 2)
  deeId <- randomId
  deeClient <- liftIO $ generate arbitrary

  let remoteDomain = Domain "far-away.example.com"
      deeRemote = Qualified deeId remoteDomain

  let aliceUnqualified = qUnqualified aliceOwningDomain
      bobUnqualified = qUnqualified bobOwningDomain
      chadUnqualified = qUnqualified chadOwningDomain

  connectLocalQualifiedUsers aliceUnqualified (list1 bobOwningDomain [chadOwningDomain])

  -- FUTUREWORK: Do this test with more than one remote domains
  resp <- postConvWithRemoteUser remoteDomain (mkProfile deeRemote (Name "Dee")) aliceUnqualified [bobOwningDomain, chadOwningDomain, deeRemote]
  let convId = (`Qualified` owningDomain) . decodeConvId $ resp

  let brigApi =
        emptyFederatedBrig
          { FederatedBrig.getUserClients = \_ -> pure $ UserMap (Map.singleton (qUnqualified deeRemote) (Set.singleton (PubClient deeClient Nothing)))
          }
      galleyApi = emptyFederatedGalley

  -- Missing Bob, chadClient2 and Dee
  let message = [(chadOwningDomain, chadClient, "text-for-chad")]
  -- FUTUREWORK: Mock federator and ensure that clients of Dee are checked. Also
  -- ensure that message is not propagated to remotes
  WS.bracketR2 cannon bobUnqualified chadUnqualified $ \(wsBob, wsChad) -> do
    (resp2, _requests) <- postProteusMessageQualifiedWithMockFederator aliceUnqualified aliceClient convId message "data" Message.MismatchIgnoreAll brigApi galleyApi
    pure resp2 !!! do
      const 201 === statusCode
      assertMismatchQualified mempty mempty mempty mempty
    let encodedTextForChad = toBase64Text "text-for-chad"
        encodedData = toBase64Text "data"
    WS.assertMatch_ t wsChad (wsAssertOtr' encodedData convId aliceOwningDomain aliceClient chadClient encodedTextForChad)
    WS.assertNoEvent (1 # Second) [wsBob]

  -- Another way to ignore all is to report nobody
  WS.bracketR2 cannon bobUnqualified chadUnqualified $ \(wsBob, wsChad) -> do
    (resp2, _requests) <- postProteusMessageQualifiedWithMockFederator aliceUnqualified aliceClient convId message "data" (Message.MismatchReportOnly mempty) brigApi galleyApi
    pure resp2 !!! do
      const 201 === statusCode
      assertMismatchQualified mempty mempty mempty mempty
    let encodedTextForChad = toBase64Text "text-for-chad"
        encodedData = toBase64Text "data"
    WS.assertMatch_ t wsChad (wsAssertOtr' encodedData convId aliceOwningDomain aliceClient chadClient encodedTextForChad)
    WS.assertNoEvent (1 # Second) [wsBob]

  -- Yet another way to ignore all is to ignore specific users
  WS.bracketR2 cannon bobUnqualified chadUnqualified $ \(wsBob, wsChad) -> do
    (resp2, _requests) <-
      postProteusMessageQualifiedWithMockFederator
        aliceUnqualified
        aliceClient
        convId
        message
        "data"
        (Message.MismatchIgnoreOnly (Set.fromList [bobOwningDomain, chadOwningDomain, deeRemote]))
        brigApi
        galleyApi
    pure resp2 !!! do
      const 201 === statusCode
      assertMismatchQualified mempty mempty mempty mempty
    let encodedTextForChad = toBase64Text "text-for-chad"
        encodedData = toBase64Text "data"
    WS.assertMatch_ t wsChad (wsAssertOtr' encodedData convId aliceOwningDomain aliceClient chadClient encodedTextForChad)
    WS.assertNoEvent (1 # Second) [wsBob]

  -- When we ask only chad be reported, but one of their clients is missing, the
  -- message shouldn't be sent!
  WS.bracketR2 cannon bobUnqualified chadUnqualified $ \(wsBob, wsChad) -> do
    (resp2, _requests) <-
      postProteusMessageQualifiedWithMockFederator
        aliceUnqualified
        aliceClient
        convId
        message
        "data"
        (Message.MismatchReportOnly (Set.fromList [chadOwningDomain]))
        brigApi
        galleyApi
    pure resp2 !!! do
      const 412 === statusCode
      let expectedMissing =
            QualifiedUserClients . Map.singleton owningDomain . Map.fromList $
              [(chadUnqualified, Set.singleton chadClient2)]
      assertMismatchQualified mempty expectedMissing mempty mempty

    WS.assertNoEvent (1 # Second) [wsBob, wsChad]

  -- Same as above, but with a remote user's client
  WS.bracketR2 cannon bobUnqualified chadUnqualified $ \(wsBob, wsChad) -> do
    (resp2, _requests) <-
      postProteusMessageQualifiedWithMockFederator
        aliceUnqualified
        aliceClient
        convId
        message
        "data"
        (Message.MismatchReportOnly (Set.fromList [deeRemote]))
        brigApi
        galleyApi
    pure resp2 !!! do
      const 412 === statusCode
      let expectedMissing =
            QualifiedUserClients . Map.singleton remoteDomain . Map.fromList $
              [(qUnqualified deeRemote, Set.singleton deeClient)]
      assertMismatchQualified mempty expectedMissing mempty mempty
    WS.assertNoEvent (1 # Second) [wsBob, wsChad]

postMessageQualifiedLocalOwningBackendFailedToSendClients :: TestM ()
postMessageQualifiedLocalOwningBackendFailedToSendClients = do
  -- WS receive timeout
  let t = 5 # Second
  -- Cannon for local users
  cannon <- view tsCannon
  -- Domain which owns the converstaion
  owningDomain <- viewFederationDomain

  (aliceOwningDomain, aliceClient) <- randomUserWithClientQualified (someLastPrekeys !! 0)
  (bobOwningDomain, bobClient) <- randomUserWithClientQualified (someLastPrekeys !! 1)
  bobClient2 <- randomClient (qUnqualified bobOwningDomain) (someLastPrekeys !! 2)
  (chadOwningDomain, chadClient) <- randomUserWithClientQualified (someLastPrekeys !! 3)
  deeId <- randomId
  deeClient <- liftIO $ generate arbitrary
  let remoteDomain = Domain "far-away.example.com"
      deeRemote = Qualified deeId remoteDomain

  let aliceUnqualified = qUnqualified aliceOwningDomain
      bobUnqualified = qUnqualified bobOwningDomain
      chadUnqualified = qUnqualified chadOwningDomain

  connectLocalQualifiedUsers aliceUnqualified (list1 bobOwningDomain [chadOwningDomain])

  -- FUTUREWORK: Do this test with more than one remote domains
  resp <- postConvWithRemoteUser remoteDomain (mkProfile deeRemote (Name "Dee")) aliceUnqualified [bobOwningDomain, chadOwningDomain, deeRemote]
  let convId = (`Qualified` owningDomain) . decodeConvId $ resp

  WS.bracketR2 cannon bobUnqualified chadUnqualified $ \(wsBob, wsChad) -> do
    let message =
          [ (bobOwningDomain, bobClient, "text-for-bob"),
            (bobOwningDomain, bobClient2, "text-for-bob2"),
            (chadOwningDomain, chadClient, "text-for-chad"),
            (deeRemote, deeClient, "text-for-dee")
          ]

    let brigApi =
          emptyFederatedBrig
            { FederatedBrig.getUserClients = \_ ->
                pure $ UserMap (Map.singleton (qUnqualified deeRemote) (Set.singleton (PubClient deeClient Nothing)))
            }
        galleyApi =
          emptyFederatedGalley
            { FederatedGalley.receiveMessage = \_ _ -> throwError err503 {errBody = "Down for maintanance."}
            }

    (resp2, _requests) <- postProteusMessageQualifiedWithMockFederator aliceUnqualified aliceClient convId message "data" Message.MismatchReportAll brigApi galleyApi

    let expectedFailedToSend =
          QualifiedUserClients . Map.fromList $
            [ ( remoteDomain,
                Map.fromList
                  [ (deeId, Set.singleton deeClient)
                  ]
              )
            ]
    pure resp2 !!! do
      const 201 === statusCode
      assertMismatchQualified expectedFailedToSend mempty mempty mempty

    liftIO $ do
      let encodedTextForBob = toBase64Text "text-for-bob"
          encodedTextForChad = toBase64Text "text-for-chad"
          encodedData = toBase64Text "data"
      WS.assertMatch_ t wsBob (wsAssertOtr' encodedData convId aliceOwningDomain aliceClient bobClient encodedTextForBob)
      WS.assertMatch_ t wsChad (wsAssertOtr' encodedData convId aliceOwningDomain aliceClient chadClient encodedTextForChad)

postMessageQualifiedRemoteOwningBackendFailure :: TestM ()
postMessageQualifiedRemoteOwningBackendFailure = do
  (aliceLocal, aliceClient) <- randomUserWithClientQualified (someLastPrekeys !! 0)
  let aliceUnqualified = qUnqualified aliceLocal
  convIdUnqualified <- randomId
  let remoteDomain = Domain "far-away.example.com"
      convId = Qualified convIdUnqualified remoteDomain

  let galleyApi =
        emptyFederatedGalley
          { FederatedGalley.sendMessage = \_ _ -> throwError err503 {errBody = "Down for maintanance."}
          }

  (resp2, _requests) <-
    postProteusMessageQualifiedWithMockFederator aliceUnqualified aliceClient convId [] "data" Message.MismatchReportAll emptyFederatedBrig galleyApi

  pure resp2 !!! do
    const 533 === statusCode

postMessageQualifiedRemoteOwningBackendSuccess :: TestM ()
postMessageQualifiedRemoteOwningBackendSuccess = do
  (aliceLocal, aliceClient) <- randomUserWithClientQualified (someLastPrekeys !! 0)
  (bobOwningDomain, bobClient) <- randomUserWithClientQualified (someLastPrekeys !! 1)
  deeId <- randomId
  deeClient <- liftIO $ generate arbitrary

  let aliceUnqualified = qUnqualified aliceLocal
  convIdUnqualified <- randomId
  let remoteDomain = Domain "far-away.example.com"
      convId = Qualified convIdUnqualified remoteDomain
      deeRemote = Qualified deeId remoteDomain

  now <- toUTCTimeMillis <$> liftIO getCurrentTime
  let redundant =
        QualifiedUserClients
          . Map.singleton remoteDomain
          . Map.singleton deeId
          . Set.singleton
          $ deeClient
      mss =
        Message.MessageSendingStatus
          { Message.mssTime = now,
            Message.mssMissingClients = mempty,
            Message.mssRedundantClients = redundant,
            Message.mssDeletedClients = mempty,
            Message.mssFailedToSend = mempty
          }
      message = [(bobOwningDomain, bobClient, "text-for-bob"), (deeRemote, deeClient, "text-for-dee")]
      galleyApi =
        emptyFederatedGalley
          { FederatedGalley.sendMessage = \_ _ -> pure (FederatedGalley.MessageSendResponse (Right mss))
          }

  (resp2, _requests) <-
    postProteusMessageQualifiedWithMockFederator aliceUnqualified aliceClient convId message "data" Message.MismatchReportAll emptyFederatedBrig galleyApi

  pure resp2 !!! do
    const 201 === statusCode
    assertMismatchQualified mempty mempty redundant mempty

postJoinConvOk :: TestM ()
postJoinConvOk = do
  c <- view tsCannon
  alice <- randomUser
  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob
  conv <- decodeConvId <$> postConv alice [] (Just "gossip") [InviteAccess, LinkAccess] Nothing Nothing
  let qconv = Qualified conv (qDomain qbob)
  WS.bracketR2 c alice bob $ \(wsA, wsB) -> do
    postJoinConv bob conv !!! const 200 === statusCode
    postJoinConv bob conv !!! const 204 === statusCode
    void . liftIO $
      WS.assertMatchN (5 # Second) [wsA, wsB] $
        wsAssertMemberJoinWithRole qconv qbob [qbob] roleNameWireMember

testJoinCodeConv :: TestM ()
testJoinCodeConv = do
  let convName = "gossip"

  alice <- randomUser
  convId <- decodeConvId <$> postConv alice [] (Just convName) [CodeAccess] (Just ActivatedAccessRole) Nothing
  cCode <- decodeConvCodeEvent <$> postConvCode alice convId

  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob
  getJoinCodeConv bob (conversationKey cCode) (conversationCode cCode) !!! do
    const (Right (ConversationCoverView convId (Just convName))) === responseJsonEither

  -- A user that would not be able to join conversation cannot view it either.
  eve <- ephemeralUser
  getJoinCodeConv eve (conversationKey cCode) (conversationCode cCode) !!! do
    const 403 === statusCode

postJoinCodeConvOk :: TestM ()
postJoinCodeConvOk = do
  c <- view tsCannon
  alice <- randomUser
  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob
  eve <- ephemeralUser
  dave <- ephemeralUser
  conv <- decodeConvId <$> postConv alice [] (Just "gossip") [CodeAccess] (Just ActivatedAccessRole) Nothing
  let qconv = Qualified conv (qDomain qbob)
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
    void . liftIO $
      WS.assertMatchN (5 # Second) [wsA, wsB] $
        wsAssertMemberJoinWithRole qconv qbob [qbob] roleNameWireMember
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
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice
  conv <- decodeConvId <$> postConv alice [] (Just "gossip") [InviteAccess] Nothing Nothing
  let qconv = Qualified conv (qDomain qalice)
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
    void . liftIO $
      WS.assertMatchN (5 # Second) [wsA] $
        wsAssertConvAccessUpdate qconv qalice nonActivatedAccess
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
  localDomain <- viewFederationDomain
  c <- view tsCannon
  -- Create a team conversation with team-alice, team-bob, activated-eve
  -- Non-activated mallory can join
  (alice, tid) <- createBindingTeam
  let qalice = Qualified alice localDomain
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
  let qmallory = Qualified mallory localDomain
      qconv = Qualified conv localDomain
  j <- decodeConvCodeEvent <$> postConvCode alice conv
  WS.bracketR3 c alice bob eve $ \(wsA, wsB, wsE) -> do
    postJoinCodeConv mallory j !!! const 200 === statusCode
    void . liftIO $
      WS.assertMatchN (5 # Second) [wsA, wsB, wsE] $
        wsAssertMemberJoinWithRole qconv qmallory [qmallory] roleNameWireMember
  WS.bracketRN c [alice, bob, eve, mallory] $ \[wsA, wsB, wsE, wsM] -> do
    let teamAccess = ConversationAccessUpdate [InviteAccess, CodeAccess] TeamAccessRole
    putAccessUpdate alice conv teamAccess !!! const 200 === statusCode
    void . liftIO $
      WS.assertMatchN (5 # Second) [wsA, wsB, wsE, wsM] $
        wsAssertConvAccessUpdate qconv qalice teamAccess
    -- non-team members get kicked out
    void . liftIO $
      WS.assertMatchN (5 # Second) [wsA, wsB, wsE, wsM] $
        wsAssertMemberLeave qconv qalice [eve, mallory]
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
    const [toUUID usr] === map (toUUID . qUnqualified . cnvQualifiedId) . decodeConvList

getConvsOk2 :: TestM ()
getConvsOk2 = do
  [alice, bob] <- randomUsers 2
  connectUsers alice (singleton bob)
  -- create & get one2one conv
  cnv1 <- responseJsonUnsafeWithMsg "conversation" <$> postO2OConv alice bob (Just "gossip1")
  getConvs alice (Just $ Left [qUnqualified . cnvQualifiedId $ cnv1]) Nothing !!! do
    const 200 === statusCode
    const (Just [cnvQualifiedId cnv1]) === fmap (map cnvQualifiedId . convList) . responseJsonUnsafe
  -- create & get group conv
  carl <- randomUser
  connectUsers alice (singleton carl)
  cnv2 <- responseJsonUnsafeWithMsg "conversation" <$> postConv alice [bob, carl] (Just "gossip2") [] Nothing Nothing
  getConvs alice (Just $ Left [qUnqualified . cnvQualifiedId $ cnv2]) Nothing !!! do
    const 200 === statusCode
    const (Just [cnvQualifiedId cnv2]) === fmap (map cnvQualifiedId . convList) . responseJsonUnsafe
  -- get both
  rs <- getConvs alice Nothing Nothing <!! const 200 === statusCode
  let convs = convList <$> responseJsonUnsafe rs
  let c1 = convs >>= find ((== cnvQualifiedId cnv1) . cnvQualifiedId)
  let c2 = convs >>= find ((== cnvQualifiedId cnv2) . cnvQualifiedId)
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

-- same test as getConvsOk2, but using the listConversations endpoint
listConvsOk2 :: TestM ()
listConvsOk2 = do
  [alice, bob] <- randomUsers 2
  connectUsers alice (singleton bob)
  -- create & get one2one conv
  cnv1 <- responseJsonUnsafeWithMsg "conversation" <$> postO2OConv alice bob (Just "gossip1")
  let req1 = ListConversations (cnvQualifiedId cnv1 :| [])
  listConvs alice req1 !!! do
    const 200 === statusCode
    const (Just [cnvQualifiedId cnv1]) === fmap (map cnvQualifiedId . crFound) . responseJsonUnsafe
  -- create & get group conv
  carl <- randomUser
  connectUsers alice (singleton carl)
  cnv2 <- responseJsonUnsafeWithMsg "conversation" <$> postConv alice [bob, carl] (Just "gossip2") [] Nothing Nothing
  let req2 = ListConversations (cnvQualifiedId cnv2 :| [])
  listConvs alice req2 !!! do
    const 200 === statusCode
    const (Just [cnvQualifiedId cnv2]) === fmap (map cnvQualifiedId . crFound) . responseJsonUnsafe
  -- get both
  let req3 = ListConversations (cnvQualifiedId cnv1 :| [cnvQualifiedId cnv2])
  rs <- listConvs alice req3  <!! const 200 === statusCode
  let convs = crFound <$> responseJsonUnsafe rs
  let c1 = convs >>= find ((== cnvQualifiedId cnv1) . cnvQualifiedId)
  let c2 = convs >>= find ((== cnvQualifiedId cnv2) . cnvQualifiedId)
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
  replicateM_ 253 $
    postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
      !!! const 201 === statusCode
  -- 1 self conv, 2 convs with bob and eve, 253 gossips = 256 convs
  foldM_ (getChunk 16 alice) Nothing [15, 14 .. 0 :: Int]
  where
    getChunk size alice start n = do
      resp <- getConvIds alice start (Just size) <!! const 200 === statusCode
      let c = fromMaybe (ConversationList [] False) (responseJsonUnsafe resp)
      liftIO $ do
        -- This is because of the way this test is setup, we always get 16
        -- convs, even on the last one
        assertEqual
          ("Number of convs should match the requested size, " <> show n <> " more gets to go")
          (fromIntegral size)
          (length (convList c))

        if n > 0
          then assertEqual "hasMore should be True" True (convHasMore c)
          else assertEqual ("hasMore should be False, " <> show n <> " more chunks to go") False (convHasMore c)

      return (Just (Right (last (convList c))))

getConvIdsFailMaxSize :: TestM ()
getConvIdsFailMaxSize = do
  usr <- randomUser
  getConvIds usr Nothing (Just 1001)
    !!! const 400 === statusCode

listConvIdsOk :: TestM ()
listConvIdsOk = do
  [alice, bob] <- randomUsers 2
  connectUsers alice (singleton bob)
  void $ postO2OConv alice bob (Just "gossip")
  let paginationOpts = GetPaginatedConversationIds Nothing (toRange (Proxy @5))
  listConvIds alice paginationOpts !!! do
    const 200 === statusCode
    const (Right 2) === fmap length . decodeQualifiedConvIdList
  listConvIds bob paginationOpts !!! do
    const 200 === statusCode
    const (Right 2) === fmap length . decodeQualifiedConvIdList

paginateConvListIds :: TestM ()
paginateConvListIds = do
  [alice, bob, eve] <- randomUsers 3
  connectUsers alice (list1 bob [eve])
  localDomain <- viewFederationDomain
  let qAlice = Qualified alice localDomain
  now <- liftIO getCurrentTime
  fedGalleyClient <- view tsFedGalleyClient

  replicateM_ 197 $
    postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
      !!! const 201 === statusCode

  remoteChad <- randomId
  let chadDomain = Domain "chad.example.com"
      qChad = Qualified remoteChad chadDomain
  replicateM_ 25 $ do
    conv <- randomId
    let cmu =
          FederatedGalley.ConversationMemberUpdate
            { FederatedGalley.cmuTime = now,
              FederatedGalley.cmuOrigUserId = qChad,
              FederatedGalley.cmuConvId = Qualified conv chadDomain,
              FederatedGalley.cmuAlreadyPresentUsers = [],
              FederatedGalley.cmuUsersAdd = [(qAlice, roleNameWireMember)],
              FederatedGalley.cmuUsersRemove = []
            }
    FederatedGalley.updateConversationMemberships fedGalleyClient cmu

  remoteDee <- randomId
  let deeDomain = Domain "dee.example.com"
      qDee = Qualified remoteDee deeDomain
  replicateM_ 31 $ do
    conv <- randomId
    let cmu =
          FederatedGalley.ConversationMemberUpdate
            { FederatedGalley.cmuTime = now,
              FederatedGalley.cmuOrigUserId = qDee,
              FederatedGalley.cmuConvId = Qualified conv deeDomain,
              FederatedGalley.cmuAlreadyPresentUsers = [],
              FederatedGalley.cmuUsersAdd = [(qAlice, roleNameWireMember)],
              FederatedGalley.cmuUsersRemove = []
            }
    FederatedGalley.updateConversationMemberships fedGalleyClient cmu

  -- 1 self conv + 2 convs with bob and eve + 197 local convs + 25 convs on
  -- chad.example.com + 31 on dee.example = 256 convs. Getting them 16 at a time
  -- should get all them in 16 times.
  foldM_ (getChunkedConvs 16 0 alice) Nothing [16, 15 .. 0 :: Int]

-- This test ensures to setup conversations so that a page would end exactly
-- when local convs are exhausted and then exactly when another remote domain's
-- convs are exhausted. As the local convs and remote convs are stored in two
-- different tables, this is an important edge case to test.
paginateConvListIdsPageEndingAtLocalsAndDomain :: TestM ()
paginateConvListIdsPageEndingAtLocalsAndDomain = do
  [alice, bob, eve] <- randomUsers 3
  connectUsers alice (list1 bob [eve])
  localDomain <- viewFederationDomain
  let qAlice = Qualified alice localDomain
  now <- liftIO getCurrentTime
  fedGalleyClient <- view tsFedGalleyClient

  -- With page size 16, 29 group convs + 2 one-to-one convs + 1 self conv, we
  -- get 32 convs. The 2nd page should end here.
  replicateM_ 29 $
    postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
      !!! const 201 === statusCode

  -- We should be able to page through current state in 2 pages exactly
  foldM_ (getChunkedConvs 16 0 alice) Nothing [2, 1, 0 :: Int]

  remoteChad <- randomId
  let chadDomain = Domain "chad.example.com"
      qChad = Qualified remoteChad chadDomain
  -- The 3rd page will end with this domain
  replicateM_ 16 $ do
    conv <- randomId
    let cmu =
          FederatedGalley.ConversationMemberUpdate
            { FederatedGalley.cmuTime = now,
              FederatedGalley.cmuOrigUserId = qChad,
              FederatedGalley.cmuConvId = Qualified conv chadDomain,
              FederatedGalley.cmuAlreadyPresentUsers = [],
              FederatedGalley.cmuUsersAdd = [(qAlice, roleNameWireMember)],
              FederatedGalley.cmuUsersRemove = []
            }
    FederatedGalley.updateConversationMemberships fedGalleyClient cmu

  remoteDee <- randomId
  let deeDomain = Domain "dee.example.com"
      qDee = Qualified remoteDee deeDomain
  -- The 4th and last page will end with this domain
  replicateM_ 16 $ do
    conv <- randomId
    let cmu =
          FederatedGalley.ConversationMemberUpdate
            { FederatedGalley.cmuTime = now,
              FederatedGalley.cmuOrigUserId = qDee,
              FederatedGalley.cmuConvId = Qualified conv deeDomain,
              FederatedGalley.cmuAlreadyPresentUsers = [],
              FederatedGalley.cmuUsersAdd = [(qAlice, roleNameWireMember)],
              FederatedGalley.cmuUsersRemove = []
            }
    FederatedGalley.updateConversationMemberships fedGalleyClient cmu

  foldM_ (getChunkedConvs 16 0 alice) Nothing [4, 3, 2, 1, 0 :: Int]

-- | Gets chunked conversation ids given size of each chunk, size of the last
-- chunk, requesting user and @n@ which represents how many chunks are remaining
-- to go, when this is 0, it is assumed that this chunk is last and the response
-- must set @has_more@ to 'False' and the number of conv ids returned should
-- match @lastSize@.
getChunkedConvs :: HasCallStack => Int32 -> Int -> UserId -> Maybe ConversationPagingState -> Int -> TestM (Maybe ConversationPagingState)
getChunkedConvs size lastSize alice pagingState n = do
  let paginationOpts = GetPaginatedConversationIds pagingState (unsafeRange size)
  resp <- listConvIds alice paginationOpts <!! const 200 === statusCode
  let c = responseJsonUnsafeWithMsg "failed to parse ConvIdsPage" resp
  liftIO $ do
    if n > 0
      then assertEqual ("Number of convs should match the requested size, " <> show n <> " more chunks to go") (fromIntegral size) (length (pageConvIds c))
      else assertEqual "Number of convs should match the last size, no more chunks to go" lastSize (length (pageConvIds c))

    if n > 0
      then assertEqual ("hasMore should be True, " <> show n <> " more chunk(s) to go") True (pageHasMore c)
      else assertEqual "hasMore should be False, no more chunks to go" False (pageHasMore c)

  return . Just $ pagePagingState c

getConvsPagingOk :: TestM ()
getConvsPagingOk = do
  [ally, bill, carl] <- randomUsers 3
  connectUsers ally (list1 bill [carl])
  replicateM_ 11 $ postConv ally [bill, carl] (Just "gossip") [] Nothing Nothing
  walk ally [3, 3, 3, 3, 2] -- 11 (group) + 2 (1:1) + 1 (self)
  walk bill [3, 3, 3, 3, 1] -- 11 (group) + 1 (1:1) + 1 (self)
  walk carl [3, 3, 3, 3, 1] -- 11 (group) + 1 (1:1) + 1 (self)
  where
    walk :: Foldable t => UserId -> t Int -> TestM ()
    walk u = foldM_ (next u 3) Nothing
    next :: UserId -> Int32 -> Maybe ConvId -> Int -> TestM (Maybe ConvId)
    next u step start n = do
      r1 <- getConvIds u (Right <$> start) (Just step) <!! const 200 === statusCode
      let ids1 = convList <$> responseJsonUnsafe r1
      liftIO $ assertEqual "unexpected length (getConvIds)" (Just n) (length <$> ids1)
      r2 <- getConvs u (Right <$> start) (Just step) <!! const 200 === statusCode
      let ids3 = map (qUnqualified . cnvQualifiedId) . convList <$> responseJsonUnsafe r2
      liftIO $ assertEqual "unexpected length (getConvs)" (Just n) (length <$> ids3)
      liftIO $ assertBool "getConvIds /= getConvs" (ids1 == ids3)
      return $ ids1 >>= listToMaybe . reverse

-- same test as getConvsPagingOk, but using the listConversations endpoint
-- (only tests pagination behaviour for local conversations)
-- FUTUREWORK: pagination for remote conversations
-- listConvsPagingOk :: TestM ()
-- listConvsPagingOk = do
--   [ally, bill, carl] <- randomUsers 3
--   connectUsers ally (list1 bill [carl])
--   replicateM_ 11 $ postConv ally [bill, carl] (Just "gossip") [] Nothing Nothing
--   walk ally [3, 3, 3, 3, 2] -- 11 (group) + 2 (1:1) + 1 (self)
--   walk bill [3, 3, 3, 3, 1] -- 11 (group) + 1 (1:1) + 1 (self)
--   walk carl [3, 3, 3, 3, 1] -- 11 (group) + 1 (1:1) + 1 (self)
--   where
--     walk :: Foldable t => UserId -> t Int -> TestM ()
--     walk u = foldM_ (next u 3) Nothing
--     next :: UserId -> Int32 -> Maybe ConvId -> Int -> TestM (Maybe ConvId)
--     next u step start n = do
--       -- FUTUREWORK: support an endpoint to get qualified conversation IDs
--       -- (without all the conversation metadata)
--       r1 <- getConvIds u (Right <$> start) (Just step) <!! const 200 === statusCode
--       let ids1 = convList <$> responseJsonUnsafe r1
--       liftIO $ assertEqual "unexpected length (getConvIds)" (Just n) (length <$> ids1)
--       localDomain <- viewFederationDomain
--       let requestBody = ListConversations Nothing (flip Qualified localDomain <$> start) (Just (unsafeRange step))
--       r2 <- listConvs u requestBody <!! const 200 === statusCode
--       let ids3 = map (qUnqualified . cnvQualifiedId) . convList <$> responseJsonUnsafe r2
--       liftIO $ assertEqual "unexpected length (getConvs)" (Just n) (length <$> ids3)
--       liftIO $ assertBool "getConvIds /= getConvs" (ids1 == ids3)
--       return $ ids1 >>= listToMaybe . reverse

postConvFailNotConnected :: TestM ()
postConvFailNotConnected = do
  alice <- randomUser
  bob <- randomUser
  jane <- randomUser
  postConv alice [bob, jane] Nothing [] Nothing Nothing !!! do
    const 403 === statusCode
    const (Just "not-connected") === fmap label . responseJsonUnsafe

postConvQualifiedFailNotConnected :: TestM ()
postConvQualifiedFailNotConnected = do
  alice <- randomUser
  bob <- randomQualifiedUser
  jane <- randomQualifiedUser
  postConvQualified alice [bob, jane] Nothing [] Nothing Nothing !!! do
    const 403 === statusCode
    const (Just "not-connected") === fmap label . responseJsonUnsafe

postConvFailNumMembers :: TestM ()
postConvFailNumMembers = do
  n <- fromIntegral <$> view tsMaxConvSize
  alice <- randomUser
  bob : others <- replicateM n randomUser
  connectUsers alice (list1 bob others)
  postConv alice (bob : others) Nothing [] Nothing Nothing !!! do
    const 400 === statusCode
    const (Just "client-error") === fmap label . responseJsonUnsafe

postConvQualifiedFailNumMembers :: TestM ()
postConvQualifiedFailNumMembers = do
  n <- fromIntegral <$> view tsMaxConvSize
  alice <- randomUser
  bob : others <- replicateM n randomQualifiedUser
  connectLocalQualifiedUsers alice (list1 bob others)
  postConvQualified alice (bob : others) Nothing [] Nothing Nothing !!! do
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

--- | If somebody has blocked a user, that user shouldn't be able to create a
-- group conversation which includes them.
postConvQualifiedFailBlocked :: TestM ()
postConvQualifiedFailBlocked = do
  alice <- randomUser
  bob <- randomQualifiedUser
  jane <- randomQualifiedUser
  connectLocalQualifiedUsers alice (list1 bob [jane])
  putConnectionQualified jane alice Blocked
    !!! const 200 === statusCode
  postConvQualified alice [bob, jane] Nothing [] Nothing Nothing !!! do
    const 403 === statusCode
    const (Just "not-connected") === fmap label . responseJsonUnsafe

postConvQualifiedNonExistentDomain :: TestM ()
postConvQualifiedNonExistentDomain = do
  alice <- randomUser
  bob <- flip Qualified (Domain "non-existent.example.com") <$> randomId
  postConvQualified alice [bob] Nothing [] Nothing Nothing !!! do
    const 422 === statusCode

postConvQualifiedNonExistentUser :: TestM ()
postConvQualifiedNonExistentUser = do
  alice <- randomUser
  bobId <- randomId
  charlieId <- randomId
  let remoteDomain = Domain "far-away.example.com"
      bob = Qualified bobId remoteDomain
      charlie = Qualified charlieId remoteDomain
  opts <- view tsGConf
  _g <- view tsGalley
  (resp, _) <-
    withTempMockFederator
      opts
      remoteDomain
      (const [mkProfile charlie (Name "charlie")])
      (postConvQualified alice [bob, charlie] (Just "remote gossip") [] Nothing Nothing)
  liftIO $ do
    statusCode resp @?= 400
    let err = responseJsonUnsafe resp :: Object
    (err ^. at "label") @?= Just "unknown-remote-user"

postConvQualifiedFederationNotEnabled :: TestM ()
postConvQualifiedFederationNotEnabled = do
  g <- view tsGalley
  alice <- randomUser
  bob <- flip Qualified (Domain "some-remote-backend.example.com") <$> randomId
  opts <- view tsGConf
  let federatorNotConfigured :: Opts = opts & optFederator .~ Nothing
  withSettingsOverrides federatorNotConfigured $
    postConvHelper g alice [bob] !!! do
      const 400 === statusCode
      const (Just "federation-not-enabled") === fmap label . responseJsonUnsafe

-- like postConvQualified
-- FUTUREWORK: figure out how to use functions in the TestM monad inside withSettingsOverrides and remove this duplication
postConvHelper :: (MonadIO m, MonadHttp m) => (Request -> Request) -> UserId -> [Qualified UserId] -> m ResponseLBS
postConvHelper g zusr newUsers = do
  let conv = NewConvUnmanaged $ NewConv [] newUsers (Just "gossip") (Set.fromList []) Nothing Nothing Nothing Nothing roleNameWireAdmin
  post $ g . path "/conversations" . zUser zusr . zConn "conn" . zType "access" . json conv

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
  let inv = NewConvUnmanaged (NewConv [alice] [] Nothing mempty Nothing Nothing Nothing Nothing roleNameWireAdmin)
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
    Just "A" @=? cnvName cnv
    [] @=? cmOthers (cnvMembers cnv)
    privateAccess @=? cnvAccess cnv
  -- Alice blocks / cancels
  cancel alice cnv
  -- Alice makes another connect attempt
  rsp2 <- postConnectConv alice bob "A2" "a2" Nothing <!! const 200 === statusCode
  let cnv2 = responseJsonUnsafeWithMsg "conversation" rsp2
  liftIO $ do
    ConnectConv @=? cnvType cnv2
    Just "A2" @=? cnvName cnv2
    [] @=? cmOthers (cnvMembers cnv2)
    privateAccess @=? cnvAccess cnv2
  -- Alice blocks / cancels again
  cancel alice cnv
  -- Now Bob attempts to connect
  rsp3 <- postConnectConv bob alice "B" "b" Nothing <!! const 200 === statusCode
  let cnv3 = responseJsonUnsafeWithMsg "conversation" rsp3
  liftIO $ do
    ConnectConv @=? cnvType cnv3
    Just "B" @=? cnvName cnv3
    privateAccess @=? cnvAccess cnv3
  -- Bob accepting is a no-op, since he is already a member
  let convId = qUnqualified . cnvQualifiedId $ cnv
  putConvAccept bob convId !!! const 200 === statusCode
  cnvX <- responseJsonUnsafeWithMsg "conversation" <$> getConv bob convId
  liftIO $ do
    ConnectConv @=? cnvType cnvX
    Just "B" @=? cnvName cnvX
    privateAccess @=? cnvAccess cnvX
  -- Alice accepts, finally turning it into a 1-1
  putConvAccept alice convId !!! const 200 === statusCode
  cnv4 <- responseJsonUnsafeWithMsg "conversation" <$> getConv alice convId
  liftIO $ do
    One2OneConv @=? cnvType cnv4
    Just "B" @=? cnvName cnv4
    privateAccess @=? cnvAccess cnv4
  where
    cancel u c = do
      g <- view tsGalley
      let cnvId = qUnqualified . cnvQualifiedId
      put (g . paths ["/i/conversations", toByteString' (cnvId c), "block"] . zUser u)
        !!! const 200 === statusCode
      getConv u (cnvId c) !!! const 403 === statusCode

putBlockConvOk :: TestM ()
putBlockConvOk = do
  g <- view tsGalley
  alice <- randomUser
  bob <- randomUser
  conv <- responseJsonUnsafeWithMsg "conversation" <$> postConnectConv alice bob "Alice" "connect with me!" (Just "me@me.com")
  let convId = qUnqualified . cnvQualifiedId $ conv
  getConv alice convId !!! const 200 === statusCode
  getConv bob convId !!! const 403 === statusCode
  put (g . paths ["/i/conversations", toByteString' convId, "block"] . zUser bob)
    !!! const 200 === statusCode
  -- A is still the only member of the 1-1
  getConv alice convId !!! do
    const 200 === statusCode
    const (cnvMembers conv) === cnvMembers . responseJsonUnsafeWithMsg "conversation"
  -- B accepts the conversation by unblocking
  put (g . paths ["/i/conversations", toByteString' convId, "unblock"] . zUser bob)
    !!! const 200 === statusCode
  getConv bob convId !!! const 200 === statusCode
  -- B blocks A in the 1-1
  put (g . paths ["/i/conversations", toByteString' convId, "block"] . zUser bob)
    !!! const 200 === statusCode
  -- B no longer sees the 1-1
  getConv bob convId !!! const 403 === statusCode
  -- B unblocks A in the 1-1
  put (g . paths ["/i/conversations", toByteString' convId, "unblock"] . zUser bob)
    !!! const 200 === statusCode
  -- B sees the blocked 1-1 again
  getConv bob convId !!! do
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

getConvQualifiedOk :: TestM ()
getConvQualifiedOk = do
  alice <- randomUser
  bob <- randomQualifiedUser
  chuck <- randomQualifiedUser
  connectLocalQualifiedUsers alice (list1 bob [chuck])
  conv <- decodeConvId <$> postConvQualified alice [bob, chuck] (Just "gossip") [] Nothing Nothing
  getConv alice conv !!! const 200 === statusCode
  getConv (qUnqualified bob) conv !!! const 200 === statusCode
  getConv (qUnqualified chuck) conv !!! const 200 === statusCode

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
  let c = maybe (error "invalid connect conversation") (qUnqualified . cnvQualifiedId) (responseJsonUnsafe bdy)
  deleteMember alice alice c !!! const 403 === statusCode

-- FUTUREWORK: Add more tests for scenarios of federation.
-- See also the comment in Galley.API.Update.addMembers for some other checks that are necessary.
testAddRemoteMember :: TestM ()
testAddRemoteMember = do
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice
  let localDomain = qDomain qalice
  bobId <- randomId
  let remoteDomain = Domain "far-away.example.com"
      remoteBob = Qualified bobId remoteDomain
  convId <- decodeConvId <$> postConv alice [] (Just "remote gossip") [] Nothing Nothing
  let qconvId = Qualified convId localDomain
  opts <- view tsGConf
  g <- view tsGalley
  (resp, reqs) <-
    withTempMockFederator
      opts
      remoteDomain
      (respond remoteBob)
      (postQualifiedMembers' g alice (remoteBob :| []) convId)
  liftIO $ do
    map F.domain reqs @?= replicate 2 (domainText remoteDomain)
    map (fmap F.path . F.request) reqs
      @?= [ Just "/federation/get-users-by-ids",
            Just "/federation/update-conversation-memberships"
          ]

  e <- responseJsonUnsafe <$> (pure resp <!! const 200 === statusCode)
  let bobMember = SimpleMember remoteBob roleNameWireAdmin
  liftIO $ do
    evtConv e @?= qconvId
    evtType e @?= MemberJoin
    evtData e @?= EdMembersJoin (SimpleMembers [bobMember])
    evtFrom e @?= qalice
  conv <- responseJsonUnsafeWithMsg "conversation" <$> getConvQualified alice qconvId
  liftIO $ do
    let actual = cmOthers $ cnvMembers conv
    let expected = [OtherMember remoteBob Nothing roleNameWireAdmin]
    assertEqual "other members should include remoteBob" expected actual
  where
    respond :: Qualified UserId -> F.FederatedRequest -> Value
    respond bob req
      | fmap F.component (F.request req) == Just F.Brig =
        toJSON [mkProfile bob (Name "bob")]
      | otherwise = toJSON ()

-- TODO: Extend this test to include local convs, remote failures, local non
-- exsitent convs, local convs which the requesting user is not part of and
-- remote not founds.
testGetRemoteConversations :: TestM ()
testGetRemoteConversations = do
  -- alice on local domain
  -- bob and the conversation on the remote domain
  aliceQ <- randomQualifiedUser
  let alice = qUnqualified aliceQ
  bobId <- randomId
  convId <- randomId
  let remoteDomain = Domain "far-away.example.com"
      remoteConvId = Qualified convId remoteDomain

  let aliceAsOtherMember = OtherMember aliceQ Nothing roleNameWireAdmin
      bobAsMember = Member bobId Nothing False Nothing Nothing False Nothing False Nothing roleNameWireAdmin
      mockConversation =
        Conversation
          { cnvQualifiedId = remoteConvId,
            cnvType = RegularConv,
            cnvCreator = alice,
            cnvAccess = [],
            cnvAccessRole = ActivatedAccessRole,
            cnvName = Just "federated gossip",
            cnvMembers = ConvMembers bobAsMember [aliceAsOtherMember],
            cnvTeam = Nothing,
            cnvMessageTimer = Nothing,
            cnvReceiptMode = Nothing
          }
      remoteConversationResponse = GetConversationsResponse [mockConversation]
  opts <- view tsGConf
  -- test GET /conversations/:domain/:cnv for single conversation
  (respOne, _) <-
    withTempMockFederator
      opts
      remoteDomain
      (const remoteConversationResponse)
      (getConvQualified alice remoteConvId)
  conv :: Conversation <- responseJsonUnsafe <$> (pure respOne <!! const 200 === statusCode)
  liftIO $ do
    let actual = cmOthers $ cnvMembers conv
    let expected = [OtherMember aliceQ Nothing roleNameWireAdmin]
    assertEqual "getConversation: other members should include remoteBob" expected actual

  -- insert remote conversationId for alice
  cassState <- view tsCass
  Cql.runClient cassState $ Cql.addLocalMembersToRemoteConv [alice] remoteConvId

  -- FUTUREWORK: Do this test with more than one remote domains
  -- test POST /list-conversations
  let req = ListConversations (remoteConvId :| [])
  (respAll, _) <-
    withTempMockFederator
      opts
      remoteDomain
      (const remoteConversationResponse)
      (listConvs alice req)
  convs <- responseJsonUnsafe <$> (pure respAll <!! const 200 === statusCode)
  liftIO $ do
    let expected = mockConversation
    let actual = find ((== remoteConvId) . cnvQualifiedId) (crFound convs)
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

testAddRemoteMemberFailure :: TestM ()
testAddRemoteMemberFailure = do
  alice <- randomUser
  bobId <- randomId
  charlieId <- randomId
  let remoteDomain = Domain "far-away.example.com"
      remoteBob = Qualified bobId remoteDomain
      remoteCharlie = Qualified charlieId remoteDomain
  convId <- decodeConvId <$> postConv alice [] (Just "remote gossip") [] Nothing Nothing
  opts <- view tsGConf
  g <- view tsGalley
  (resp, _) <-
    withTempMockFederator
      opts
      remoteDomain
      (const [mkProfile remoteCharlie (Name "charlie")])
      (postQualifiedMembers' g alice (remoteBob :| [remoteCharlie]) convId)
  liftIO $ statusCode resp @?= 400
  let err = responseJsonUnsafe resp :: Object
  liftIO $ (err ^. at "label") @?= Just "unknown-remote-user"

testAddDeletedRemoteUser :: TestM ()
testAddDeletedRemoteUser = do
  alice <- randomUser
  bobId <- randomId
  let remoteDomain = Domain "far-away.example.com"
      remoteBob = Qualified bobId remoteDomain
  convId <- decodeConvId <$> postConv alice [] (Just "remote gossip") [] Nothing Nothing
  opts <- view tsGConf
  g <- view tsGalley
  (resp, _) <-
    withTempMockFederator
      opts
      remoteDomain
      (const [(mkProfile remoteBob (Name "bob")) {profileDeleted = True}])
      (postQualifiedMembers' g alice (remoteBob :| []) convId)
  liftIO $ statusCode resp @?= 400
  let err = responseJsonUnsafe resp :: Object
  liftIO $ (err ^. at "label") @?= Just "unknown-remote-user"

testAddRemoteMemberInvalidDomain :: TestM ()
testAddRemoteMemberInvalidDomain = do
  alice <- randomUser
  bobId <- randomId
  let remoteBob = Qualified bobId (Domain "invalid.example.com")
  convId <- decodeConvId <$> postConv alice [] (Just "remote gossip") [] Nothing Nothing
  postQualifiedMembers alice (remoteBob :| []) convId
    !!! do
      const 422 === statusCode
      const (Just "/federation/get-users-by-ids")
        === preview (ix "data" . ix "path") . responseJsonUnsafe @Value
      const (Just "invalid.example.com")
        === preview (ix "data" . ix "domain") . responseJsonUnsafe @Value

-- This test is a safeguard to ensure adding remote members will fail
-- on environments where federation isn't configured (such as our production as of May 2021)
testAddRemoteMemberFederationDisabled :: TestM ()
testAddRemoteMemberFederationDisabled = do
  g <- view tsGalley
  alice <- randomUser
  remoteBob <- flip Qualified (Domain "some-remote-backend.example.com") <$> randomId
  convId <- decodeConvId <$> postConv alice [] (Just "remote gossip") [] Nothing Nothing
  opts <- view tsGConf
  -- federator endpoint not configured is equivalent to federation being disabled
  -- This is the case on staging/production in May 2021.
  let federatorNotConfigured :: Opts = opts & optFederator .~ Nothing
  withSettingsOverrides federatorNotConfigured $
    postQualifiedMembers' g alice (remoteBob :| []) convId !!! do
      const 400 === statusCode
      const (Just "federation-not-enabled") === fmap label . responseJsonUnsafe
  -- federator endpoint being configured in brig and/or galley, but not being
  -- available (i.e. no service listing on that IP/port) can happen due to a
  -- misconfiguration of federator. That should give a 500.
  -- Port 1 should always be wrong hopefully.
  let federatorUnavailable :: Opts = opts & optFederator ?~ Endpoint "127.0.0.1" 1
  withSettingsOverrides federatorUnavailable $
    postQualifiedMembers' g alice (remoteBob :| []) convId !!! do
      const 500 === statusCode
      const (Just "federation-not-available") === fmap label . responseJsonUnsafe

postMembersOk :: TestM ()
postMembersOk = do
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice
  bob <- randomUser
  chuck <- randomUser
  qeve <- randomQualifiedUser
  let eve = qUnqualified qeve
  connectUsers alice (list1 bob [chuck, eve])
  connectUsers eve (singleton bob)
  conv <- decodeConvId <$> postConv alice [bob, chuck] (Just "gossip") [] Nothing Nothing
  let qconv = Qualified conv (qDomain qalice)
  e <- responseJsonUnsafe <$> (postMembers alice (singleton eve) conv <!! const 200 === statusCode)
  liftIO $ do
    evtConv e @?= qconv
    evtType e @?= MemberJoin
    evtData e @?= EdMembersJoin (SimpleMembers [SimpleMember qeve roleNameWireAdmin])
    evtFrom e @?= qalice
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
  postMembers bob (singleton chuck) conv !!! do
    const 204 === statusCode
    const Nothing === responseBody
  chuck' <- responseJsonUnsafe <$> (getSelfMember chuck conv <!! const 200 === statusCode)
  liftIO $
    assertEqual "wrong self member" (Just chuck) (memId <$> chuck')

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
  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob
  connectUsers alice (singleton bob)
  conv <- decodeConvId <$> postO2OConv alice bob (Just "gossip")
  let qconv = Qualified conv (qDomain qbob)
  -- This endpoint should be deprecated but clients still use it
  WS.bracketR2 c alice bob $ \(wsA, wsB) -> do
    void $ putConversationName bob conv "gossip++" !!! const 200 === statusCode
    void . liftIO . WS.assertMatchN (5 # Second) [wsA, wsB] $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconv
      evtType e @?= ConvRename
      evtFrom e @?= qbob
      evtData e @?= EdConvRename (ConversationRename "gossip++")

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
  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob
  connectUsers alice (singleton bob)
  conv <- decodeConvId <$> postO2OConv alice bob (Just "gossip")
  let qconv = Qualified conv (qDomain qbob)
  getConv alice conv !!! const 200 === statusCode
  -- Expected member state
  let memberBob =
        Member
          { memId = bob,
            memService = Nothing,
            memOtrMuted = Just True == mupOtrMute update,
            memOtrMutedStatus = mupOtrMuteStatus update,
            memOtrMutedRef = mupOtrMuteRef update,
            memOtrArchived = Just True == mupOtrArchive update,
            memOtrArchivedRef = mupOtrArchiveRef update,
            memHidden = Just True == mupHidden update,
            memHiddenRef = mupHiddenRef update,
            memConvRoleName = fromMaybe roleNameWireAdmin (mupConvRoleName update)
          }
  -- Update member state & verify push notification
  WS.bracketR c bob $ \ws -> do
    putMember bob update conv !!! const 200 === statusCode
    void . liftIO . WS.assertMatch (5 # Second) ws $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconv
      evtType e @?= MemberStateUpdate
      evtFrom e @?= qbob
      case evtData e of
        EdMemberUpdate mis -> do
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
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice
  bob <- randomUser
  jane <- randomUser
  connectUsers alice (list1 bob [jane])
  cnv <- decodeConvId <$> postConv alice [bob, jane] (Just "gossip") [] Nothing Nothing
  let qcnv = Qualified cnv (qDomain qalice)
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
    void . liftIO $ checkWs qalice (qcnv, wsB)
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
    checkWs qalice (qcnv, ws) = WS.awaitMatch (5 # Second) ws $ \n -> do
      ntfTransient n @?= False
      let e = List1.head (WS.unpackPayload n)
      evtConv e @?= qcnv
      evtType e @?= ConvReceiptModeUpdate
      evtFrom e @?= qalice
      case evtData e of
        EdConvReceiptModeUpdate (ConversationReceiptModeUpdate (ReceiptMode mode)) ->
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
  bob <- randomQualifiedUser
  carl <- randomQualifiedUser
  let carl' = qUnqualified carl
  let bob' = qUnqualified bob
  connectUsers alice (list1 bob' [carl'])
  conv1 <- decodeConvId <$> postConv alice [bob'] (Just "gossip") [] Nothing Nothing
  conv2 <- decodeConvId <$> postConv alice [bob', carl'] (Just "gossip2") [] Nothing Nothing
  conv3 <- decodeConvId <$> postConv alice [carl'] (Just "gossip3") [] Nothing Nothing
  let qconv1 = Qualified conv1 (qDomain bob)
      qconv2 = Qualified conv2 (qDomain bob)
  WS.bracketR3 c alice bob' carl' $ \(wsA, wsB, wsC) -> do
    deleteUser bob'
    void . liftIO $
      WS.assertMatchN (5 # Second) [wsA, wsB] $
        matchMemberLeave qconv1 bob
    void . liftIO $
      WS.assertMatchN (5 # Second) [wsA, wsB, wsC] $
        matchMemberLeave qconv2 bob
  -- Check memberships
  mems1 <- fmap cnvMembers . responseJsonUnsafe <$> getConv alice conv1
  mems2 <- fmap cnvMembers . responseJsonUnsafe <$> getConv alice conv2
  mems3 <- fmap cnvMembers . responseJsonUnsafe <$> getConv alice conv3
  let other u = find ((== u) . omQualifiedId) . cmOthers
  liftIO $ do
    (mems1 >>= other bob) @?= Nothing
    (mems2 >>= other bob) @?= Nothing
    (mems2 >>= other carl) @?= Just (OtherMember carl Nothing roleNameWireAdmin)
    (mems3 >>= other bob) @?= Nothing
    (mems3 >>= other carl) @?= Just (OtherMember carl Nothing roleNameWireAdmin)
  where
    matchMemberLeave conv u n = do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= conv
      evtType e @?= MemberLeave
      evtFrom e @?= u
      evtData e @?= EdMembersLeave (UserIdList [qUnqualified u])
