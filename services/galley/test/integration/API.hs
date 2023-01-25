{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
import qualified API.MLS
import qualified API.MessageTimer as MessageTimer
import qualified API.Roles as Roles
import API.SQS
import qualified API.Teams as Teams
import qualified API.Teams.Feature as TeamFeature
import qualified API.Teams.LegalHold as Teams.LegalHold
import qualified API.Teams.LegalHold.DisabledByDefault
import API.Util
import qualified API.Util as Util
import API.Util.TeamFeature as TeamFeatures
import qualified API.Util.TeamFeature as Util
import Bilge hiding (head, timeout)
import qualified Bilge
import Bilge.Assert
import qualified Control.Concurrent.Async as Async
import Control.Exception (throw)
import Control.Lens (at, ix, preview, view, (.~), (?~))
import Control.Monad.Trans.Maybe
import Data.Aeson hiding (json)
import qualified Data.ByteString as BS
import Data.ByteString.Conversion
import qualified Data.Code as Code
import Data.Domain
import Data.Either.Extra (eitherToMaybe)
import Data.Id
import Data.Json.Util (toBase64Text, toUTCTimeMillis)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List1 hiding (head)
import qualified Data.List1 as List1
import qualified Data.Map.Strict as Map
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import Data.Singletons
import qualified Data.Text as T
import qualified Data.Text.Ascii as Ascii
import Data.Time.Clock (getCurrentTime)
import Federator.Discovery (DiscoveryFailure (..))
import Federator.MockServer
import Galley.API.Mapping
import Galley.Options (optFederator)
import Galley.Types.Conversations.Intra
import Galley.Types.Conversations.Members
import Imports
import qualified Network.HTTP.Types.Status as HTTP
import Network.Wai.Utilities.Error
import Test.QuickCheck (arbitrary, generate)
import Test.Tasty
import Test.Tasty.Cannon (TimeoutUnit (..), (#))
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Util.Options (Endpoint (Endpoint))
import Wire.API.Connection
import Wire.API.Conversation
import Wire.API.Conversation.Action
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.Event.Conversation
import Wire.API.Federation.API
import qualified Wire.API.Federation.API.Brig as F
import Wire.API.Federation.API.Common
import Wire.API.Federation.API.Galley
import qualified Wire.API.Federation.API.Galley as F
import Wire.API.Internal.Notification
import Wire.API.Message
import qualified Wire.API.Message as Message
import Wire.API.Routes.MultiTablePaging
import Wire.API.Routes.Version
import Wire.API.Routes.Versioned
import qualified Wire.API.Team.Feature as Public
import qualified Wire.API.Team.Member as Teams
import Wire.API.User
import Wire.API.User.Client
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
      Federation.tests s,
      API.MLS.tests s
    ]
  where
    mainTests =
      testGroup
        "Main Conversations API"
        [ test s "status" status,
          test s "metrics" metrics,
          test s "fetch conversation by qualified ID (v2)" testGetConvQualifiedV2,
          test s "create Proteus conversation" postProteusConvOk,
          test s "create conversation with remote users" postConvWithRemoteUsersOk,
          test s "get empty conversations" getConvsOk,
          test s "get conversations by ids" getConvsOk2,
          test s "fail to get >500 conversations with v2 API" getConvsFailMaxSizeV2,
          test s "get conversation ids with v2 API" testGetConvIdsV2,
          test s "list conversation ids" testListConvIds,
          test s "paginate through conversation ids with v2 API" paginateConvIds,
          test s "paginate through /conversations/list-ids" paginateConvListIds,
          test s "paginate through /conversations/list-ids - page ending at locals and remote domain" paginateConvListIdsPageEndingAtLocalsAndDomain,
          test s "fail to get >1000 conversation ids" getConvIdsFailMaxSize,
          test s "fail to get >1000 conversation ids with v2 API" getConvIdsFailMaxSizeV2,
          test s "page through conversations" getConvsPagingOk,
          test s "fail to create conversation when not connected" postConvFailNotConnected,
          test s "fail to create conversation with qualified users when not connected" postConvQualifiedFailNotConnected,
          test s "M:N conversation creation with N - 1 invitees should be allowed" postConvLimitOk,
          test s "M:N conversation creation must have <N members" postConvFailNumMembers,
          test s "M:N conversation creation must have <N qualified members" postConvQualifiedFailNumMembers,
          test s "fail to create conversation when blocked" postConvFailBlocked,
          test s "fail to create conversation when blocked by qualified member" postConvQualifiedFailBlocked,
          test s "fail to create conversation with remote users when remote user is not connected" postConvQualifiedNoConnection,
          test s "fail to create team conversation with remote users when remote user is not connected" postTeamConvQualifiedNoConnection,
          test s "fail to create conversation with remote users when remote user's domain doesn't exist" postConvQualifiedNonExistentDomain,
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
          test s "add guest forbidden when no guest access role" postMembersFailNoGuestAccess,
          test s "generate guest link forbidden when no guest or non-team-member access role" generateGuestLinkFailIfNoNonTeamMemberOrNoGuestAccess,
          test s "fail to add members when not connected" postMembersFail,
          test s "fail to add too many members" postTooManyMembersFail,
          test s "add remote members" testAddRemoteMember,
          test s "delete conversation with remote members" testDeleteTeamConversationWithRemoteMembers,
          test s "get conversations/:domain/:cnv - local" testGetQualifiedLocalConv,
          test s "get conversations/:domain/:cnv - local, not found" testGetQualifiedLocalConvNotFound,
          test s "get conversations/:domain/:cnv - local, not participating" testGetQualifiedLocalConvNotParticipating,
          test s "get conversations/:domain/:cnv - remote" testGetQualifiedRemoteConv,
          test s "get conversations/:domain/:cnv - remote, not found" testGetQualifiedRemoteConvNotFound,
          test s "get conversations/:domain/:cnv - remote, not found on remote" testGetQualifiedRemoteConvNotFoundOnRemote,
          test s "post conversations/list/v2" testBulkGetQualifiedConvs,
          test s "add remote members on invalid domain" testAddRemoteMemberInvalidDomain,
          test s "add remote members when federation isn't enabled" testAddRemoteMemberFederationDisabled,
          test s "add remote members when federator is unavailable" testAddRemoteMemberFederationUnavailable,
          test s "delete conversations/:domain/:cnv/members/:domain/:usr - fail, self conv" deleteMembersQualifiedFailSelf,
          test s "delete conversations/:domain:/cnv/members/:domain/:usr - fail, 1:1 conv" deleteMembersQualifiedFailO2O,
          test s "delete conversations/:domain/:cnv/members/:domain/:usr - local conv with all locals" deleteMembersConvLocalQualifiedOk,
          test s "delete conversations/:domain/:cnv/members/:domain/:usr - local conv with locals and remote, delete local" deleteLocalMemberConvLocalQualifiedOk,
          test s "delete conversations/:domain/:cnv/members/:domain/:usr - local conv with locals and remote, delete remote" deleteRemoteMemberConvLocalQualifiedOk,
          test s "delete conversations/:domain/:cnv/members/:domain/:usr - remote conv, leave conv" leaveRemoteConvQualifiedOk,
          test s "delete conversations/:domain/:cnv/members/:domain/:usr - remote conv, leave conv, non-existent" leaveNonExistentRemoteConv,
          test s "delete conversations/:domain/:cnv/members/:domain/:usr - remote conv, leave conv, denied" leaveRemoteConvDenied,
          test s "delete conversations/:domain/:cnv/members/:domain/:usr - remote conv, remove local user, fail" removeLocalMemberConvQualifiedFail,
          test s "delete conversations/:domain/:cnv/members/:domain/:usr - remote conv, remove remote user, fail" removeRemoteMemberConvQualifiedFail,
          test s "rename conversation (deprecated endpoint)" putConvDeprecatedRenameOk,
          test s "rename conversation" putConvRenameOk,
          test s "rename qualified conversation" putQualifiedConvRenameOk,
          test s "rename qualified conversation with remote members" putQualifiedConvRenameWithRemotesOk,
          test s "rename qualified conversation failure" putQualifiedConvRenameFailure,
          test s "other member update role" putOtherMemberOk,
          test s "qualified other member update role" putQualifiedOtherMemberOk,
          test s "member update (otr mute)" putMemberOtrMuteOk,
          test s "member update (otr archive)" putMemberOtrArchiveOk,
          test s "member update (hidden)" putMemberHiddenOk,
          test s "member update (everything b)" putMemberAllOk,
          test s "remote conversation member update (otr mute)" putRemoteConvMemberOtrMuteOk,
          test s "remote conversation member update (otr archive)" putRemoteConvMemberOtrArchiveOk,
          test s "remote conversation member update (otr hidden)" putRemoteConvMemberHiddenOk,
          test s "remote conversation member update (everything)" putRemoteConvMemberAllOk,
          test s "conversation receipt mode update" putReceiptModeOk,
          test s "conversation receipt mode update with remote members" putReceiptModeWithRemotesOk,
          test s "remote conversation receipt mode update" putRemoteReceiptModeOk,
          test s "leave connect conversation" leaveConnectConversation,
          test s "post conversations/:cnv/otr/message: message delivery and missing clients" postCryptoMessageVerifyMsgSentAndRejectIfMissingClient,
          test s "post conversations/:cnv/otr/message: mismatch and prekey fetching" postCryptoMessageVerifyRejectMissingClientAndRepondMissingPrekeysJson,
          test s "post conversations/:cnv/otr/message: mismatch with protobuf" postCryptoMessageVerifyRejectMissingClientAndRepondMissingPrekeysProto,
          test s "post conversations/:cnv/otr/message: unknown sender client" postCryptoMessageNotAuthorizeUnknownClient,
          test s "post conversations/:cnv/otr/message: ignore_missing and report_missing" postCryptoMessageVerifyCorrectResponseIfIgnoreAndReportMissingQueryParam,
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
          test s "local and remote guests are removed when access changes" testAccessUpdateGuestRemoved,
          test s "team member can't join via guest link if access role removed" testTeamMemberCantJoinViaGuestLinkIfAccessRoleRemoved,
          test s "cannot join private conversation" postJoinConvFail,
          test s "revoke guest links for team conversation" testJoinTeamConvGuestLinksDisabled,
          test s "revoke guest links for non-team conversation" testJoinNonTeamConvGuestLinksDisabled,
          test s "get code rejected if guest links disabled" testGetCodeRejectedIfGuestLinksDisabled,
          test s "post code rejected if guest links disabled" testPostCodeRejectedIfGuestLinksDisabled,
          test s "remove user with only local convs" removeUserNoFederation,
          test s "remove user with local and remote convs" removeUser,
          test s "iUpsertOne2OneConversation" testAllOne2OneConversationRequests,
          test s "post message - reject if missing client" postMessageRejectIfMissingClients,
          test s "post message - client that is not in group doesn't receive message" postMessageClientNotInGroupDoesNotReceiveMsg,
          test s "get guest links status from foreign team conversation" getGuestLinksStatusFromForeignTeamConv,
          testGroup
            "Typing indicators"
            [ test s "send typing indicators" postTypingIndicators,
              test s "send typing indicators without domain" postTypingIndicatorsV2,
              test s "send typing indicators with invalid pyaload" postTypingIndicatorsHandlesNonsense,
              test s "POST /federation/on-typing-indicator-updated : Update typing indicator by remote user" updateTypingIndicatorFromRemoteUser,
              test s "POST /federation/on-typing-indicator-updated : Update typing indicator to remote user" updateTypingIndicatorToRemoteUser,
              test s "send typing indicator update from local to remote on remote conv" updateTypingIndicatorToRemoteUserRemoteConv
            ]
        ]

-------------------------------------------------------------------------------
-- API Tests

status :: TestM ()
status = do
  g <- viewGalley
  get (g . path "/i/status")
    !!! const 200 === statusCode
  Bilge.head (g . path "/i/status")
    !!! const 200 === statusCode

metrics :: TestM ()
metrics = do
  g <- viewGalley
  get (g . path "/i/metrics") !!! do
    const 200 === statusCode
    -- Should contain the request duration metric in its output
    const (Just "TYPE http_request_duration_seconds histogram") =~= responseBody

testGetConvQualifiedV2 :: TestM ()
testGetConvQualifiedV2 = do
  alice <- randomUser
  bob <- randomUser
  connectUsers alice (list1 bob [])
  conv <-
    responseJsonError
      =<< postConvQualified
        alice
        defNewProteusConv
          { newConvUsers = [bob]
          }
        <!! const 201 === statusCode
  let qcnv = cnvQualifiedId conv
  conv' <-
    fmap (unVersioned @'V2) . responseJsonError
      =<< getConvQualifiedV2 alice qcnv
        <!! const 200 === statusCode
  liftIO $ conv @=? conv'

postProteusConvOk :: TestM ()
postProteusConvOk = do
  c <- view tsCannon
  (alice, qalice) <- randomUserTuple
  (bob, qbob) <- randomUserTuple
  (jane, qjane) <- randomUserTuple
  connectUsers alice (list1 bob [jane])
  let nameMaxSize = T.replicate 256 "a"
  WS.bracketR3 c alice bob jane $ \(wsA, wsB, wsJ) -> do
    rsp <-
      postConv alice [bob, jane] (Just nameMaxSize) [] Nothing Nothing
        <!! const 201 === statusCode
    qcid <- assertConv rsp RegularConv alice qalice [qbob, qjane] (Just nameMaxSize) Nothing
    let cid = qUnqualified qcid
    cvs <- mapM (convView cid) [alice, bob, jane]
    liftIO $ mapM_ WS.assertSuccess =<< Async.mapConcurrently (checkWs qalice) (zip cvs [wsA, wsB, wsJ])
  where
    convView cnv usr = do
      r <- getConv usr cnv <!! const 200 === statusCode
      unVersioned @'V2 <$> responseJsonError r
    checkWs qalice (cnv, ws) = WS.awaitMatch (5 # Second) ws $ \n -> do
      ntfTransient n @?= False
      let e = List1.head (WS.unpackPayload n)
      evtConv e @?= cnvQualifiedId cnv
      evtType e @?= ConvCreate
      evtFrom e @?= qalice
      case evtData e of
        EdConversation c' -> assertConvEquals cnv c'
        _ -> assertFailure "Unexpected event data"

postConvWithRemoteUsersOk :: TestM ()
postConvWithRemoteUsersOk = do
  c <- view tsCannon
  (alice, qAlice) <- randomUserTuple
  (alex, qAlex) <- randomUserTuple
  (amy, qAmy) <- randomUserTuple
  connectUsers alice (list1 alex [amy])
  let cDomain = Domain "c.example.com"
      dDomain = Domain "d.example.com"
  qChad <- randomQualifiedId cDomain
  qCharlie <- randomQualifiedId cDomain
  qDee <- randomQualifiedId dDomain
  mapM_ (connectWithRemoteUser alice) [qChad, qCharlie, qDee]

  let nameMaxSize = T.replicate 256 "a"
  WS.bracketR3 c alice alex amy $ \(wsAlice, wsAlex, wsAmy) -> do
    (rsp, federatedRequests) <-
      withTempMockFederator' (mockReply ()) $
        postConvQualified alice defNewProteusConv {newConvName = checked nameMaxSize, newConvQualifiedUsers = [qAlex, qAmy, qChad, qCharlie, qDee]}
          <!! const 201 === statusCode
    qcid <- assertConv rsp RegularConv alice qAlice [qAlex, qAmy, qChad, qCharlie, qDee] (Just nameMaxSize) Nothing
    let cid = qUnqualified qcid
    cvs <- mapM (convView cid) [alice, alex, amy]
    liftIO $ mapM_ WS.assertSuccess =<< Async.mapConcurrently (checkWs qAlice) (zip cvs [wsAlice, wsAlex, wsAmy])

    cFedReq <- assertOne $ filter (\r -> frTargetDomain r == cDomain) federatedRequests
    cFedReqBody <- assertRight $ parseFedRequest cFedReq

    dFedReq <- assertOne $ filter (\r -> frTargetDomain r == dDomain) federatedRequests
    dFedReqBody <- assertRight $ parseFedRequest dFedReq

    liftIO $ do
      length federatedRequests @?= 2

      F.ccOrigUserId cFedReqBody @?= alice
      F.ccCnvId cFedReqBody @?= cid
      F.ccCnvType cFedReqBody @?= RegularConv
      F.ccCnvAccess cFedReqBody @?= [InviteAccess]
      F.ccCnvAccessRoles cFedReqBody @?= Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole, ServiceAccessRole]
      F.ccCnvName cFedReqBody @?= Just nameMaxSize
      F.ccNonCreatorMembers cFedReqBody @?= Set.fromList (toOtherMember <$> [qAlex, qAmy, qChad, qCharlie, qDee])
      F.ccMessageTimer cFedReqBody @?= Nothing
      F.ccReceiptMode cFedReqBody @?= Nothing

      dFedReqBody @?= cFedReqBody
  where
    toOtherMember qid = OtherMember qid Nothing roleNameWireAdmin
    convView cnv usr = do
      r <- getConv usr cnv <!! const 200 === statusCode
      unVersioned @'V2 <$> responseJsonError r
    checkWs qalice (cnv, ws) = WS.awaitMatch (5 # Second) ws $ \n -> do
      ntfTransient n @?= False
      let e = List1.head (WS.unpackPayload n)
      evtConv e @?= cnvQualifiedId cnv
      evtType e @?= ConvCreate
      evtFrom e @?= qalice
      case evtData e of
        EdConversation c' -> assertConvEquals cnv c'
        _ -> assertFailure "Unexpected event data"

-- @SF.Separation @TSFI.RESTfulAPI @S2
-- This test verifies whether a message actually gets sent all the way to
-- cannon.
postCryptoMessageVerifyMsgSentAndRejectIfMissingClient :: TestM ()
postCryptoMessageVerifyMsgSentAndRejectIfMissingClient = do
  localDomain <- viewFederationDomain
  c <- view tsCannon
  (alice, ac) <- randomUserWithClient (head someLastPrekeys)
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
    liftIO $
      WS.assertMatch_ (5 # WS.Second) wsE $
        wsAssertClientRemoved ec
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

-- @END

-- @SF.Separation @TSFI.RESTfulAPI @S2
-- This test verifies basic mismatch behavior of the the JSON endpoint.
postCryptoMessageVerifyRejectMissingClientAndRepondMissingPrekeysJson :: TestM ()
postCryptoMessageVerifyRejectMissingClientAndRepondMissingPrekeysJson = do
  (alice, ac) <- randomUserWithClient (head someLastPrekeys)
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
  b <- view tsUnversionedBrig
  r2 <-
    post (b . zUser alice . path "v1/users/prekeys" . json (missingClients x))
      <!! const 200 === statusCode
  let p = responseJsonUnsafeWithMsg "prekeys" r2 :: UserClientPrekeyMap
  liftIO $ do
    Map.keys (userClientMap (getUserClientPrekeyMap p)) @=? [eve]
    Map.keys <$> Map.lookup eve (userClientMap (getUserClientPrekeyMap p)) @=? Just [ec]

-- @END

-- @SF.Separation @TSFI.RESTfulAPI @S2
-- This test verifies basic mismatch behaviour of the protobuf endpoint.
postCryptoMessageVerifyRejectMissingClientAndRepondMissingPrekeysProto :: TestM ()
postCryptoMessageVerifyRejectMissingClientAndRepondMissingPrekeysProto = do
  (alice, ac) <- randomUserWithClient (head someLastPrekeys)
  (bob, bc) <- randomUserWithClient (someLastPrekeys !! 1)
  (eve, ec) <- randomUserWithClient (someLastPrekeys !! 2)
  connectUsers alice (list1 bob [eve])
  conv <- decodeConvId <$> postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
  -- Missing eve
  let ciphertext = toBase64Text "hello bob"
  let m = otrRecipients [(bob, bc, ciphertext)]
  r1 <-
    postProtoOtrMessage alice ac conv m
      <!! const 412 === statusCode
  let x = responseJsonUnsafeWithMsg "ClientMismatch" r1
  pure r1
    !!! assertMismatchWithMessage (Just "client mismatch") [(eve, Set.singleton ec)] [] []
  -- Fetch all missing clients prekeys
  b <- view tsUnversionedBrig
  r2 <-
    post (b . zUser alice . path "v1/users/prekeys" . json (missingClients x))
      <!! const 200 === statusCode
  let p = responseJsonUnsafeWithMsg "prekeys" r2 :: UserClientPrekeyMap
  liftIO $ do
    Map.keys (userClientMap (getUserClientPrekeyMap p)) @=? [eve]
    Map.keys <$> Map.lookup eve (userClientMap (getUserClientPrekeyMap p)) @=? Just [ec]

-- @END

-- | This test verifies behaviour when an unknown client posts the message. Only
-- tests the Protobuf endpoint.
postCryptoMessageNotAuthorizeUnknownClient :: TestM ()
postCryptoMessageNotAuthorizeUnknownClient = do
  alice <- randomUser
  bob <- randomUser
  bc <- randomClient bob (head someLastPrekeys)
  connectUsers alice (list1 bob [])
  conv <- decodeConvId <$> postConv alice [bob] (Just "gossip") [] Nothing Nothing
  -- Unknown client ID => 403
  let ciphertext = toBase64Text "hello bob"
  let m = otrRecipients [(bob, bc, ciphertext)]
  postProtoOtrMessage alice (ClientId "172618352518396") conv m
    !!! const 403 === statusCode

-- @SF.Separation @TSFI.RESTfulAPI @S2
-- This test verifies the following scenario.
-- A client sends a message to all clients of a group and one more who is not part of the group.
-- The server must not send this message to client ids not part of the group.
postMessageClientNotInGroupDoesNotReceiveMsg :: TestM ()
postMessageClientNotInGroupDoesNotReceiveMsg = do
  localDomain <- viewFederationDomain
  cannon <- view tsCannon
  (alice, ac) <- randomUserWithClient (head someLastPrekeys)
  (bob, bc) <- randomUserWithClient (someLastPrekeys !! 1)
  (eve, ec) <- randomUserWithClient (someLastPrekeys !! 2)
  (chad, cc) <- randomUserWithClient (someLastPrekeys !! 3)
  connectUsers alice (list1 bob [eve, chad])
  conversationWithAllButChad <- decodeConvId <$> postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
  let qalice = Qualified alice localDomain
      qconv = Qualified conversationWithAllButChad localDomain
  WS.bracketR3 cannon bob eve chad $ \(wsBob, wsEve, wsChad) -> do
    let msgToAllIncludingChad = [(bob, bc, toBase64Text "ciphertext2"), (eve, ec, toBase64Text "ciphertext2"), (chad, cc, toBase64Text "ciphertext2")]
    postOtrMessage id alice ac conversationWithAllButChad msgToAllIncludingChad !!! const 201 === statusCode
    let checkBobGetsMsg = void . liftIO $ WS.assertMatch (5 # Second) wsBob (wsAssertOtr qconv qalice ac bc (toBase64Text "ciphertext2"))
    let checkEveGetsMsg = void . liftIO $ WS.assertMatch (5 # Second) wsEve (wsAssertOtr qconv qalice ac ec (toBase64Text "ciphertext2"))
    let checkChadDoesNotGetMsg = assertNoMsg wsChad (wsAssertOtr qconv qalice ac ac (toBase64Text "ciphertext2"))
    checkBobGetsMsg
    checkEveGetsMsg
    checkChadDoesNotGetMsg

-- @END

-- @SF.Separation @TSFI.RESTfulAPI @S2
-- This test verifies that when a client sends a message not to all clients of a group then the server should reject the message and sent a notification to the sender (412 Missing clients).
-- The test is somewhat redundant because this is already tested as part of other tests already. This is a stand alone test that solely tests the behavior described above.
postMessageRejectIfMissingClients :: TestM ()
postMessageRejectIfMissingClients = do
  (sender, senderClient) : allReceivers <- randomUserWithClient `traverse` someLastPrekeys
  let (receiver1, receiverClient1) : otherReceivers = allReceivers
  connectUsers sender (list1 receiver1 (fst <$> otherReceivers))
  conv <- decodeConvId <$> postConv sender (receiver1 : (fst <$> otherReceivers)) (Just "gossip") [] Nothing Nothing
  let msgToAllClients = mkMsg "hello!" <$> allReceivers
  let msgMissingClients = mkMsg "hello!" <$> drop 1 allReceivers

  let checkSendToAllClientShouldBeSuccessful =
        postOtrMessage id sender senderClient conv msgToAllClients !!! do
          const 201 === statusCode
          assertMismatch [] [] []

  let checkSendWitMissingClientsShouldFail =
        postOtrMessage id sender senderClient conv msgMissingClients !!! do
          const 412 === statusCode
          assertMismatch [(receiver1, Set.singleton receiverClient1)] [] []

  checkSendToAllClientShouldBeSuccessful
  checkSendWitMissingClientsShouldFail
  where
    mkMsg :: ByteString -> (UserId, ClientId) -> (UserId, ClientId, Text)
    mkMsg text (userId, clientId) = (userId, clientId, toBase64Text text)

-- @END

-- @SF.Separation @TSFI.RESTfulAPI @S2
-- This test verifies behaviour under various values of ignore_missing and
-- report_missing. Only tests the JSON endpoint.
postCryptoMessageVerifyCorrectResponseIfIgnoreAndReportMissingQueryParam :: TestM ()
postCryptoMessageVerifyCorrectResponseIfIgnoreAndReportMissingQueryParam = do
  (alice, ac) <- randomUserWithClient (head someLastPrekeys)
  (bob, bc) <- randomUserWithClient (someLastPrekeys !! 1)
  (chad, cc) <- randomUserWithClient (someLastPrekeys !! 2)
  (eve, ec) <- randomUserWithClient (someLastPrekeys !! 3)
  connectUsers alice (list1 bob [chad, eve])
  conv <- decodeConvId <$> postConv alice [bob, chad, eve] (Just "gossip") [] Nothing Nothing
  -- Missing eve
  let msgMissingChadAndEve = [(bob, bc, toBase64Text "hello bob")]
  let m' = otrRecipients [(bob, bc, toBase64Text "hello bob")]
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

-- @END

-- | Sets up a conversation on Backend A known as "owning backend". All user's
-- on this backend have names begining with 'A'. The conversation has a couple
-- of users from backend B and one user from backend C.
--
-- One of the users from Backend A will send the message, it is expected that
-- message will be sent successfully.
postMessageQualifiedLocalOwningBackendSuccess :: TestM ()
postMessageQualifiedLocalOwningBackendSuccess = do
  -- WS receive timeout
  let t = 5 # Second
  -- Cannon for local users
  cannon <- view tsCannon
  -- Domain which owns the converstaion
  owningDomain <- viewFederationDomain

  (alice, aliceClient) <- randomUserWithClientQualified (head someLastPrekeys)
  (alex, alexClient) <- randomUserWithClientQualified (someLastPrekeys !! 1)
  alexClient2 <- randomClient (qUnqualified alex) (someLastPrekeys !! 2)
  (amy, amyClient) <- randomUserWithClientQualified (someLastPrekeys !! 3)

  let bDomain = Domain "b.far-away.example.com"
      cDomain = Domain "c.far-away.example.com"
      randomQuidAndClients d n = (,) <$> randomQualifiedId d <*> liftIO (replicateM n $ generate arbitrary)
  (bob, [bobClient]) <- randomQuidAndClients bDomain 1
  (bart, [bartClient1, bartClient2]) <- randomQuidAndClients bDomain 2
  (carl, [carlClient]) <- randomQuidAndClients cDomain 1

  let aliceU = qUnqualified alice
      alexU = qUnqualified alex
      amyU = qUnqualified amy

  connectLocalQualifiedUsers aliceU (list1 alex [amy])
  forM_ [bob, bart, carl] $ connectWithRemoteUser aliceU

  resp <-
    postConvWithRemoteUsers
      aliceU
      defNewProteusConv {newConvQualifiedUsers = [alex, amy, bob, bart, carl]}
  let convId = (`Qualified` owningDomain) . decodeConvId $ resp

  WS.bracketAsClientRN cannon [(alexU, alexClient), (alexU, alexClient2), (amyU, amyClient)] $ \[wsAlex1, wsAlex2, wsAmy] -> do
    let message =
          [ (alex, alexClient, "text-for-alex"),
            (alex, alexClient2, "text-for-alex2"),
            (amy, amyClient, "text-for-amy"),
            (bob, bobClient, "text-for-bob"),
            (bart, bartClient1, "text-for-bart1"),
            (bart, bartClient2, "text-for-bart2"),
            (carl, carlClient, "text-for-carl")
          ]

    let mkPubClient c = PubClient c Nothing
        brigMock = do
          guardRPC "get-user-clients"
          d <- frTargetDomain <$> getRequest
          asum
            [ do
                guard (d == bDomain)

                mockReply $
                  UserMap . Map.fromList $
                    [ (qUnqualified bob, Set.singleton (mkPubClient bobClient)),
                      (qUnqualified bart, Set.fromList (map mkPubClient [bartClient1, bartClient2]))
                    ],
              do
                guard (d == cDomain)
                mockReply $
                  UserMap
                    ( Map.singleton
                        (qUnqualified carl)
                        (Set.singleton (PubClient carlClient Nothing))
                    )
            ]
        galleyMock = "on-message-sent" ~> ()

    (resp2, requests) <- postProteusMessageQualifiedWithMockFederator aliceU aliceClient convId message "data" Message.MismatchReportAll (brigMock <|> galleyMock)
    pure resp2 !!! do
      const 201 === statusCode
      assertMismatchQualified mempty mempty mempty mempty
    let encodedTextForAlex1 = toBase64Text "text-for-alex"
        encodedTextForAlex2 = toBase64Text "text-for-alex2"
        encodedTextForAmy = toBase64Text "text-for-amy"
        encodedTextForBob = toBase64Text "text-for-bob"
        encodedTextForBart1 = toBase64Text "text-for-bart1"
        encodedTextForBart2 = toBase64Text "text-for-bart2"
        encodedTextForCarl = toBase64Text "text-for-carl"
        encodedData = toBase64Text "data"
    liftIO $ do
      let matchReq domain component r = frTargetDomain r == domain && frComponent r == component
          filterReq domain component = filter (matchReq domain component) requests
      bBrigReq <- assertOne $ filterReq bDomain Brig
      bGalleyReq <- assertOne $ filterReq bDomain Galley
      cBrigReq <- assertOne $ filterReq cDomain Brig
      cGalleyReq <- assertOne $ filterReq cDomain Galley

      frRPC bBrigReq @?= "get-user-clients"
      (sort . F.gucUsers <$> parseFedRequest bBrigReq) @?= Right (sort $ qUnqualified <$> [bob, bart])
      frRPC cBrigReq @?= "get-user-clients"
      parseFedRequest cBrigReq @?= Right (F.GetUserClients [qUnqualified carl])

      frRPC bGalleyReq @?= "on-message-sent"
      bActualNotif <- assertRight $ parseFedRequest bGalleyReq
      let bExpectedNotif =
            F.RemoteMessage
              { rmTime = F.rmTime bActualNotif,
                rmData = Just $ toBase64Text "data",
                rmSender = alice,
                rmSenderClient = aliceClient,
                rmConversation = qUnqualified convId,
                rmPriority = Nothing,
                rmPush = True,
                rmTransient = False,
                rmRecipients =
                  UserClientMap $
                    Map.fromList
                      [ (qUnqualified bob, Map.singleton bobClient encodedTextForBob),
                        ( qUnqualified bart,
                          Map.fromList
                            [ (bartClient1, encodedTextForBart1),
                              (bartClient2, encodedTextForBart2)
                            ]
                        )
                      ]
              }
      bActualNotif @?= bExpectedNotif
      frRPC cGalleyReq @?= "on-message-sent"
      cActualNotif <- assertRight $ parseFedRequest cGalleyReq
      let cExpectedNotif =
            bExpectedNotif
              { F.rmRecipients =
                  UserClientMap $ Map.fromList [(qUnqualified carl, Map.singleton carlClient encodedTextForCarl)]
              }
      cActualNotif @?= cExpectedNotif

      WS.assertMatch_ t wsAlex1 (wsAssertOtr' encodedData convId alice aliceClient alexClient encodedTextForAlex1)
      WS.assertMatch_ t wsAlex2 (wsAssertOtr' encodedData convId alice aliceClient alexClient2 encodedTextForAlex2)
      WS.assertMatch_ t wsAmy (wsAssertOtr' encodedData convId alice aliceClient amyClient encodedTextForAmy)

-- @SF.Separation @TSFI.RESTfulAPI @S2
-- Sets up a conversation on Backend A known as "owning backend". One of the
-- users from Backend A will send the message but have a missing client. It is
-- expected that the message will not be sent.
postMessageQualifiedLocalOwningBackendMissingClients :: TestM ()
postMessageQualifiedLocalOwningBackendMissingClients = do
  -- Cannon for local users
  cannon <- view tsCannon
  -- Domain which owns the converstaion
  owningDomain <- viewFederationDomain

  (aliceOwningDomain, aliceClient) <- randomUserWithClientQualified (head someLastPrekeys)
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
  connectWithRemoteUser aliceUnqualified deeRemote

  -- FUTUREWORK: Do this test with more than one remote domains
  resp <-
    postConvWithRemoteUsers
      aliceUnqualified
      defNewProteusConv {newConvQualifiedUsers = [bobOwningDomain, chadOwningDomain, deeRemote]}
  let convId = (`Qualified` owningDomain) . decodeConvId $ resp

  -- Missing Bob, chadClient2 and Dee
  let message = [(chadOwningDomain, chadClient, "text-for-chad")]
  -- FUTUREWORK: Mock federator and ensure that message is not propagated to remotes
  WS.bracketR2 cannon bobUnqualified chadUnqualified $ \(wsBob, wsChad) -> do
    let mock = "get-user-clients" ~> UserMap (Map.singleton (qUnqualified deeRemote) (Set.singleton (PubClient deeClient Nothing)))
    (resp2, _requests) <- postProteusMessageQualifiedWithMockFederator aliceUnqualified aliceClient convId message "data" Message.MismatchReportAll mock

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

-- @END

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

  (aliceOwningDomain, aliceClient) <- randomUserWithClientQualified (head someLastPrekeys)
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
  connectWithRemoteUser aliceUnqualified deeRemote

  -- FUTUREWORK: Do this test with more than one remote domains
  resp <-
    postConvWithRemoteUsers
      aliceUnqualified
      defNewProteusConv {newConvQualifiedUsers = [bobOwningDomain, chadOwningDomain, deeRemote]}
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
    let brigMock = do
          guardRPC "get-user-clients"
          getUserClients <- getRequestBody
          let lookupClients uid
                | uid == deeRemoteUnqualified = Just (uid, Set.fromList [PubClient deeClient Nothing])
                | uid == nonMemberRemoteUnqualified = Just (uid, Set.fromList [PubClient nonMemberRemoteClient Nothing])
                | otherwise = Nothing
          mockReply $ UserMap . Map.fromList . mapMaybe lookupClients $ F.gucUsers getUserClients
        galleyMock = "on-message-sent" ~> ()

    (resp2, _requests) <- postProteusMessageQualifiedWithMockFederator aliceUnqualified aliceClient convId message "data" Message.MismatchReportAll (brigMock <|> galleyMock)
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

-- @SF.Separation @TSFI.RESTfulAPI @S2
-- Sets up a conversation on Backend A known as "owning backend". One of the
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

  (aliceOwningDomain, aliceClient) <- randomUserWithClientQualified (head someLastPrekeys)
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
  connectWithRemoteUser aliceUnqualified deeRemote

  -- FUTUREWORK: Do this test with more than one remote domains
  resp <-
    postConvWithRemoteUsers
      aliceUnqualified
      defNewProteusConv {newConvQualifiedUsers = [bobOwningDomain, chadOwningDomain, deeRemote]}
  let convId = (`Qualified` owningDomain) . decodeConvId $ resp

  let mock =
        "get-user-clients" ~>
          UserMap (Map.singleton (qUnqualified deeRemote) (Set.singleton (PubClient deeClient Nothing)))

  -- Missing Bob, chadClient2 and Dee
  let message = [(chadOwningDomain, chadClient, "text-for-chad")]
  -- FUTUREWORK: Mock federator and ensure that clients of Dee are checked. Also
  -- ensure that message is not propagated to remotes
  WS.bracketR2 cannon bobUnqualified chadUnqualified $ \(wsBob, wsChad) -> do
    (resp2, _requests) <- postProteusMessageQualifiedWithMockFederator aliceUnqualified aliceClient convId message "data" Message.MismatchIgnoreAll mock
    pure resp2 !!! do
      const 201 === statusCode
      assertMismatchQualified mempty mempty mempty mempty
    let encodedTextForChad = toBase64Text "text-for-chad"
        encodedData = toBase64Text "data"
    WS.assertMatch_ t wsChad (wsAssertOtr' encodedData convId aliceOwningDomain aliceClient chadClient encodedTextForChad)
    WS.assertNoEvent (1 # Second) [wsBob]

  -- Another way to ignore all is to report nobody
  WS.bracketR2 cannon bobUnqualified chadUnqualified $ \(wsBob, wsChad) -> do
    (resp2, _requests) <- postProteusMessageQualifiedWithMockFederator aliceUnqualified aliceClient convId message "data" (Message.MismatchReportOnly mempty) mock
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
        mock
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
        mock
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
        mock
    pure resp2 !!! do
      const 412 === statusCode
      let expectedMissing =
            QualifiedUserClients . Map.singleton remoteDomain . Map.fromList $
              [(qUnqualified deeRemote, Set.singleton deeClient)]
      assertMismatchQualified mempty expectedMissing mempty mempty
    WS.assertNoEvent (1 # Second) [wsBob, wsChad]

-- @END

postMessageQualifiedLocalOwningBackendFailedToSendClients :: TestM ()
postMessageQualifiedLocalOwningBackendFailedToSendClients = do
  -- WS receive timeout
  let t = 5 # Second
  -- Cannon for local users
  cannon <- view tsCannon
  -- Domain which owns the converstaion
  owningDomain <- viewFederationDomain

  (aliceOwningDomain, aliceClient) <- randomUserWithClientQualified (head someLastPrekeys)
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
  connectWithRemoteUser aliceUnqualified deeRemote

  -- FUTUREWORK: Do this test with more than one remote domains
  resp <-
    postConvWithRemoteUsers
      aliceUnqualified
      defNewProteusConv {newConvQualifiedUsers = [bobOwningDomain, chadOwningDomain, deeRemote]}
  let convId = (`Qualified` owningDomain) . decodeConvId $ resp

  WS.bracketR2 cannon bobUnqualified chadUnqualified $ \(wsBob, wsChad) -> do
    let message =
          [ (bobOwningDomain, bobClient, "text-for-bob"),
            (bobOwningDomain, bobClient2, "text-for-bob2"),
            (chadOwningDomain, chadClient, "text-for-chad"),
            (deeRemote, deeClient, "text-for-dee")
          ]

    let mock =
          ( "get-user-clients" ~>
              UserMap (Map.singleton (qUnqualified deeRemote) (Set.singleton (PubClient deeClient Nothing)))
          )
            <|> ( guardRPC "on-message-sent"
                    *> throw (MockErrorResponse HTTP.status503 "Down for maintenance.")
                )

    (resp2, _requests) <- postProteusMessageQualifiedWithMockFederator aliceUnqualified aliceClient convId message "data" Message.MismatchReportAll mock

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
  (aliceLocal, aliceClient) <- randomUserWithClientQualified (head someLastPrekeys)
  let aliceUnqualified = qUnqualified aliceLocal
  convIdUnqualified <- randomId
  let remoteDomain = Domain "far-away.example.com"
      convId = Qualified convIdUnqualified remoteDomain

  let mock =
        guardRPC "send-message"
          *> throw (MockErrorResponse HTTP.status503 "Down for maintenance.")

  (resp2, _requests) <-
    postProteusMessageQualifiedWithMockFederator aliceUnqualified aliceClient convId [] "data" Message.MismatchReportAll mock

  pure resp2 !!! do
    const 503 === statusCode

postMessageQualifiedRemoteOwningBackendSuccess :: TestM ()
postMessageQualifiedRemoteOwningBackendSuccess = do
  (aliceLocal, aliceClient) <- randomUserWithClientQualified (head someLastPrekeys)
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
      mock = "send-message" ~> F.MessageSendResponse (Right mss)
  (resp2, _requests) <-
    postProteusMessageQualifiedWithMockFederator aliceUnqualified aliceClient convId message "data" Message.MismatchReportAll mock

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
  Right noGuestsAccess <- liftIO $ genAccessRolesV2 [NonTeamMemberAccessRole] [GuestAccessRole]
  alice <- randomUser
  convId <- decodeConvId <$> postConv alice [] (Just convName) [CodeAccess] (Just noGuestsAccess) Nothing
  cCode <- decodeConvCodeEvent <$> postConvCode alice convId

  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob
  getJoinCodeConv bob (conversationKey cCode) (conversationCode cCode) !!! do
    const (Right (ConversationCoverView convId (Just convName))) === responseJsonEither

  -- A user that would not be able to join conversation cannot view it either.
  eve <- ephemeralUser
  getJoinCodeConv eve (conversationKey cCode) (conversationCode cCode) !!! do
    const 403 === statusCode

testGetCodeRejectedIfGuestLinksDisabled :: TestM ()
testGetCodeRejectedIfGuestLinksDisabled = do
  galley <- viewGalley
  (owner, teamId, []) <- Util.createBindingTeamWithNMembers 0
  Right accessRoles <- liftIO $ genAccessRolesV2 [TeamMemberAccessRole, GuestAccessRole] []
  let createConvWithGuestLink = do
        convId <- decodeConvId <$> postTeamConv teamId owner [] (Just "testConversation") [CodeAccess] (Just accessRoles) Nothing
        void $ decodeConvCodeEvent <$> postConvCode owner convId
        pure convId
  convId <- createConvWithGuestLink
  let checkGetCode expectedStatus = getConvCode owner convId !!! const expectedStatus === statusCode
  let setStatus tfStatus =
        TeamFeatures.putTeamFeatureFlagWithGalley @Public.GuestLinksConfig galley owner teamId (Public.WithStatusNoLock tfStatus Public.GuestLinksConfig Public.FeatureTTLUnlimited) !!! do
          const 200 === statusCode

  checkGetCode 200
  setStatus Public.FeatureStatusDisabled
  checkGetCode 409
  setStatus Public.FeatureStatusEnabled
  checkGetCode 200

testPostCodeRejectedIfGuestLinksDisabled :: TestM ()
testPostCodeRejectedIfGuestLinksDisabled = do
  galley <- viewGalley
  (owner, teamId, []) <- Util.createBindingTeamWithNMembers 0
  Right noGuestsAccess <- liftIO $ genAccessRolesV2 [NonTeamMemberAccessRole] [GuestAccessRole]
  convId <- decodeConvId <$> postTeamConv teamId owner [] (Just "testConversation") [CodeAccess] (Just noGuestsAccess) Nothing
  let checkPostCode expectedStatus = postConvCode owner convId !!! statusCode === const expectedStatus
  let setStatus tfStatus =
        TeamFeatures.putTeamFeatureFlagWithGalley @Public.GuestLinksConfig galley owner teamId (Public.WithStatusNoLock tfStatus Public.GuestLinksConfig Public.FeatureTTLUnlimited) !!! do
          const 200 === statusCode

  checkPostCode 201
  setStatus Public.FeatureStatusDisabled
  checkPostCode 409
  setStatus Public.FeatureStatusEnabled
  checkPostCode 200

-- @SF.Separation @TSFI.RESTfulAPI @S2
-- Check if guests cannot join anymore if guest invite feature was disabled on team level
testJoinTeamConvGuestLinksDisabled :: TestM ()
testJoinTeamConvGuestLinksDisabled = do
  galley <- viewGalley
  let convName = "testConversation"
  (owner, teamId, [alice]) <- Util.createBindingTeamWithNMembers 1
  eve <- ephemeralUser
  bob <- randomUser
  Right accessRoles <- liftIO $ genAccessRolesV2 [TeamMemberAccessRole, NonTeamMemberAccessRole, GuestAccessRole] []
  convId <- decodeConvId <$> postTeamConv teamId owner [] (Just convName) [CodeAccess, LinkAccess] (Just accessRoles) Nothing
  cCode <- decodeConvCodeEvent <$> postConvCode owner convId

  let checkFeatureStatus fstatus =
        Util.getTeamFeatureFlagWithGalley @Public.GuestLinksConfig galley owner teamId !!! do
          const 200 === statusCode
          const (Right (Public.withStatus fstatus Public.LockStatusUnlocked Public.GuestLinksConfig Public.FeatureTTLUnlimited)) === responseJsonEither

  -- guest can join if guest link feature is enabled
  checkFeatureStatus Public.FeatureStatusEnabled
  getJoinCodeConv eve (conversationKey cCode) (conversationCode cCode) !!! do
    const (Right (ConversationCoverView convId (Just convName))) === responseJsonEither
    const 200 === statusCode
  postConvCodeCheck cCode !!! const 200 === statusCode
  postJoinCodeConv eve cCode !!! const 200 === statusCode
  -- non-team-members can join as well
  postJoinCodeConv bob cCode !!! const 200 === statusCode

  -- disabled guest links feature
  let disabled = Public.WithStatusNoLock Public.FeatureStatusDisabled Public.GuestLinksConfig Public.FeatureTTLUnlimited
  TeamFeatures.putTeamFeatureFlagWithGalley @Public.GuestLinksConfig galley owner teamId disabled !!! do
    const 200 === statusCode

  -- guest can't join if guest link feature is disabled
  eve' <- ephemeralUser
  bob' <- randomUser
  getJoinCodeConv eve' (conversationKey cCode) (conversationCode cCode) !!! do
    const 409 === statusCode
  postConvCodeCheck cCode !!! const 404 === statusCode
  postJoinCodeConv eve' cCode !!! const 409 === statusCode
  -- non-team-members can't join either
  postJoinCodeConv bob' cCode !!! const 409 === statusCode
  -- team members can't join either
  postJoinCodeConv alice cCode !!! const 409 === statusCode
  -- check feature status is still disabled
  checkFeatureStatus Public.FeatureStatusDisabled

  -- after re-enabling, the old link is still valid
  let enabled = Public.WithStatusNoLock Public.FeatureStatusEnabled Public.GuestLinksConfig Public.FeatureTTLUnlimited
  TeamFeatures.putTeamFeatureFlagWithGalley @Public.GuestLinksConfig galley owner teamId enabled !!! do
    const 200 === statusCode
  getJoinCodeConv eve' (conversationKey cCode) (conversationCode cCode) !!! do
    const (Right (ConversationCoverView convId (Just convName))) === responseJsonEither
    const 200 === statusCode
  postConvCodeCheck cCode !!! const 200 === statusCode
  postJoinCodeConv eve' cCode !!! const 200 === statusCode
  postJoinCodeConv bob' cCode !!! const 200 === statusCode
  checkFeatureStatus Public.FeatureStatusEnabled

-- @END

testJoinNonTeamConvGuestLinksDisabled :: TestM ()
testJoinNonTeamConvGuestLinksDisabled = do
  galley <- viewGalley
  let convName = "testConversation"
  (owner, teamId, []) <- Util.createBindingTeamWithNMembers 0
  userNotInTeam <- randomUser
  Right accessRoles <- liftIO $ genAccessRolesV2 [NonTeamMemberAccessRole] [GuestAccessRole]
  convId <- decodeConvId <$> postConv owner [] (Just convName) [CodeAccess] (Just accessRoles) Nothing
  cCode <- decodeConvCodeEvent <$> postConvCode owner convId

  -- works by default
  getJoinCodeConv userNotInTeam (conversationKey cCode) (conversationCode cCode) !!! do
    const (Right (ConversationCoverView convId (Just convName))) === responseJsonEither
    const 200 === statusCode

  -- for non-team conversations it still works if status is disabled for the team but not server wide
  let tfStatus = Public.WithStatusNoLock Public.FeatureStatusDisabled Public.GuestLinksConfig Public.FeatureTTLUnlimited
  TeamFeatures.putTeamFeatureFlagWithGalley @Public.GuestLinksConfig galley owner teamId tfStatus !!! do
    const 200 === statusCode

  getJoinCodeConv userNotInTeam (conversationKey cCode) (conversationCode cCode) !!! do
    const (Right (ConversationCoverView convId (Just convName))) === responseJsonEither
    const 200 === statusCode

-- @SF.Separation @TSFI.RESTfulAPI @S2
-- This test case covers a negative check that if access code of a guest link is revoked no further
-- people can join the group conversation. Additionally it covers:
-- Random users can use invite link
-- Reusing previously used link yields same conv (idempotency)
-- Guest can use invite link
-- Guest cannot create invite link
-- Non-admin cannot create invite link
postJoinCodeConvOk :: TestM ()
postJoinCodeConvOk = do
  c <- view tsCannon
  alice <- randomUser
  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob
  eve <- ephemeralUser
  dave <- ephemeralUser
  Right accessRoles <- liftIO $ genAccessRolesV2 [TeamMemberAccessRole, NonTeamMemberAccessRole] [GuestAccessRole]
  conv <- decodeConvId <$> postConv alice [] (Just "gossip") [CodeAccess] (Just accessRoles) Nothing
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
    -- non-admin cannot create invite link
    postConvCode bob conv !!! const 403 === statusCode
    -- test no-op
    postJoinCodeConv bob payload !!! const 204 === statusCode
    -- eve cannot join
    postJoinCodeConv eve payload !!! const 403 === statusCode
    void . liftIO $
      WS.assertMatchN (5 # Second) [wsA, wsB] $
        wsAssertMemberJoinWithRole qconv qbob [qbob] roleNameWireMember
    -- changing access to non-activated should give eve access
    Right accessRolesWithGuests <- liftIO $ genAccessRolesV2 [TeamMemberAccessRole, NonTeamMemberAccessRole, GuestAccessRole] []
    let nonActivatedAccess = ConversationAccessData (Set.singleton CodeAccess) accessRolesWithGuests
    putQualifiedAccessUpdate alice qconv nonActivatedAccess !!! const 200 === statusCode
    postJoinCodeConv eve payload !!! const 200 === statusCode
    -- guest cannot create invite link
    postConvCode eve conv !!! const 403 === statusCode
    -- after removing CodeAccess, no further people can join
    let noCodeAccess = ConversationAccessData (Set.singleton InviteAccess) accessRoles
    putQualifiedAccessUpdate alice qconv noCodeAccess !!! const 200 === statusCode
    postJoinCodeConv dave payload !!! const 404 === statusCode

-- @END

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
  -- cannot change to (Set.fromList [TeamMemberAccessRole]) as not a team conversation
  let teamAccess = ConversationAccessData (Set.singleton InviteAccess) (Set.fromList [TeamMemberAccessRole])
  putQualifiedAccessUpdate alice qconv teamAccess !!! const 403 === statusCode
  -- change access
  WS.bracketR c alice $ \wsA -> do
    let nonActivatedAccess =
          ConversationAccessData
            (Set.fromList [InviteAccess, CodeAccess])
            (Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole, GuestAccessRole, ServiceAccessRole])
    putQualifiedAccessUpdate alice qconv nonActivatedAccess !!! const 200 === statusCode
    -- test no-op
    putQualifiedAccessUpdate alice qconv nonActivatedAccess !!! const 204 === statusCode
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
  let noCodeAccess =
        ConversationAccessData
          (Set.singleton InviteAccess)
          (Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole, GuestAccessRole, ServiceAccessRole])
  putQualifiedAccessUpdate alice qconv noCodeAccess !!! const 200 === statusCode
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
  (eve, qeve) <- randomUserTuple
  connectUsers alice (singleton eve)
  let acc = Just $ Set.fromList [InviteAccess, CodeAccess]
  -- creating a team-only conversation containing eve should fail
  createTeamConvAccessRaw alice tid [bob, eve] (Just "blaa") acc (Just (Set.fromList [TeamMemberAccessRole])) Nothing Nothing
    !!! const 403 === statusCode
  Right accessRoles <- liftIO $ genAccessRolesV2 [TeamMemberAccessRole, NonTeamMemberAccessRole, GuestAccessRole] []
  -- create conversation allowing any type of guest
  conv <- createTeamConvAccess alice tid [bob, eve] (Just "blaa") acc (Just accessRoles) Nothing Nothing
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
    let teamAccess =
          ConversationAccessData
            (Set.fromList [InviteAccess, CodeAccess])
            (Set.fromList [TeamMemberAccessRole])
    putQualifiedAccessUpdate alice qconv teamAccess !!! const 200 === statusCode
    void . liftIO $
      WS.assertMatchN (5 # Second) [wsA, wsB, wsE, wsM] $
        wsAssertConvAccessUpdate qconv qalice teamAccess
    -- non-team members get kicked out
    liftIO $ do
      WS.assertMatchN_ (5 # Second) [wsA, wsB, wsE, wsM] $
        wsAssertMemberLeave qconv qalice (pure qeve)
      WS.assertMatchN_ (5 # Second) [wsA, wsB, wsE, wsM] $
        wsAssertMemberLeave qconv qalice (pure qmallory)
    -- joining (for mallory) is no longer possible
    postJoinCodeConv mallory j !!! const 403 === statusCode
    -- team members (dave) can still join
    postJoinCodeConv dave j !!! const 200 === statusCode

-- @SF.Federation @SF.Separation @TSFI.RESTfulAPI @S2
--
-- The test asserts that, among others, remote users are removed from a
-- conversation when an access update occurs that disallows guests from
-- accessing.
testAccessUpdateGuestRemoved :: TestM ()
testAccessUpdateGuestRemoved = do
  -- alice, bob are in a team
  (tid, alice, [bob]) <- createBindingTeamWithQualifiedMembers 2

  -- charlie is a local guest
  charlie <- randomQualifiedUser
  connectUsers (qUnqualified alice) (pure (qUnqualified charlie))

  -- dee is a remote guest
  let remoteDomain = Domain "far-away.example.com"
  dee <- Qualified <$> randomId <*> pure remoteDomain

  connectWithRemoteUser (qUnqualified alice) dee

  -- they are all in a local conversation
  conv <-
    responseJsonError
      =<< postConvWithRemoteUsers
        (qUnqualified alice)
        defNewProteusConv
          { newConvQualifiedUsers = [bob, charlie, dee],
            newConvTeam = Just (ConvTeamInfo tid)
          }
        <!! const 201 === statusCode

  c <- view tsCannon
  WS.bracketRN c (map qUnqualified [alice, bob, charlie]) $ \[wsA, wsB, wsC] -> do
    -- conversation access role changes to team only
    (_, reqs) <- withTempMockFederator' (mockReply ()) $ do
      putQualifiedAccessUpdate
        (qUnqualified alice)
        (cnvQualifiedId conv)
        (ConversationAccessData mempty (Set.fromList [TeamMemberAccessRole]))
        !!! const 200 === statusCode

      -- charlie and dee are kicked out
      --
      -- note that removing users happens asynchronously, so this check should
      -- happen while the mock federator is still available
      WS.assertMatchN_ (5 # Second) [wsA, wsB, wsC] $
        wsAssertMembersLeave (cnvQualifiedId conv) alice [charlie]
      WS.assertMatchN_ (5 # Second) [wsA, wsB, wsC] $
        wsAssertMembersLeave (cnvQualifiedId conv) alice [dee]

    -- dee's remote receives a notification
    let compareLists [] ys = [] @?= ys
        compareLists (x : xs) ys = case break (== x) ys of
          (ys1, _ : ys2) -> compareLists xs (ys1 <> ys2)
          _ -> assertFailure $ "Could not find " <> show x <> " in " <> show ys
    liftIO $
      compareLists
        ( map
            ( \fr -> do
                cu <- eitherDecode (frBody fr)
                pure (F.cuOrigUserId cu, F.cuAction cu)
            )
            ( filter
                ( \fr ->
                    frComponent fr == Galley
                      && frRPC fr == "on-conversation-updated"
                )
                reqs
            )
        )
        [ Right (alice, SomeConversationAction (sing @'ConversationRemoveMembersTag) (pure charlie)),
          Right (alice, SomeConversationAction (sing @'ConversationRemoveMembersTag) (pure dee)),
          Right
            ( alice,
              SomeConversationAction
                (sing @'ConversationAccessDataTag)
                ConversationAccessData
                  { cupAccess = mempty,
                    cupAccessRoles = Set.fromList [TeamMemberAccessRole]
                  }
            )
        ]

  -- only alice and bob remain
  conv2 <-
    responseJsonError
      =<< getConvQualified (qUnqualified alice) (cnvQualifiedId conv)
        <!! const 200 === statusCode
  liftIO $ map omQualifiedId (cmOthers (cnvMembers conv2)) @?= [bob]

-- @END

testTeamMemberCantJoinViaGuestLinkIfAccessRoleRemoved :: TestM ()
testTeamMemberCantJoinViaGuestLinkIfAccessRoleRemoved = do
  -- given alice, bob, charlie and dee are in a team
  (alice, tid, [bob, charlie, dee]) <- createBindingTeamWithNMembers 3

  -- and given alice and bob are in a team conversation and alice created a guest link
  let accessRoles = Set.fromList [TeamMemberAccessRole, GuestAccessRole, ServiceAccessRole]
  qconvId <- decodeQualifiedConvId <$> postTeamConv tid alice [bob] (Just "chit chat") [CodeAccess] (Just accessRoles) Nothing
  cCode <- decodeConvCodeEvent <$> postConvCode alice (qUnqualified qconvId)

  -- then charlie can join via the guest link
  postJoinCodeConv charlie cCode !!! const 200 === statusCode

  -- when the guests are disabled for the conversation
  let accessData = ConversationAccessData (Set.singleton InviteAccess) (Set.fromList [TeamMemberAccessRole, ServiceAccessRole])
  putQualifiedAccessUpdate alice qconvId accessData !!! const 200 === statusCode

  -- then dee cannot join via guest link
  postJoinCodeConv dee cCode !!! const 404 === statusCode

getGuestLinksStatusFromForeignTeamConv :: TestM ()
getGuestLinksStatusFromForeignTeamConv = do
  localDomain <- viewFederationDomain
  galley <- viewGalley
  let setTeamStatus u tid tfStatus =
        TeamFeatures.putTeamFeatureFlagWithGalley @Public.GuestLinksConfig galley u tid (Public.WithStatusNoLock tfStatus Public.GuestLinksConfig Public.FeatureTTLUnlimited) !!! do
          const 200 === statusCode
  let checkGuestLinksStatus u c s =
        getGuestLinkStatus galley u c !!! do
          const 200 === statusCode
          const s === (Public.wsStatus . (responseJsonUnsafe @(Public.WithStatus Public.GuestLinksConfig)))
  let checkGetGuestLinksStatus s u c =
        getGuestLinkStatus galley u c !!! do
          const s === statusCode

  -- given alice is in team A with guest links allowed
  (alice, teamA, [alex]) <- createBindingTeamWithNMembers 1
  let qalice = Qualified alice localDomain
  setTeamStatus alice teamA Public.FeatureStatusEnabled

  -- and given bob is in team B with guest links disallowed
  (bob, teamB, [bert]) <- createBindingTeamWithNMembers 1
  let qbert = Qualified bert localDomain
  setTeamStatus bob teamB Public.FeatureStatusDisabled

  -- and given alice and bob are connected
  connectUsers alice (singleton bob)

  -- and given bob creates a conversation, invites alice, and makes her group admin
  let accessRoles = Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole]
  conv <- decodeConvId <$> postTeamConv teamB bob [] (Just "teams b's conversation") [InviteAccess] (Just accessRoles) Nothing
  let qconv = Qualified conv localDomain
  postMembersWithRole bob (pure qalice) qconv roleNameWireAdmin !!! const 200 === statusCode

  -- when alice gets the guest link status for the conversation
  -- then the status should be disabled
  checkGuestLinksStatus alice conv Public.FeatureStatusDisabled

  -- when bob gets the guest link status for the conversation
  -- then the status should be disabled
  checkGuestLinksStatus bob conv Public.FeatureStatusDisabled

  -- when bob enables guest links for his team and gets the guest link status for the conversation
  setTeamStatus bob teamB Public.FeatureStatusEnabled

  -- then the status should be enabled
  checkGuestLinksStatus bob conv Public.FeatureStatusEnabled

  -- when alice gets the guest link status for the conversation
  -- then the status should be enabled
  checkGuestLinksStatus alice conv Public.FeatureStatusEnabled

  -- when alice disables guest links for her team and gets the guest link status for the conversation
  setTeamStatus alice teamA Public.FeatureStatusDisabled

  -- then the guest link status for the conversation should still be enabled (note that in the UI she can't create guest links because her own team settings do not allow this)
  checkGuestLinksStatus alice conv Public.FeatureStatusEnabled

  -- when bob gets the guest link status for the conversation
  -- then the status should be enabled
  checkGuestLinksStatus bob conv Public.FeatureStatusEnabled

  -- when a user that is not in the conversation tries to get the guest link status
  -- then the result should be not found
  checkGetGuestLinksStatus 404 alex conv
  checkGetGuestLinksStatus 404 bert conv

  -- when a conversation member that is not an admin tries to get the guest link status
  -- then the result should be forbidden
  postMembersWithRole bob (pure qbert) qconv roleNameWireMember !!! const 200 === statusCode
  checkGetGuestLinksStatus 403 bert conv

postJoinConvFail :: TestM ()
postJoinConvFail = do
  alice <- randomUser
  bob <- randomUser
  conv <- decodeConvId <$> postConv alice [] (Just "gossip") [] Nothing Nothing
  void $ postJoinConv bob conv !!! const 403 === statusCode

getConvsOk :: TestM ()
getConvsOk = do
  usr <- randomUser
  convs <- getAllConvs usr
  liftIO $
    [selfConv usr, mlsSelfConvId usr]
      @?= map (qUnqualified . cnvQualifiedId) convs

getConvsOk2 :: TestM ()
getConvsOk2 = do
  [alice, bob] <- randomUsers 2
  connectUsers alice (singleton bob)
  -- create & get one2one conv
  cnv1 <- responseJsonError =<< postO2OConv alice bob (Just "gossip1") <!! const 200 === statusCode
  do
    r <-
      responseJsonError
        =<< getConvs alice [cnvQualifiedId cnv1] <!! do
          const 200 === statusCode
    liftIO $
      [cnvQualifiedId cnv1] @=? map cnvQualifiedId (crFound r)
  -- create & get group conv
  carl <- randomUser
  connectUsers alice (singleton carl)
  cnv2 <-
    responseJsonError
      =<< postConv alice [bob, carl] (Just "gossip2") [] Nothing Nothing
        <!! const 201 === statusCode
  do
    r <-
      responseJsonError
        =<< getConvs alice [cnvQualifiedId cnv2] <!! do
          const 200 === statusCode
    liftIO $
      [cnvQualifiedId cnv2] @=? map cnvQualifiedId (crFound r)
  -- get both
  convs <- getAllConvs alice
  let c1 = find ((== cnvQualifiedId cnv1) . cnvQualifiedId) convs
  let c2 = find ((== cnvQualifiedId cnv2) . cnvQualifiedId) convs
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

getConvsFailMaxSizeV2 :: TestM ()
getConvsFailMaxSizeV2 = do
  usr <- randomUser
  g <- view tsUnversionedGalley
  get
    ( g
        . path "/v2/conversations"
        . zUser usr
        . zConn "conn"
        . queryItem "size" (toByteString' (501 :: Int32))
    )
    !!! const 400 === statusCode

testGetConvIdsV2 :: TestM ()
testGetConvIdsV2 = do
  [alice, bob] <- randomUsers 2
  connectUsers alice (singleton bob)
  void $ postO2OConv alice bob (Just "gossip")
  getConvIdsV2 alice Nothing Nothing !!! do
    const 200 === statusCode
    const 2 === length . decodeConvIdList
  getConvIdsV2 bob Nothing Nothing !!! do
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
      resp <- getConvIdsV2 alice start (Just size) <!! const 200 === statusCode
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

      pure (Just (Right (last (convList c))))

getConvIdsFailMaxSizeV2 :: TestM ()
getConvIdsFailMaxSizeV2 = do
  usr <- randomUser
  getConvIdsV2 usr Nothing (Just 1001)
    !!! const 400 === statusCode

getConvIdsFailMaxSize :: TestM ()
getConvIdsFailMaxSize = do
  usr <- randomUser
  getConvPage usr Nothing (Just 1001)
    !!! const 400 === statusCode

testListConvIds :: TestM ()
testListConvIds = do
  [alice, bob] <- randomUsers 2
  connectUsers alice (singleton bob)
  void $ postO2OConv alice bob (Just "gossip")
  -- Each user has a Proteus self-conversation and the one-to-one coversation.
  for_ [alice, bob] $ \u -> do
    r :: ConversationList ConvId <-
      responseJsonError
        =<< getConvIdsV2 u Nothing (Just 5)
          <!! const 200 === statusCode
    liftIO $ do
      length (convList r) @?= 2
      convHasMore r @?= False

paginateConvListIds :: TestM ()
paginateConvListIds = do
  [alice, bob, eve] <- randomUsers 3
  connectUsers alice (list1 bob [eve])
  localDomain <- viewFederationDomain
  let qAlice = Qualified alice localDomain
  now <- liftIO getCurrentTime
  fedGalleyClient <- view tsFedGalleyClient

  replicateM_ 196 $
    postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
      !!! const 201 === statusCode

  remoteChad <- randomId
  let chadDomain = Domain "chad.example.com"
      qChad = Qualified remoteChad chadDomain
  connectWithRemoteUser alice qChad
  replicateM_ 25 $ do
    conv <- randomId
    let cu =
          F.ConversationUpdate
            { F.cuTime = now,
              F.cuOrigUserId = qChad,
              F.cuConvId = conv,
              F.cuAlreadyPresentUsers = [],
              F.cuAction = SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qAlice) roleNameWireMember)
            }
    runFedClient @"on-conversation-updated" fedGalleyClient chadDomain cu

  remoteDee <- randomId
  let deeDomain = Domain "dee.example.com"
      qDee = Qualified remoteDee deeDomain
  connectWithRemoteUser alice qDee
  replicateM_ 31 $ do
    conv <- randomId
    let cu =
          F.ConversationUpdate
            { F.cuTime = now,
              F.cuOrigUserId = qDee,
              F.cuConvId = conv,
              F.cuAlreadyPresentUsers = [],
              F.cuAction = SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qAlice) roleNameWireMember)
            }
    runFedClient @"on-conversation-updated" fedGalleyClient deeDomain cu

  -- 1 Proteus self conv + 1 MLS self conv + 2 convs with bob and eve + 196
  -- local convs + 25 convs on chad.example.com + 31 on dee.example = 256 convs.
  -- Getting them 16 at a time should get all them in 16 times.
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

  -- With page size 16, 28 group convs + 2 one-to-one convs + 1 Proteus self
  -- conv + 1 MLS self conv, we get 32 convs. The 2nd page should end here.
  replicateM_ 28 $
    postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
      !!! const 201 === statusCode

  -- We should be able to page through current state in 2 pages exactly
  foldM_ (getChunkedConvs 16 0 alice) Nothing [2, 1, 0 :: Int]

  remoteChad <- randomId
  let chadDomain = Domain "chad.example.com"
      qChad = Qualified remoteChad chadDomain
  connectWithRemoteUser alice qChad

  -- The 3rd page will end with this domain
  replicateM_ 16 $ do
    conv <- randomId
    let cu =
          F.ConversationUpdate
            { F.cuTime = now,
              F.cuOrigUserId = qChad,
              F.cuConvId = conv,
              F.cuAlreadyPresentUsers = [],
              F.cuAction = SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qAlice) roleNameWireMember)
            }
    runFedClient @"on-conversation-updated" fedGalleyClient chadDomain cu

  remoteDee <- randomId
  let deeDomain = Domain "dee.example.com"
      qDee = Qualified remoteDee deeDomain
  connectWithRemoteUser alice qDee

  -- The 4th and last page will end with this domain
  replicateM_ 16 $ do
    conv <- randomId
    let cu =
          F.ConversationUpdate
            { F.cuTime = now,
              F.cuOrigUserId = qDee,
              F.cuConvId = conv,
              F.cuAlreadyPresentUsers = [],
              F.cuAction = SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qAlice) roleNameWireMember)
            }
    runFedClient @"on-conversation-updated" fedGalleyClient deeDomain cu

  foldM_ (getChunkedConvs 16 0 alice) Nothing [4, 3, 2, 1, 0 :: Int]

-- | Gets chunked conversation ids given size of each chunk, size of the last
-- chunk, requesting user and @n@ which represents how many chunks are remaining
-- to go, when this is 0, it is assumed that this chunk is last and the response
-- must set @has_more@ to 'False' and the number of conv ids returned should
-- match @lastSize@.
getChunkedConvs :: HasCallStack => Int32 -> Int -> UserId -> Maybe ConversationPagingState -> Int -> TestM (Maybe ConversationPagingState)
getChunkedConvs size lastSize alice pagingState n = do
  resp <- getConvPage alice pagingState (Just size) <!! const 200 === statusCode
  let c = responseJsonUnsafeWithMsg @ConvIdsPage "failed to parse ConvIdsPage" resp
  liftIO $ do
    if n > 0
      then assertEqual ("Number of convs should match the requested size, " <> show n <> " more chunks to go") (fromIntegral size) (length (mtpResults c))
      else assertEqual "Number of convs should match the last size, no more chunks to go" lastSize (length (mtpResults c))

    if n > 0
      then assertEqual ("hasMore should be True, " <> show n <> " more chunk(s) to go") True (mtpHasMore c)
      else assertEqual "hasMore should be False, no more chunks to go" False (mtpHasMore c)

  pure . Just $ mtpPagingState c

getConvsPagingOk :: TestM ()
getConvsPagingOk = do
  [ally, bill, carl] <- randomUsers 3
  connectUsers ally (list1 bill [carl])
  replicateM_ 10 $ postConv ally [bill, carl] (Just "gossip") [] Nothing Nothing

  walk ally [3, 3, 3, 3, 2] -- 10 (group) + 2 (1:1) + 2 (self)
  walk bill [3, 3, 3, 3, 1] -- 10 (group) + 1 (1:1) + 2 (self)
  walk carl [3, 3, 3, 3, 1] -- 10 (group) + 1 (1:1) + 2 (self)
  where
    walk :: Foldable t => UserId -> t Int -> TestM ()
    walk u = foldM_ (next u 3) Nothing

    next ::
      UserId ->
      Int32 ->
      Maybe ConversationPagingState ->
      Int ->
      TestM (Maybe ConversationPagingState)
    next u step state n = do
      (ids1, state') <- do
        r :: ConvIdsPage <-
          responseJsonError
            =<< getConvPage u state (Just step)
              <!! const 200 === statusCode
        pure (mtpResults r, mtpPagingState r)
      liftIO $ assertEqual "unexpected length (getConvIds)" n (length ids1)

      ids2 <- do
        r <-
          responseJsonError
            =<< getConvs u ids1 <!! const 200 === statusCode
        pure $ map cnvQualifiedId (crFound r)
      liftIO $ assertEqual "unexpected length (getConvs)" n (length ids2)
      liftIO $ assertBool "getConvIds /= getConvs" (ids1 == ids2)

      pure (Just state')

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
  postConvQualified alice defNewProteusConv {newConvQualifiedUsers = [bob, jane]} !!! do
    const 403 === statusCode
    const (Just "not-connected") === fmap label . responseJsonUnsafe

postConvLimitOk :: TestM ()
postConvLimitOk = do
  n <- fromIntegral . pred <$> view tsMaxConvSize
  alice <- randomUser
  bob : others <- replicateM n randomUser
  connectUsers alice (list1 bob others)
  postConv alice (bob : others) Nothing [] Nothing Nothing !!! do
    const 201 === statusCode

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
  postConvQualified alice defNewProteusConv {newConvQualifiedUsers = bob : others} !!! do
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
  postConvQualified alice defNewProteusConv {newConvQualifiedUsers = [bob, jane]} !!! do
    const 403 === statusCode
    const (Just "not-connected") === fmap label . responseJsonUnsafe

postConvQualifiedNoConnection :: TestM ()
postConvQualifiedNoConnection = do
  alice <- randomUser
  bob <- flip Qualified (Domain "far-away.example.com") <$> randomId
  postConvQualified alice defNewProteusConv {newConvQualifiedUsers = [bob]}
    !!! const 403 === statusCode

postTeamConvQualifiedNoConnection :: TestM ()
postTeamConvQualifiedNoConnection = do
  (tid, alice, _) <- createBindingTeamWithQualifiedMembers 1
  bob <- randomQualifiedId (Domain "bob.example.com")
  charlie <- randomQualifiedUser
  postConvQualified
    (qUnqualified alice)
    defNewProteusConv
      { newConvQualifiedUsers = [bob],
        newConvTeam = Just (ConvTeamInfo tid)
      }
    !!! const 403 === statusCode
  postConvQualified
    (qUnqualified alice)
    defNewProteusConv
      { newConvQualifiedUsers = [charlie],
        newConvTeam = Just (ConvTeamInfo tid)
      }
    !!! const 403 === statusCode

postConvQualifiedNonExistentDomain :: TestM ()
postConvQualifiedNonExistentDomain = do
  alice <- randomUser
  bob <- flip Qualified (Domain "non-existent.example.com") <$> randomId
  connectWithRemoteUser alice bob
  postConvQualified
    alice
    defNewProteusConv {newConvQualifiedUsers = [bob]}
    !!! do
      const 422 === statusCode

postConvQualifiedFederationNotEnabled :: TestM ()
postConvQualifiedFederationNotEnabled = do
  alice <- randomUser
  bob <- flip Qualified (Domain "some-remote-backend.example.com") <$> randomId
  connectWithRemoteUser alice bob
  let federatorNotConfigured = optFederator .~ Nothing
  withSettingsOverrides federatorNotConfigured $ do
    g <- viewGalley
    postConvHelper g alice [bob] !!! do
      const 400 === statusCode
      const (Just "federation-not-enabled") === fmap label . responseJsonUnsafe

-- like postConvQualified
-- FUTUREWORK: figure out how to use functions in the TestM monad inside withSettingsOverrides and remove this duplication
postConvHelper :: (MonadIO m, MonadHttp m) => (Request -> Request) -> UserId -> [Qualified UserId] -> m ResponseLBS
postConvHelper g zusr newUsers = do
  let conv = NewConv [] newUsers (checked "gossip") (Set.fromList []) Nothing Nothing Nothing Nothing roleNameWireAdmin ProtocolProteusTag Nothing
  post $ g . path "/conversations" . zUser zusr . zConn "conn" . zType "access" . json conv

postSelfConvOk :: TestM ()
postSelfConvOk = do
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice
  m <- postSelfConv alice <!! const 200 === statusCode
  n <- postSelfConv alice <!! const 200 === statusCode
  mId <- assertConv m SelfConv alice qalice [] Nothing Nothing
  nId <- assertConv n SelfConv alice qalice [] Nothing Nothing
  liftIO $ mId @=? nId

postO2OConvOk :: TestM ()
postO2OConvOk = do
  (alice, qalice) <- randomUserTuple
  (bob, qbob) <- randomUserTuple
  connectUsers alice (singleton bob)
  a <- postO2OConv alice bob Nothing <!! const 200 === statusCode
  c <- postO2OConv alice bob Nothing <!! const 200 === statusCode
  aId <- assertConv a One2OneConv alice qalice [qbob] Nothing Nothing
  cId <- assertConv c One2OneConv alice qalice [qbob] Nothing Nothing
  liftIO $ aId @=? cId

postConvO2OFailWithSelf :: TestM ()
postConvO2OFailWithSelf = do
  g <- viewGalley
  alice <- randomUser
  let inv = NewConv [alice] [] Nothing mempty Nothing Nothing Nothing Nothing roleNameWireAdmin ProtocolProteusTag Nothing
  post (g . path "/conversations/one2one" . zUser alice . zConn "conn" . zType "access" . json inv) !!! do
    const 403 === statusCode
    const (Just "invalid-op") === fmap label . responseJsonUnsafe

postConnectConvOk :: TestM ()
postConnectConvOk = do
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice
  bob <- randomUser
  m <-
    postConnectConv alice bob "Alice" "connect with me!" Nothing
      <!! const 201 === statusCode
  n <-
    postConnectConv alice bob "Alice" "connect with me!" Nothing
      <!! const 200 === statusCode
  mId <- assertConv m ConnectConv alice qalice [] (Just "Alice") Nothing
  nId <- assertConv n ConnectConv alice qalice [] (Just "Alice") Nothing
  liftIO $ mId @=? nId

postConnectConvOk2 :: TestM ()
postConnectConvOk2 = do
  alice <- randomUser
  bob <- randomUser
  m <- decodeConvId <$> req alice bob
  n <- decodeConvId <$> req alice bob
  liftIO $ m @=? n
  where
    req alice bob =
      postConnectConv alice bob "Alice" "connect with me!" (Just "me@me.com")

putConvAcceptOk :: TestM ()
putConvAcceptOk = do
  alice <- randomUser
  bob <- randomUser
  qcnv <- decodeQualifiedConvId <$> postConnectConv alice bob "Alice" "come to zeta!" Nothing
  putConvAccept bob (qUnqualified qcnv) !!! const 200 === statusCode
  getConvQualified alice qcnv !!! do
    const 200 === statusCode
    const (Just One2OneConv) === fmap cnvType . responseJsonUnsafe
  getConvQualified bob qcnv !!! do
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
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice
  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob
  ac <-
    postConnectConv alice bob "A" "a" Nothing
      <!! const 201 === statusCode
  acId <- assertConv ac ConnectConv alice qalice [] (Just "A") Nothing
  bc <-
    postConnectConv bob alice "B" "b" Nothing
      <!! const 200 === statusCode
  -- The connect conversation was simply accepted, thus the
  -- conversation name and message sent in Bob's request ignored.
  bcId <- assertConv bc One2OneConv alice qbob [qalice] (Just "A") Nothing
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
  let qconvId = cnvQualifiedId cnv
  let convId = qUnqualified qconvId
  putConvAccept bob convId !!! const 200 === statusCode
  cnvX <- responseJsonUnsafeWithMsg "conversation" <$> getConvQualified bob qconvId
  liftIO $ do
    ConnectConv @=? cnvType cnvX
    Just "B" @=? cnvName cnvX
    privateAccess @=? cnvAccess cnvX
  -- Alice accepts, finally turning it into a 1-1
  putConvAccept alice convId !!! const 200 === statusCode
  cnv4 <- responseJsonUnsafeWithMsg "conversation" <$> getConvQualified alice qconvId
  liftIO $ do
    One2OneConv @=? cnvType cnv4
    Just "B" @=? cnvName cnv4
    privateAccess @=? cnvAccess cnv4
  where
    cancel u c = do
      g <- viewGalley
      let cnvId = qUnqualified . cnvQualifiedId
      put (g . paths ["/i/conversations", toByteString' (cnvId c), "block"] . zUser u)
        !!! const 200 === statusCode
      getConv u (cnvId c) !!! const 403 === statusCode

putBlockConvOk :: TestM ()
putBlockConvOk = do
  g <- viewGalley
  alice <- randomUser
  bob <- randomUser
  conv <- responseJsonUnsafeWithMsg "conversation" <$> postConnectConv alice bob "Alice" "connect with me!" (Just "me@me.com")
  let qconvId = cnvQualifiedId conv
  let convId = qUnqualified qconvId
  getConvQualified alice qconvId !!! const 200 === statusCode
  getConvQualified bob qconvId !!! const 403 === statusCode
  put (g . paths ["/i/conversations", toByteString' convId, "block"] . zUser bob)
    !!! const 200 === statusCode
  -- A is still the only member of the 1-1
  getConvQualified alice qconvId !!! do
    const 200 === statusCode
    const (cnvMembers conv) === cnvMembers . responseJsonUnsafeWithMsg "conversation"
  -- B accepts the conversation by unblocking
  put (g . paths ["/i/conversations", toByteString' convId, "unblock"] . zUser bob)
    !!! const 200 === statusCode
  getConvQualified bob qconvId !!! const 200 === statusCode
  -- B blocks A in the 1-1
  put (g . paths ["/i/conversations", toByteString' convId, "block"] . zUser bob)
    !!! const 200 === statusCode
  -- B no longer sees the 1-1
  getConvQualified bob qconvId !!! const 403 === statusCode
  -- B unblocks A in the 1-1
  put (g . paths ["/i/conversations", toByteString' convId, "unblock"] . zUser bob)
    !!! const 200 === statusCode
  -- B sees the blocked 1-1 again
  getConvQualified bob qconvId !!! do
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
  conv <-
    decodeConvId
      <$> postConvQualified
        alice
        defNewProteusConv
          { newConvQualifiedUsers = [bob, chuck],
            newConvName = checked "gossip"
          }
  getConv alice conv !!! const 200 === statusCode
  getConv (qUnqualified bob) conv !!! const 200 === statusCode
  getConv (qUnqualified chuck) conv !!! const 200 === statusCode

accessConvMeta :: TestM ()
accessConvMeta = do
  g <- viewGalley
  alice <- randomUser
  bob <- randomUser
  chuck <- randomUser
  connectUsers alice (list1 bob [chuck])
  conv <- decodeConvId <$> postConv alice [bob, chuck] (Just "gossip") [] Nothing Nothing
  let meta =
        ConversationMetadata
          RegularConv
          alice
          [InviteAccess]
          (Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole, ServiceAccessRole])
          (Just "gossip")
          Nothing
          Nothing
          Nothing
  get (g . paths ["i/conversations", toByteString' conv, "meta"] . zUser alice) !!! do
    const 200 === statusCode
    const (Just meta) === (decode <=< responseBody)

leaveConnectConversation :: TestM ()
leaveConnectConversation = do
  (alice, qalice) <- randomUserTuple
  bob <- randomUser
  bdy <- postConnectConv alice bob "alice" "ni" Nothing <!! const 201 === statusCode
  let c = maybe (error "invalid connect conversation") (qUnqualified . cnvQualifiedId) (responseJsonUnsafe bdy)
  qc <- Qualified c <$> viewFederationDomain
  deleteMemberQualified alice qalice qc !!! const 403 === statusCode

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

  postQualifiedMembers alice (remoteBob :| []) convId !!! do
    const 403 === statusCode
    const (Right (Just "not-connected")) === fmap (view (at "label")) . responseJsonEither @Object

  connectWithRemoteUser alice remoteBob

  (resp, reqs) <-
    withTempMockFederator' (respond remoteBob) $
      postQualifiedMembers alice (remoteBob :| []) convId
        <!! const 200 === statusCode
  liftIO $ do
    map frTargetDomain reqs @?= [remoteDomain, remoteDomain]
    map frRPC reqs @?= ["on-new-remote-conversation", "on-conversation-updated"]

  let e = responseJsonUnsafe resp
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
    respond :: Qualified UserId -> Mock LByteString
    respond bob =
      asum
        [ guardComponent Brig *> mockReply [mkProfile bob (Name "bob")],
          "on-new-remote-conversation" ~> EmptyResponse,
          "on-conversation-updated" ~> ()
        ]

testDeleteTeamConversationWithRemoteMembers :: TestM ()
testDeleteTeamConversationWithRemoteMembers = do
  (alice, tid) <- createBindingTeam
  localDomain <- viewFederationDomain
  let qalice = Qualified alice localDomain

  bobId <- randomId
  let remoteDomain = Domain "far-away.example.com"
      remoteBob = Qualified bobId remoteDomain

  convId <- decodeConvId <$> postTeamConv tid alice [] (Just "remote gossip") [] Nothing Nothing
  let _qconvId = Qualified convId localDomain

  connectWithRemoteUser alice remoteBob

  let mock =
        ("on-new-remote-conversation" ~> EmptyResponse)
          <|> ("on-conversation-updated" ~> ())
  (_, received) <- withTempMockFederator' mock $ do
    postQualifiedMembers alice (remoteBob :| []) convId
      !!! const 200 === statusCode

    deleteTeamConv tid convId alice
      !!! const 200 === statusCode

  liftIO $ do
    let convUpdates = mapMaybe (eitherToMaybe . parseFedRequest) received
    convUpdate <- case filter ((== SomeConversationAction (sing @'ConversationDeleteTag) ()) . cuAction) convUpdates of
      [] -> assertFailure "No ConversationUpdate requests received"
      [convDelete] -> pure convDelete
      _ -> assertFailure "Multiple ConversationUpdate requests received"
    cuAlreadyPresentUsers convUpdate @?= [bobId]
    cuOrigUserId convUpdate @?= qalice

testGetQualifiedLocalConv :: TestM ()
testGetQualifiedLocalConv = do
  alice <- randomUser
  convId <- decodeQualifiedConvId <$> postConv alice [] (Just "gossip") [] Nothing Nothing
  conv :: Conversation <- fmap responseJsonUnsafe $ getConvQualified alice convId <!! const 200 === statusCode
  liftIO $ do
    assertEqual "conversation id" convId (cnvQualifiedId conv)
    assertEqual "conversation name" (Just "gossip") (cnvName conv)

testGetQualifiedLocalConvNotFound :: TestM ()
testGetQualifiedLocalConvNotFound = do
  alice <- randomUser
  localDomain <- viewFederationDomain
  convId <- (`Qualified` localDomain) <$> randomId
  getConvQualified alice convId !!! do
    const 404 === statusCode
    const (Right (Just "no-conversation")) === fmap (view (at "label")) . responseJsonEither @Object

testGetQualifiedLocalConvNotParticipating :: TestM ()
testGetQualifiedLocalConvNotParticipating = do
  alice <- randomUser
  bob <- randomUser
  convId <- decodeQualifiedConvId <$> postConv bob [] (Just "gossip about alice") [] Nothing Nothing
  getConvQualified alice convId !!! do
    const 403 === statusCode
    const (Just "access-denied") === view (at "label") . responseJsonUnsafe @Object

testGetQualifiedRemoteConv :: TestM ()
testGetQualifiedRemoteConv = do
  aliceQ <- randomQualifiedUser
  let aliceId = qUnqualified aliceQ
  loc <- flip toLocalUnsafe () <$> viewFederationDomain
  bobId <- randomId
  convId <- randomId
  let remoteDomain = Domain "far-away.example.com"
      bobQ = Qualified bobId remoteDomain
      remoteConvId = Qualified convId remoteDomain
      bobAsOtherMember = OtherMember bobQ Nothing roleNameWireAdmin
      aliceAsLocal =
        LocalMember aliceId defMemberStatus Nothing roleNameWireAdmin
      aliceAsOtherMember = localMemberToOther (qDomain aliceQ) aliceAsLocal
      aliceAsSelfMember = localMemberToSelf loc aliceAsLocal

  connectWithRemoteUser aliceId bobQ
  registerRemoteConv remoteConvId bobId Nothing (Set.fromList [aliceAsOtherMember])

  let mockConversation = mkProteusConv convId bobId roleNameWireAdmin [bobAsOtherMember]
      remoteConversationResponse = GetConversationsResponse [mockConversation]
      expected =
        Conversation
          remoteConvId
          (rcnvMetadata mockConversation)
          (ConvMembers aliceAsSelfMember (rcmOthers (rcnvMembers mockConversation)))
          ProtocolProteus

  (respAll, _) <-
    withTempMockFederator'
      (mockReply remoteConversationResponse)
      (getConvQualified aliceId remoteConvId)

  conv <- responseJsonUnsafe <$> (pure respAll <!! const 200 === statusCode)
  liftIO $ do assertEqual "conversation metadata" expected conv

testGetQualifiedRemoteConvNotFound :: TestM ()
testGetQualifiedRemoteConvNotFound = do
  aliceId <- randomUser
  let remoteDomain = Domain "far-away.example.com"
  remoteConvId <- (`Qualified` remoteDomain) <$> randomId
  -- No need to mock federator as we don't expect a call to be made
  getConvQualified aliceId remoteConvId !!! do
    const 404 === statusCode
    const (Just "no-conversation") === view (at "label") . responseJsonUnsafe @Object

testGetQualifiedRemoteConvNotFoundOnRemote :: TestM ()
testGetQualifiedRemoteConvNotFoundOnRemote = do
  aliceQ <- randomQualifiedUser
  let aliceId = qUnqualified aliceQ
  bobId <- randomId
  convId <- randomId
  let remoteDomain = Domain "far-away.example.com"
      remoteConvId = Qualified convId remoteDomain
      aliceAsOtherMember = OtherMember aliceQ Nothing roleNameWireAdmin

  registerRemoteConv remoteConvId bobId Nothing (Set.fromList [aliceAsOtherMember])

  void . withTempMockFederator' (mockReply (GetConversationsResponse [])) $ do
    getConvQualified aliceId remoteConvId !!! do
      const 404 === statusCode
      const (Just "no-conversation") === view (at "label") . responseJsonUnsafe @Object

-- | Tests getting many converations given their ids.
--
-- In this test, Alice is a local user, who will be asking for metadata of these
-- conversations:
--
-- - A local conversation which she is part of
--
-- - A remote conv on a.far-away.example.com (with Bob)
--
-- - A remote conv on b.far-away.example.com (with Carl)
--
-- - A remote conv on a.far-away.example.com, which is not found in the local DB
--
-- - A remote conv on b.far-away.example.com, it is found in the local DB but
--   the remote does not return it
--
-- - A remote conv on c.far-away.example.com (with Dee), for which the federated call fails
--
-- - A local conversation which doesn't exist
--
-- - A local conversation which they're not part of
testBulkGetQualifiedConvs :: TestM ()
testBulkGetQualifiedConvs = do
  localDomain <- viewFederationDomain
  aliceQ <- randomQualifiedUser
  let alice = qUnqualified aliceQ
      lAlice = toLocalUnsafe localDomain alice
  bobId <- randomId
  carlId <- randomId
  deeId <- randomId
  let remoteDomainA = Domain "a.far-away.example.com"
      remoteDomainB = Domain "b.far-away.example.com"
      remoteDomainC = Domain "c.far-away.example.com"
      bobQ = Qualified bobId remoteDomainA
      carlQ = Qualified carlId remoteDomainB
      deeQ = Qualified deeId remoteDomainC

  connectWithRemoteUser alice bobQ
  connectWithRemoteUser alice carlQ
  connectWithRemoteUser alice deeQ

  localConv <- responseJsonUnsafe <$> postConv alice [] (Just "gossip") [] Nothing Nothing
  let localConvId = cnvQualifiedId localConv

  remoteConvIdA <- randomQualifiedId remoteDomainA
  remoteConvIdB <- randomQualifiedId remoteDomainB
  remoteConvIdALocallyNotFound <- randomQualifiedId remoteDomainA
  remoteConvIdBNotFoundOnRemote <- randomQualifiedId remoteDomainB
  localConvIdNotFound <- randomQualifiedId localDomain
  remoteConvIdCFailure <- randomQualifiedId remoteDomainC

  eve <- randomQualifiedUser
  localConvIdNotParticipating <- decodeQualifiedConvId <$> postConv (qUnqualified eve) [] (Just "gossip about alice!") [] Nothing Nothing

  let aliceAsOtherMember = OtherMember aliceQ Nothing roleNameWireAdmin
  registerRemoteConv remoteConvIdA bobId Nothing (Set.fromList [aliceAsOtherMember])
  registerRemoteConv remoteConvIdB carlId Nothing (Set.fromList [aliceAsOtherMember])
  registerRemoteConv remoteConvIdBNotFoundOnRemote carlId Nothing (Set.fromList [aliceAsOtherMember])
  registerRemoteConv remoteConvIdCFailure deeId Nothing (Set.fromList [aliceAsOtherMember])

  let bobAsOtherMember = OtherMember bobQ Nothing roleNameWireAdmin
      carlAsOtherMember = OtherMember carlQ Nothing roleNameWireAdmin
      mockConversationA = mkProteusConv (qUnqualified remoteConvIdA) bobId roleNameWireAdmin [bobAsOtherMember]
      mockConversationB = mkProteusConv (qUnqualified remoteConvIdB) carlId roleNameWireAdmin [carlAsOtherMember]
      req =
        ListConversations . unsafeRange $
          [ localConvId,
            remoteConvIdA,
            remoteConvIdB,
            remoteConvIdALocallyNotFound,
            localConvIdNotFound,
            localConvIdNotParticipating,
            remoteConvIdBNotFoundOnRemote,
            remoteConvIdCFailure
          ]
  (respAll, receivedRequests) <- do
    let mock = do
          d <- frTargetDomain <$> getRequest
          asum
            [ guard (d == remoteDomainA) *> mockReply (GetConversationsResponse [mockConversationA]),
              guard (d == remoteDomainB) *> mockReply (GetConversationsResponse [mockConversationB]),
              guard (d == remoteDomainC) *> liftIO (throw (DiscoveryFailureSrvNotAvailable "domainC")),
              do
                r <- getRequest
                liftIO . assertFailure $ "Unrecognized domain: " <> show r
            ]
    withTempMockFederator' mock (listConvs alice req)
  convs <- responseJsonUnsafe <$> (pure respAll <!! const 200 === statusCode)

  liftIO $ do
    let expectedFound =
          sortOn
            cnvQualifiedId
            $ pure (remoteConversationView lAlice defMemberStatus (toRemoteUnsafe remoteDomainA mockConversationA))
              <> pure (remoteConversationView lAlice defMemberStatus (toRemoteUnsafe remoteDomainB mockConversationB))
              <> [localConv]
        actualFound = sortOn cnvQualifiedId $ crFound convs
    assertEqual "found conversations" expectedFound actualFound

    -- Assumes only one request is made
    let requestedConvIdsA =
          fmap F.gcrConvIds
            . (decode . frBody =<<)
            $ find ((== remoteDomainA) . frTargetDomain) receivedRequests
    assertEqual "only locally found conversations should be queried" (Just [qUnqualified remoteConvIdA]) requestedConvIdsA

    let expectedNotFound = sort [localConvIdNotFound, localConvIdNotParticipating, remoteConvIdALocallyNotFound, remoteConvIdBNotFoundOnRemote]
        actualNotFound = sort $ crNotFound convs
    assertEqual "not founds" expectedNotFound actualNotFound
    assertEqual "failures" [remoteConvIdCFailure] (crFailed convs)

testAddRemoteMemberInvalidDomain :: TestM ()
testAddRemoteMemberInvalidDomain = do
  alice <- randomUser
  bobId <- randomId
  let remoteBob = Qualified bobId (Domain "invalid.example.com")
  convId <- decodeConvId <$> postConv alice [] (Just "remote gossip") [] Nothing Nothing

  connectWithRemoteUser alice remoteBob

  postQualifiedMembers alice (remoteBob :| []) convId
    !!! do
      const 422 === statusCode
      const (Just "/federation/api-version")
        === preview (ix "data" . ix "path") . responseJsonUnsafe @Value
      const (Just "invalid.example.com")
        === preview (ix "data" . ix "domain") . responseJsonUnsafe @Value

-- This test is a safeguard to ensure adding remote members will fail
-- on environments where federation isn't configured (such as our production as of May 2021)
testAddRemoteMemberFederationDisabled :: TestM ()
testAddRemoteMemberFederationDisabled = do
  alice <- randomUser
  remoteBob <- flip Qualified (Domain "some-remote-backend.example.com") <$> randomId
  qconvId <- decodeQualifiedConvId <$> postConv alice [] (Just "remote gossip") [] Nothing Nothing
  let convId = qUnqualified qconvId
  connectWithRemoteUser alice remoteBob

  -- federator endpoint not configured is equivalent to federation being disabled
  -- This is the case on staging/production in May 2021.
  let federatorNotConfigured = optFederator .~ Nothing
  withSettingsOverrides federatorNotConfigured $
    postQualifiedMembers alice (remoteBob :| []) convId !!! do
      const 400 === statusCode
      const (Right "federation-not-enabled") === fmap label . responseJsonEither

  -- the member is not actually added to the conversation
  conv <- responseJsonError =<< getConvQualified alice qconvId <!! const 200 === statusCode
  liftIO $ map omQualifiedId (cmOthers (cnvMembers conv)) @?= []

testAddRemoteMemberFederationUnavailable :: TestM ()
testAddRemoteMemberFederationUnavailable = do
  alice <- randomUser
  remoteBob <- flip Qualified (Domain "some-remote-backend.example.com") <$> randomId
  qconvId <- decodeQualifiedConvId <$> postConv alice [] (Just "remote gossip") [] Nothing Nothing
  let convId = qUnqualified qconvId
  connectWithRemoteUser alice remoteBob

  -- federator endpoint being configured in brig and/or galley, but not being
  -- available (i.e. no service listing on that IP/port) can happen due to a
  -- misconfiguration of federator. That should give a 500.
  -- Port 1 should always be wrong hopefully.
  let federatorUnavailable = optFederator ?~ Endpoint "127.0.0.1" 1
  withSettingsOverrides federatorUnavailable $
    postQualifiedMembers alice (remoteBob :| []) convId !!! do
      const 500 === statusCode
      const (Right "federation-not-available") === fmap label . responseJsonEither

  -- in this case, we discover that federation is unavailable too late, and the
  -- member has already been added to the conversation
  conv <- responseJsonError =<< getConvQualified alice qconvId <!! const 200 === statusCode
  liftIO $ map omQualifiedId (cmOthers (cnvMembers conv)) @?= [remoteBob]

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
  e <- responseJsonError =<< postMembers alice (pure qeve) qconv <!! const 200 === statusCode
  liftIO $ do
    evtConv e @?= qconv
    evtType e @?= MemberJoin
    evtData e @?= EdMembersJoin (SimpleMembers [SimpleMember qeve roleNameWireAdmin])
    evtFrom e @?= qalice
  -- Check that last_event markers are set for all members
  forM_ [alice, bob, chuck, eve] $ \u -> do
    _ <- getSelfMember u conv <!! const 200 === statusCode
    pure ()

postMembersOk2 :: TestM ()
postMembersOk2 = do
  alice <- randomUser
  bob <- randomUser
  chuck <- randomQualifiedUser
  connectUsers alice (list1 bob [qUnqualified chuck])
  connectUsers bob (singleton . qUnqualified $ chuck)
  conv <- decodeConvId <$> postConv alice [bob, qUnqualified chuck] Nothing [] Nothing Nothing
  qconv <- Qualified conv <$> viewFederationDomain
  postMembers bob (pure chuck) qconv !!! do
    const 204 === statusCode
    const Nothing === responseBody
  chuck' <- responseJsonUnsafe <$> (getSelfMember (qUnqualified chuck) conv <!! const 200 === statusCode)
  liftIO $
    assertEqual "wrong self member" (Just chuck) (memId <$> chuck')

postMembersOk3 :: TestM ()
postMembersOk3 = do
  alice <- randomUser
  (bob, qbob) <- randomUserTuple
  eve <- randomUser
  connectUsers alice (list1 bob [eve])
  conv <- decodeConvId <$> postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
  qconv <- Qualified conv <$> viewFederationDomain
  -- Bob leaves
  deleteMemberQualified bob qbob qconv !!! const 200 === statusCode
  -- Fetch bob
  getSelfMember bob conv !!! const 200 === statusCode
  -- Alice re-adds Bob to the conversation
  postMembers alice (pure qbob) qconv !!! const 200 === statusCode
  -- Fetch bob again
  getSelfMember bob conv !!! const 200 === statusCode

postMembersFailNoGuestAccess :: TestM ()
postMembersFailNoGuestAccess = do
  alice <- randomUser
  bob <- randomUser
  peter <- randomUser
  eve <- ephemeralUser
  qeve <- Qualified eve <$> viewFederationDomain
  connectUsers alice (list1 bob [peter])
  Right noGuestsAccess <- liftIO $ genAccessRolesV2 [TeamMemberAccessRole, NonTeamMemberAccessRole] [GuestAccessRole]
  conv <- decodeConvId <$> postConv alice [bob, peter] (Just "gossip") [] (Just noGuestsAccess) Nothing
  qconv <- Qualified conv <$> viewFederationDomain
  postMembers alice (pure qeve) qconv !!! const 403 === statusCode

generateGuestLinkFailIfNoNonTeamMemberOrNoGuestAccess :: TestM ()
generateGuestLinkFailIfNoNonTeamMemberOrNoGuestAccess = do
  alice <- randomUser
  bob <- randomUser
  connectUsers alice (singleton bob)
  Right noGuestsAccess <- liftIO $ genAccessRolesV2 [TeamMemberAccessRole] [GuestAccessRole, NonTeamMemberAccessRole]
  convId <- decodeConvId <$> postConv alice [bob] (Just "gossip") [CodeAccess] (Just noGuestsAccess) Nothing
  postConvCode alice convId !!! const 403 === statusCode

postMembersFail :: TestM ()
postMembersFail = do
  alice <- randomUser
  (bob, qbob) <- randomUserTuple
  chuck <- randomUser
  (dave, qdave) <- randomUserTuple
  (eve, qeve) <- randomUserTuple
  connectUsers alice (list1 bob [chuck, eve])
  connectUsers eve (singleton bob)
  conv <- decodeConvId <$> postConv alice [bob, chuck] (Just "gossip") [] Nothing Nothing
  qconv <- Qualified conv <$> viewFederationDomain
  postMembers eve (pure qbob) qconv !!! const 404 === statusCode
  postMembers alice (pure qeve) qconv !!! const 200 === statusCode
  -- Not connected but already there
  postMembers chuck (pure qeve) qconv !!! const 204 === statusCode
  postMembers chuck (pure qdave) qconv !!! do
    const 403 === statusCode
    const (Just "not-connected") === fmap label . responseJsonUnsafe
  void $ connectUsers chuck (singleton dave)
  postMembers chuck (pure qdave) qconv !!! const 200 === statusCode
  postMembers chuck (pure qdave) qconv !!! const 204 === statusCode

postTooManyMembersFail :: TestM ()
postTooManyMembersFail = do
  n <- fromIntegral <$> view tsMaxConvSize
  alice <- randomUser
  bob <- randomUser
  chuck <- randomUser
  connectUsers alice (list1 bob [chuck])
  conv <- decodeConvId <$> postConv alice [bob, chuck] (Just "gossip") [] Nothing Nothing
  qconv <- Qualified conv <$> viewFederationDomain
  x : xs <- replicateM (n - 2) randomQualifiedUser
  postMembers chuck (x :| xs) qconv !!! do
    const 403 === statusCode
    const (Just "too-many-members") === fmap label . responseJsonUnsafe

-- Creates a conversation with three users from the same domain. Then it uses a
-- qualified endpoint for deleting a conversation member:
--
-- DELETE /conversations/:domain/:cnv/members/:domain/:usr
deleteMembersConvLocalQualifiedOk :: TestM ()
deleteMembersConvLocalQualifiedOk = do
  localDomain <- viewFederationDomain
  [alice, bob, eve] <- randomUsers 3
  let [qAlice, qBob, qEve] = (`Qualified` localDomain) <$> [alice, bob, eve]
  connectUsers alice (list1 bob [eve])
  conv <-
    decodeConvId
      <$> postConvQualified
        alice
        defNewProteusConv
          { newConvQualifiedUsers = [qBob, qEve],
            newConvName = checked "federated gossip"
          }
  let qconv = Qualified conv localDomain
  deleteMemberQualified bob qBob qconv !!! const 200 === statusCode
  deleteMemberQualified bob qBob qconv !!! const 404 === statusCode
  -- if the conversation still exists, don't respond with 404, but with 403.
  getConv bob conv !!! const 403 === statusCode
  deleteMemberQualified alice qEve qconv !!! const 200 === statusCode
  deleteMemberQualified alice qEve qconv !!! const 204 === statusCode
  deleteMemberQualified alice qAlice qconv !!! const 200 === statusCode
  deleteMemberQualified alice qAlice qconv !!! const 404 === statusCode

-- Creates a conversation with three users. Alice and Bob are on the local
-- domain, while Eve is on a remote domain. It uses a qualified endpoint for
-- removing Bob from the conversation:
--
-- DELETE /conversations/:domain/:cnv/members/:domain/:usr
deleteLocalMemberConvLocalQualifiedOk :: TestM ()
deleteLocalMemberConvLocalQualifiedOk = do
  localDomain <- viewFederationDomain
  [alice, bob] <- randomUsers 2
  eve <- randomId
  let [qAlice, qBob] = (`Qualified` localDomain) <$> [alice, bob]
      remoteDomain = Domain "far-away.example.com"
      qEve = Qualified eve remoteDomain

  connectUsers alice (singleton bob)
  connectWithRemoteUser alice qEve
  convId <-
    decodeConvId
      <$> postConvWithRemoteUsers
        alice
        defNewProteusConv {newConvQualifiedUsers = [qBob, qEve]}
  let qconvId = Qualified convId localDomain

  let mockReturnEve =
        mockedFederatedBrigResponse [(qEve, "Eve")]
          <|> mockReply ()
  (respDel, fedRequests) <-
    withTempMockFederator' mockReturnEve $
      deleteMemberQualified alice qBob qconvId
  let [galleyFederatedRequest] = fedRequestsForDomain remoteDomain Galley fedRequests
  assertRemoveUpdate galleyFederatedRequest qconvId qAlice [qUnqualified qEve] qBob

  liftIO $ do
    statusCode respDel @?= 200
    case responseJsonEither respDel of
      Left err -> assertFailure err
      Right e -> assertLeaveEvent qconvId qAlice [qBob] e

  -- Now that Bob is gone, try removing him once again
  deleteMemberQualified alice qBob qconvId !!! do
    const 204 === statusCode
    const Nothing === responseBody

-- Creates a conversation with five users. Alice and Bob are on the local
-- domain. Chad and Dee are on far-away-1.example.com. Eve is on
-- far-away-2.example.com. It uses a qualified endpoint to remove Chad from the
-- conversation:
--
-- DELETE /conversations/:domain/:cnv/members/:domain/:usr
deleteRemoteMemberConvLocalQualifiedOk :: TestM ()
deleteRemoteMemberConvLocalQualifiedOk = do
  localDomain <- viewFederationDomain
  [alice, bob] <- randomUsers 2
  let [qAlice, qBob] = (`Qualified` localDomain) <$> [alice, bob]
      remoteDomain1 = Domain "far-away-1.example.com"
      remoteDomain2 = Domain "far-away-2.example.com"
  qChad <- (`Qualified` remoteDomain1) <$> randomId
  qDee <- (`Qualified` remoteDomain1) <$> randomId
  qEve <- (`Qualified` remoteDomain2) <$> randomId
  connectUsers alice (singleton bob)
  mapM_ (connectWithRemoteUser alice) [qChad, qDee, qEve]

  let mockedResponse = do
        guardRPC "get-users-by-ids"
        d <- frTargetDomain <$> getRequest
        asum
          [ guard (d == remoteDomain1)
              *> mockReply [mkProfile qChad (Name "Chad"), mkProfile qDee (Name "Dee")],
            guard (d == remoteDomain2)
              *> mockReply [mkProfile qEve (Name "Eve")]
          ]
  (convId, _) <-
    withTempMockFederator' (mockedResponse <|> mockReply ()) $
      fmap decodeConvId $
        postConvQualified
          alice
          defNewProteusConv {newConvQualifiedUsers = [qBob, qChad, qDee, qEve]}
          <!! const 201 === statusCode
  let qconvId = Qualified convId localDomain

  (respDel, federatedRequests) <-
    withTempMockFederator' (mockedResponse <|> mockReply ()) $
      deleteMemberQualified alice qChad qconvId
  liftIO $ do
    statusCode respDel @?= 200
    case responseJsonEither respDel of
      Left err -> assertFailure err
      Right e -> assertLeaveEvent qconvId qAlice [qChad] e

  let [remote1GalleyFederatedRequest] = fedRequestsForDomain remoteDomain1 Galley federatedRequests
      [remote2GalleyFederatedRequest] = fedRequestsForDomain remoteDomain2 Galley federatedRequests
  assertRemoveUpdate remote1GalleyFederatedRequest qconvId qAlice [qUnqualified qChad, qUnqualified qDee] qChad
  assertRemoveUpdate remote2GalleyFederatedRequest qconvId qAlice [qUnqualified qEve] qChad

  -- Now that Chad is gone, try removing him once again
  deleteMemberQualified alice qChad qconvId !!! do
    const 204 === statusCode
    const Nothing === responseBody

-- Alice, a local user, leaves a remote conversation. Bob's domain is the same
-- as that of the conversation. The test uses the following endpoint:
--
-- DELETE /conversations/:domain/:cnv/members/:domain/:usr
leaveRemoteConvQualifiedOk :: TestM ()
leaveRemoteConvQualifiedOk = do
  localDomain <- viewFederationDomain
  alice <- randomUser
  let qAlice = Qualified alice localDomain
  conv <- randomId
  bob <- randomId
  let remoteDomain = Domain "faraway.example.com"
      qconv = Qualified conv remoteDomain
      qBob = Qualified bob remoteDomain
  let mockedFederatedGalleyResponse = do
        guardComponent Galley
        mockReply (F.LeaveConversationResponse (Right ()))
      mockResponses =
        mockedFederatedBrigResponse [(qBob, "Bob")]
          <|> mockedFederatedGalleyResponse

  (resp, fedRequests) <-
    withTempMockFederator' mockResponses $
      deleteMemberQualified alice qAlice qconv
  let leaveRequest =
        fromJust . decode . frBody . Imports.head $
          fedRequests
  liftIO $ do
    statusCode resp @?= 200
    case responseJsonEither resp of
      Left err -> assertFailure err
      Right e -> assertLeaveEvent qconv qAlice [qAlice] e
    F.lcConvId leaveRequest @?= conv
    F.lcLeaver leaveRequest @?= alice

-- Alice tries to leave a non-existent remote conversation
leaveNonExistentRemoteConv :: TestM ()
leaveNonExistentRemoteConv = do
  alice <- randomQualifiedUser
  let remoteDomain = Domain "faraway.example.com"
  conv <- randomQualifiedId remoteDomain

  let mockResponses = do
        guardComponent Galley
        mockReply $
          F.LeaveConversationResponse (Left F.RemoveFromConversationErrorNotFound)

  (resp, fedRequests) <-
    withTempMockFederator' mockResponses $
      responseJsonError
        =<< deleteMemberQualified (qUnqualified alice) alice conv
          <!! const 404 === statusCode
  let leaveRequest =
        fromJust . decode . frBody . Imports.head $
          fedRequests
  liftIO $ do
    fmap label resp @?= Just "no-conversation"
    F.lcConvId leaveRequest @?= qUnqualified conv
    F.lcLeaver leaveRequest @?= qUnqualified alice

-- Alice tries to leave a conversation of the wrong type
leaveRemoteConvDenied :: TestM ()
leaveRemoteConvDenied = do
  alice <- randomQualifiedUser
  let remoteDomain = Domain "faraway.example.com"
  conv <- randomQualifiedId remoteDomain

  let mockResponses = do
        guardComponent Galley
        mockReply $
          F.LeaveConversationResponse
            ( Left F.RemoveFromConversationErrorRemovalNotAllowed
            )

  (resp, fedRequests) <-
    withTempMockFederator' mockResponses $
      responseJsonError
        =<< deleteMemberQualified (qUnqualified alice) alice conv
          <!! const 403 === statusCode
  let leaveRequest =
        fromJust . decode . frBody . Imports.head $
          fedRequests
  liftIO $ do
    fmap label resp @?= Just "action-denied"
    F.lcConvId leaveRequest @?= qUnqualified conv
    F.lcLeaver leaveRequest @?= qUnqualified alice

-- Alice, a user remote to the conversation, tries to remove someone on her own
-- backend other than herself via:
--
-- DELETE /conversations/:domain/:cnv/members/:domain/:usr
removeLocalMemberConvQualifiedFail :: TestM ()
removeLocalMemberConvQualifiedFail = do
  alice <- randomUser
  conv <- randomId
  qBob <- randomQualifiedUser
  let remoteDomain = Domain "faraway.example.com"
      qconv = Qualified conv remoteDomain

  deleteMemberQualified alice qBob qconv !!! do
    const 403 === statusCode
    const (Just "action-denied") === fmap label . responseJsonUnsafe

-- Alice, a user remote to the conversation, tries to remove someone on a remote
-- backend via:
--
-- DELETE /conversations/:domain/:cnv/members/:domain/:usr
removeRemoteMemberConvQualifiedFail :: TestM ()
removeRemoteMemberConvQualifiedFail = do
  alice <- randomUser
  conv <- randomId
  bob <- randomId
  let remoteDomain = Domain "faraway.example.com"
      qconv = Qualified conv remoteDomain
      qBob = Qualified bob remoteDomain

  deleteMemberQualified alice qBob qconv !!! do
    const 403 === statusCode
    const (Just "action-denied") === fmap label . responseJsonUnsafe

deleteMembersQualifiedFailSelf :: TestM ()
deleteMembersQualifiedFailSelf = do
  (alice, qalice) <- randomUserTuple
  self <- decodeConvId <$> postSelfConv alice
  qself <- Qualified self <$> viewFederationDomain
  deleteMemberQualified alice qalice qself !!! const 403 === statusCode

deleteMembersQualifiedFailO2O :: TestM ()
deleteMembersQualifiedFailO2O = do
  alice <- randomUser
  (bob, qbob) <- randomUserTuple
  connectUsers alice (singleton bob)
  o2o <- decodeConvId <$> postO2OConv alice bob (Just "foo")
  qo2o <- Qualified o2o <$> viewFederationDomain
  deleteMemberQualified alice qbob qo2o !!! const 403 === statusCode

putQualifiedConvRenameFailure :: TestM ()
putQualifiedConvRenameFailure = do
  conv <- randomId
  qbob <- randomQualifiedUser
  let qconv = Qualified conv (qDomain qbob)
  putQualifiedConversationName (qUnqualified qbob) qconv "gossip"
    !!! do
      const 404 === statusCode
      const (Just "no-conversation") === fmap label . responseJsonUnsafe

putQualifiedConvRenameOk :: TestM ()
putQualifiedConvRenameOk = do
  c <- view tsCannon
  alice <- randomUser
  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob
  connectUsers alice (singleton bob)
  conv <- decodeConvId <$> postO2OConv alice bob (Just "gossip")
  let qconv = Qualified conv (qDomain qbob)
  WS.bracketR2 c alice bob $ \(wsA, wsB) -> do
    void $ putQualifiedConversationName bob qconv "gossip++" !!! const 200 === statusCode
    void . liftIO . WS.assertMatchN (5 # Second) [wsA, wsB] $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconv
      evtType e @?= ConvRename
      evtFrom e @?= qbob
      evtData e @?= EdConvRename (ConversationRename "gossip++")

putQualifiedConvRenameWithRemotesOk :: TestM ()
putQualifiedConvRenameWithRemotesOk = do
  c <- view tsCannon
  let remoteDomain = Domain "alice.example.com"
  qalice <- Qualified <$> randomId <*> pure remoteDomain
  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob

  connectWithRemoteUser bob qalice

  resp <-
    postConvWithRemoteUsers
      bob
      defNewProteusConv {newConvQualifiedUsers = [qalice]}
      <!! const 201 === statusCode
  let qconv = decodeQualifiedConvId resp

  WS.bracketR c bob $ \wsB -> do
    (_, requests) <-
      withTempMockFederator' (mockReply ()) $
        putQualifiedConversationName bob qconv "gossip++" !!! const 200 === statusCode

    req <- assertOne requests
    liftIO $ do
      frTargetDomain req @?= remoteDomain
      frComponent req @?= Galley
      frRPC req @?= "on-conversation-updated"
      Right cu <- pure . eitherDecode . frBody $ req
      F.cuConvId cu @?= qUnqualified qconv
      F.cuAction cu @?= SomeConversationAction (sing @'ConversationRenameTag) (ConversationRename "gossip++")

    void . liftIO . WS.assertMatch (5 # Second) wsB $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconv
      evtType e @?= ConvRename
      evtFrom e @?= qbob
      evtData e @?= EdConvRename (ConversationRename "gossip++")

putConvDeprecatedRenameOk :: TestM ()
putConvDeprecatedRenameOk = do
  c <- view tsCannon
  g <- viewGalley
  alice <- randomUser
  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob
  connectUsers alice (singleton bob)
  conv <- decodeConvId <$> postO2OConv alice bob (Just "gossip")
  let qconv = Qualified conv (qDomain qbob)
  WS.bracketR2 c alice bob $ \(wsA, wsB) -> do
    -- This endpoint is deprecated but clients still use it
    put
      ( g
          . paths ["conversations", toByteString' conv]
          . zUser bob
          . zConn "conn"
          . zType "access"
          . json (ConversationRename "gossip++")
      )
      !!! const 200
        === statusCode
    void . liftIO . WS.assertMatchN (5 # Second) [wsA, wsB] $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconv
      evtType e @?= ConvRename
      evtFrom e @?= qbob
      evtData e @?= EdConvRename (ConversationRename "gossip++")

putConvRenameOk :: TestM ()
putConvRenameOk = do
  c <- view tsCannon
  alice <- randomUser
  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob
  connectUsers alice (singleton bob)
  conv <- decodeConvId <$> postO2OConv alice bob (Just "gossip")
  let qconv = Qualified conv (qDomain qbob)
  WS.bracketR2 c alice bob $ \(wsA, wsB) -> do
    void $ putConversationName bob conv "gossip++" !!! const 200 === statusCode
    void . liftIO . WS.assertMatchN (5 # Second) [wsA, wsB] $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconv
      evtType e @?= ConvRename
      evtFrom e @?= qbob
      evtData e @?= EdConvRename (ConversationRename "gossip++")

putQualifiedOtherMemberOk :: TestM ()
putQualifiedOtherMemberOk = do
  c <- view tsCannon
  qalice <- randomQualifiedUser
  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob
      alice = qUnqualified qalice
  connectUsers alice (singleton bob)
  conv <- decodeConvId <$> postConv alice [bob] (Just "gossip") [] Nothing Nothing
  let qconv = Qualified conv (qDomain qbob)
      expectedMemberUpdateData =
        MemberUpdateData
          { misTarget = qalice,
            misOtrMutedStatus = Nothing,
            misOtrMutedRef = Nothing,
            misOtrArchived = Nothing,
            misOtrArchivedRef = Nothing,
            misHidden = Nothing,
            misHiddenRef = Nothing,
            misConvRoleName = Just roleNameWireMember
          }

  WS.bracketR2 c alice bob $ \(wsA, wsB) -> do
    -- demote qalice
    putOtherMemberQualified bob qalice (OtherMemberUpdate (Just roleNameWireMember)) qconv
      !!! const 200 === statusCode
    void . liftIO . WS.assertMatchN (5 # Second) [wsA, wsB] $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconv
      evtType e @?= MemberStateUpdate
      evtFrom e @?= qbob
      evtData e @?= EdMemberUpdate expectedMemberUpdateData

putOtherMemberOk :: TestM ()
putOtherMemberOk = do
  c <- view tsCannon
  qalice <- randomQualifiedUser
  qbob <- randomQualifiedUser
  let alice = qUnqualified qalice
      bob = qUnqualified qbob
  connectUsers alice (singleton bob)
  conv <- decodeConvId <$> postConv alice [bob] (Just "gossip") [] Nothing Nothing
  let qconv = Qualified conv (qDomain qbob)
      expectedMemberUpdateData =
        MemberUpdateData
          { misTarget = qalice,
            misOtrMutedStatus = Nothing,
            misOtrMutedRef = Nothing,
            misOtrArchived = Nothing,
            misOtrArchivedRef = Nothing,
            misHidden = Nothing,
            misHiddenRef = Nothing,
            misConvRoleName = Just roleNameWireMember
          }

  WS.bracketR2 c alice bob $ \(wsA, wsB) -> do
    -- demote alice
    putOtherMember bob alice (OtherMemberUpdate (Just roleNameWireMember)) conv
      !!! const 200 === statusCode
    void . liftIO . WS.assertMatchN (5 # Second) [wsA, wsB] $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconv
      evtType e @?= MemberStateUpdate
      evtFrom e @?= qbob
      evtData e @?= EdMemberUpdate expectedMemberUpdateData

putMemberOtrMuteOk :: TestM ()
putMemberOtrMuteOk = do
  putMemberOk (memberUpdate {mupOtrMuteStatus = Just 1, mupOtrMuteRef = Just "ref"})
  putMemberOk (memberUpdate {mupOtrMuteStatus = Just 0})

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
        { mupOtrMuteStatus = Just 0,
          mupOtrMuteRef = Just "mref",
          mupOtrArchive = Just True,
          mupOtrArchiveRef = Just "aref",
          mupHidden = Just True,
          mupHiddenRef = Just "href"
        }
    )

putRemoteConvMemberOtrMuteOk :: TestM ()
putRemoteConvMemberOtrMuteOk = do
  putRemoteConvMemberOk (memberUpdate {mupOtrMuteStatus = Just 1, mupOtrMuteRef = Just "ref"})
  putRemoteConvMemberOk (memberUpdate {mupOtrMuteStatus = Just 0})

putRemoteConvMemberOtrArchiveOk :: TestM ()
putRemoteConvMemberOtrArchiveOk = do
  putRemoteConvMemberOk (memberUpdate {mupOtrArchive = Just True, mupOtrArchiveRef = Just "ref"})
  putRemoteConvMemberOk (memberUpdate {mupOtrArchive = Just False})

putRemoteConvMemberHiddenOk :: TestM ()
putRemoteConvMemberHiddenOk = do
  putRemoteConvMemberOk (memberUpdate {mupHidden = Just True, mupHiddenRef = Just "ref"})
  putRemoteConvMemberOk (memberUpdate {mupHidden = Just False})

putRemoteConvMemberAllOk :: TestM ()
putRemoteConvMemberAllOk =
  putRemoteConvMemberOk
    ( memberUpdate
        { mupOtrMuteStatus = Just 0,
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
          { memId = qbob,
            memService = Nothing,
            memOtrMutedStatus = mupOtrMuteStatus update,
            memOtrMutedRef = mupOtrMuteRef update,
            memOtrArchived = Just True == mupOtrArchive update,
            memOtrArchivedRef = mupOtrArchiveRef update,
            memHidden = Just True == mupHidden update,
            memHiddenRef = mupHiddenRef update,
            memConvRoleName = roleNameWireAdmin
          }
  -- Update member state & verify push notification
  WS.bracketR c bob $ \ws -> do
    putMember bob update qconv !!! const 200 === statusCode
    void . liftIO . WS.assertMatch (5 # Second) ws $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconv
      evtType e @?= MemberStateUpdate
      evtFrom e @?= qbob
      case evtData e of
        EdMemberUpdate mis -> do
          assertEqual "otr_muted_status" (mupOtrMuteStatus update) (misOtrMutedStatus mis)
          assertEqual "otr_muted_ref" (mupOtrMuteRef update) (misOtrMutedRef mis)
          assertEqual "otr_archived" (mupOtrArchive update) (misOtrArchived mis)
          assertEqual "otr_archived_ref" (mupOtrArchiveRef update) (misOtrArchivedRef mis)
          assertEqual "hidden" (mupHidden update) (misHidden mis)
          assertEqual "hidden_ref" (mupHiddenRef update) (misHiddenRef mis)
        x -> assertFailure $ "Unexpected event data: " ++ show x
  -- Verify new member state
  rs <- getConvQualified bob qconv <!! const 200 === statusCode
  let bob' = cmSelf . cnvMembers <$> responseJsonUnsafe rs
  liftIO $ do
    assertBool "user" (isJust bob')
    let newBob = fromJust bob'
    assertEqual "id" (memId memberBob) (memId newBob)
    assertEqual "otr_muted_status" (memOtrMutedStatus memberBob) (memOtrMutedStatus newBob)
    assertEqual "otr_muted_ref" (memOtrMutedRef memberBob) (memOtrMutedRef newBob)
    assertEqual "otr_archived" (memOtrArchived memberBob) (memOtrArchived newBob)
    assertEqual "otr_archived_ref" (memOtrArchivedRef memberBob) (memOtrArchivedRef newBob)
    assertEqual "hidden" (memHidden memberBob) (memHidden newBob)
    assertEqual "hidden_ref" (memHiddenRef memberBob) (memHiddenRef newBob)

putRemoteConvMemberOk :: MemberUpdate -> TestM ()
putRemoteConvMemberOk update = do
  c <- view tsCannon
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice

  -- create a remote conversation with alice
  let remoteDomain = Domain "bobland.example.com"
  qbob <- Qualified <$> randomId <*> pure remoteDomain
  qconv <- Qualified <$> randomId <*> pure remoteDomain
  connectWithRemoteUser alice qbob

  fedGalleyClient <- view tsFedGalleyClient
  now <- liftIO getCurrentTime
  let cu =
        F.ConversationUpdate
          { cuTime = now,
            cuOrigUserId = qbob,
            cuConvId = qUnqualified qconv,
            cuAlreadyPresentUsers = [],
            cuAction =
              SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qalice) roleNameWireMember)
          }
  runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain cu

  -- Expected member state
  let memberAlice =
        Member
          { memId = qalice,
            memService = Nothing,
            memOtrMutedStatus = mupOtrMuteStatus update,
            memOtrMutedRef = mupOtrMuteRef update,
            memOtrArchived = Just True == mupOtrArchive update,
            memOtrArchivedRef = mupOtrArchiveRef update,
            memHidden = Just True == mupHidden update,
            memHiddenRef = mupHiddenRef update,
            memConvRoleName = roleNameWireMember
          }
  -- Update member state & verify push notification
  WS.bracketR c alice $ \ws -> do
    putMember alice update qconv !!! const 200 === statusCode
    void . liftIO . WS.assertMatch (5 # Second) ws $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconv
      evtType e @?= MemberStateUpdate
      evtFrom e @?= qalice
      case evtData e of
        EdMemberUpdate mis -> do
          assertEqual "otr_muted_status" (mupOtrMuteStatus update) (misOtrMutedStatus mis)
          assertEqual "otr_muted_ref" (mupOtrMuteRef update) (misOtrMutedRef mis)
          assertEqual "otr_archived" (mupOtrArchive update) (misOtrArchived mis)
          assertEqual "otr_archived_ref" (mupOtrArchiveRef update) (misOtrArchivedRef mis)
          assertEqual "hidden" (mupHidden update) (misHidden mis)
          assertEqual "hidden_ref" (mupHiddenRef update) (misHiddenRef mis)
        x -> assertFailure $ "Unexpected event data: " ++ show x

  -- Fetch remote conversation
  let bobAsLocal =
        LocalMember
          (qUnqualified qbob)
          defMemberStatus
          Nothing
          roleNameWireAdmin
  let mockConversation =
        mkProteusConv
          (qUnqualified qconv)
          (qUnqualified qbob)
          roleNameWireMember
          [localMemberToOther remoteDomain bobAsLocal]
      remoteConversationResponse = GetConversationsResponse [mockConversation]
  (rs, _) <-
    withTempMockFederator'
      (mockReply remoteConversationResponse)
      $ getConvQualified alice qconv
        <!! const 200 === statusCode

  -- Verify new member state
  let alice' = cmSelf . cnvMembers <$> responseJsonUnsafe rs
  liftIO $ do
    assertBool "user" (isJust alice')
    let newAlice = fromJust alice'
    assertEqual "id" (memId memberAlice) (memId newAlice)
    assertEqual "otr_muted_status" (memOtrMutedStatus memberAlice) (memOtrMutedStatus newAlice)
    assertEqual "otr_muted_ref" (memOtrMutedRef memberAlice) (memOtrMutedRef newAlice)
    assertEqual "otr_archived" (memOtrArchived memberAlice) (memOtrArchived newAlice)
    assertEqual "otr_archived_ref" (memOtrArchivedRef memberAlice) (memOtrArchivedRef newAlice)
    assertEqual "hidden" (memHidden memberAlice) (memHidden newAlice)
    assertEqual "hidden_ref" (memHiddenRef memberAlice) (memHiddenRef newAlice)

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
    getConvQualified alice qcnv !!! do
      const 200 === statusCode
      const (Just Nothing) === fmap cnvReceiptMode . responseJsonUnsafe
    -- Set receipt mode
    putReceiptMode alice cnv (ReceiptMode 0) !!! const 200 === statusCode
    -- Ensure the field is properly set
    getConvQualified alice qcnv !!! do
      const 200 === statusCode
      const (Just $ Just (ReceiptMode 0)) === fmap cnvReceiptMode . responseJsonUnsafe
    void . liftIO $ checkWs qalice (qcnv, wsB)
    -- No changes
    putReceiptMode alice cnv (ReceiptMode 0) !!! const 204 === statusCode
    -- No event should have been generated
    WS.assertNoEvent (1 # Second) [wsB]
    -- Ensure that the new field remains unchanged
    getConvQualified alice qcnv !!! do
      const 200 === statusCode
      const (Just $ Just (ReceiptMode 0)) === fmap cnvReceiptMode . responseJsonUnsafe
  qcnv' <- decodeQualifiedConvId <$> postConvWithReceipt alice [bob, jane] (Just "gossip") [] Nothing Nothing (ReceiptMode 0)
  getConvQualified alice qcnv' !!! do
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

-- | Test setup
-- A (local)  - alice: admin on remote conversation, adam: regular member of remote conversation
-- B (mocked) - owns the conversation
--
-- The federator on A is also mocked.
--
-- alice changes receipt remote via client api
-- assertion: A's federator is called correctly
-- assertion: backend A generates events for adam
-- and federator's response
putRemoteReceiptModeOk :: TestM ()
putRemoteReceiptModeOk = do
  c <- view tsCannon
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice

  -- create a remote conversation at bob with alice as admin
  let remoteDomain = Domain "bobland.example.com"
  qbob <- Qualified <$> randomId <*> pure remoteDomain
  qconv <- Qualified <$> randomId <*> pure remoteDomain
  connectWithRemoteUser alice qbob
  fedGalleyClient <- view tsFedGalleyClient
  now <- liftIO getCurrentTime
  let cuAddAlice =
        F.ConversationUpdate
          { cuTime = now,
            cuOrigUserId = qbob,
            cuConvId = qUnqualified qconv,
            cuAlreadyPresentUsers = [],
            cuAction =
              SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qalice) roleNameWireAdmin)
          }
  runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain cuAddAlice

  -- add another user adam as member
  qadam <- randomQualifiedUser
  let adam = qUnqualified qadam
  connectWithRemoteUser adam qbob
  let cuAddAdam =
        F.ConversationUpdate
          { cuTime = now,
            cuOrigUserId = qbob,
            cuConvId = qUnqualified qconv,
            cuAlreadyPresentUsers = [],
            cuAction =
              SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qadam) roleNameWireMember)
          }
  runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain cuAddAdam

  let newReceiptMode = ReceiptMode 42
  let action = ConversationReceiptModeUpdate newReceiptMode
  let responseConvUpdate =
        F.ConversationUpdate
          { cuTime = now,
            cuOrigUserId = qalice,
            cuConvId = qUnqualified qconv,
            cuAlreadyPresentUsers = [adam],
            cuAction =
              SomeConversationAction (sing @'ConversationReceiptModeUpdateTag) action
          }
  let mockResponse = mockReply (ConversationUpdateResponseUpdate responseConvUpdate)

  WS.bracketR c adam $ \wsAdam -> do
    (res, federatedRequests) <- withTempMockFederator' mockResponse $ do
      putQualifiedReceiptMode alice qconv newReceiptMode
        <!! const 200 === statusCode

    let event :: Event = responseJsonUnsafe res
    let (EdConvReceiptModeUpdate (ConversationReceiptModeUpdate receiptModeEvent)) = evtData event

    liftIO $ assertEqual "Unexcepected receipt mode in event" newReceiptMode receiptModeEvent

    cFedReq <- assertOne $ filter (\r -> frTargetDomain r == remoteDomain && frRPC r == "update-conversation") federatedRequests
    cFedReqBody <- assertRight $ parseFedRequest cFedReq
    liftIO $ do
      curUser cFedReqBody @?= alice
      curConvId cFedReqBody @?= qUnqualified qconv
      curAction cFedReqBody @?= SomeConversationAction (sing @'ConversationReceiptModeUpdateTag) action

    WS.assertMatch_ (5 # Second) wsAdam $ \n -> do
      liftIO $ wsAssertConvReceiptModeUpdate qconv qalice newReceiptMode n

putReceiptModeWithRemotesOk :: TestM ()
putReceiptModeWithRemotesOk = do
  c <- view tsCannon
  let remoteDomain = Domain "alice.example.com"
  qalice <- Qualified <$> randomId <*> pure remoteDomain
  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob

  connectWithRemoteUser bob qalice

  resp <-
    postConvWithRemoteUsers
      bob
      defNewProteusConv {newConvQualifiedUsers = [qalice]}
  let qconv = decodeQualifiedConvId resp

  WS.bracketR c bob $ \wsB -> do
    (_, requests) <-
      withTempMockFederator' (mockReply ()) $
        putQualifiedReceiptMode bob qconv (ReceiptMode 43) !!! const 200 === statusCode

    req <- assertOne requests
    liftIO $ do
      frTargetDomain req @?= remoteDomain
      frComponent req @?= Galley
      frRPC req @?= "on-conversation-updated"
      Right cu <- pure . eitherDecode . frBody $ req
      F.cuConvId cu @?= qUnqualified qconv
      F.cuAction cu
        @?= SomeConversationAction (sing @'ConversationReceiptModeUpdateTag) (ConversationReceiptModeUpdate (ReceiptMode 43))

    void . liftIO . WS.assertMatch (5 # Second) wsB $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconv
      evtType e @?= ConvReceiptModeUpdate
      evtFrom e @?= qbob
      evtData e
        @?= EdConvReceiptModeUpdate
          (ConversationReceiptModeUpdate (ReceiptMode 43))

postTypingIndicatorsV2 :: TestM ()
postTypingIndicatorsV2 = do
  c <- view tsCannon
  g <- view tsUnversionedGalley

  alice <- randomUser
  bob <- randomUser

  aliceL <- qualifyLocal alice
  bobL <- qualifyLocal bob

  connectUsers alice (singleton bob)

  conv <- decodeConvId <$> postO2OConv alice bob Nothing
  lcnv <- qualifyLocal conv

  WS.bracketR2 c alice bob $ \(wsAlice, wsBob) -> do
    post
      ( g
          . paths ["v2", "conversations", toByteString' conv, "typing"]
          . zUser alice
          . zConn "conn"
          . zType "access"
          . json StartedTyping
      )
      !!! const 200 === statusCode

    void . liftIO $
      WS.assertMatchN (5 # Second) [wsAlice, wsBob] $ \n ->
        wsAssertTyping (tUntagged lcnv) (tUntagged aliceL) StartedTyping n

    post
      ( g
          . paths ["v2", "conversations", toByteString' conv, "typing"]
          . zUser bob
          . zConn "conn"
          . zType "access"
          . json StoppedTyping
      )
      !!! const 200 === statusCode

    void . liftIO $
      WS.assertMatchN (5 # Second) [wsAlice, wsBob] $ \n ->
        wsAssertTyping (tUntagged lcnv) (tUntagged bobL) StoppedTyping n

postTypingIndicators :: TestM ()
postTypingIndicators = do
  domain <- viewFederationDomain
  c <- view tsCannon
  g <- viewGalley

  alice <- randomUser
  bob <- randomUser

  aliceL <- qualifyLocal alice
  bobL <- qualifyLocal bob

  connectUsers alice (singleton bob)

  conv <- decodeConvId <$> postO2OConv alice bob Nothing
  lcnv <- qualifyLocal conv

  WS.bracketR2 c alice bob $ \(wsAlice, wsBob) -> do
    -- to alice from bob
    post
      ( g
          . paths ["conversations", toByteString' domain, toByteString' conv, "typing"]
          . zUser bob
          . zConn "conn"
          . zType "access"
          . json StoppedTyping
      )
      !!! const 200 === statusCode

    void . liftIO $
      WS.assertMatchN (5 # Second) [wsAlice, wsBob] $ \n ->
        wsAssertTyping (tUntagged lcnv) (tUntagged bobL) StoppedTyping n

    -- to bob from alice
    post
      ( g
          . paths ["conversations", toByteString' domain, toByteString' conv, "typing"]
          . zUser alice
          . zConn "conn"
          . zType "access"
          . json StartedTyping
      )
      !!! const 200 === statusCode

    void . liftIO $
      WS.assertMatchN (5 # Second) [wsAlice, wsBob] $ \n ->
        wsAssertTyping (tUntagged lcnv) (tUntagged aliceL) StartedTyping n

postTypingIndicatorsHandlesNonsense :: TestM ()
postTypingIndicatorsHandlesNonsense = do
  domain <- viewFederationDomain
  g <- viewGalley

  alice <- randomUser
  bob <- randomUser

  connectUsers alice (singleton bob)
  conv <- decodeConvId <$> postO2OConv alice bob Nothing

  post
    ( g
        . paths ["conversations", toByteString' domain, toByteString' conv, "typing"]
        . zUser bob
        . zConn "conn"
        . zType "access"
        . json (object ["status" .= ("dummy" :: T.Text)])
    )
    !!! const 400 === statusCode

removeUserNoFederation :: TestM ()
removeUserNoFederation = do
  c <- view tsCannon
  [alice, bob, carl] <- replicateM 3 randomQualifiedUser
  let [alice', bob', carl'] = qUnqualified <$> [alice, bob, carl]

  connectUsers alice' (list1 bob' [carl'])

  qconv1 <- decodeQualifiedConvId <$> postConv alice' [bob'] (Just "gossip") [] Nothing Nothing
  qconv2 <- decodeQualifiedConvId <$> postConv alice' [bob', carl'] (Just "gossip2") [] Nothing Nothing
  qconv3 <- decodeQualifiedConvId <$> postConv alice' [carl'] (Just "gossip3") [] Nothing Nothing

  WS.bracketR3 c alice' bob' carl' $ \(wsA, wsB, wsC) -> do
    deleteUser bob' !!! const 200 === statusCode

    void . liftIO $
      WS.assertMatchN (5 # Second) [wsA, wsB] $
        wsAssertMembersLeave qconv1 bob [bob]
    void . liftIO $
      WS.assertMatchN (5 # Second) [wsA, wsB, wsC] $
        wsAssertMembersLeave qconv2 bob [bob]
  -- Check memberships
  mems1 <- fmap cnvMembers . responseJsonUnsafe <$> getConvQualified alice' qconv1
  mems2 <- fmap cnvMembers . responseJsonUnsafe <$> getConvQualified alice' qconv2
  mems3 <- fmap cnvMembers . responseJsonUnsafe <$> getConvQualified alice' qconv3
  let other u = find ((== u) . omQualifiedId) . cmOthers
  liftIO $ do
    (mems1 >>= other bob) @?= Nothing
    (mems2 >>= other bob) @?= Nothing
    (mems2 >>= other carl) @?= Just (OtherMember carl Nothing roleNameWireAdmin)
    (mems3 >>= other bob) @?= Nothing
    (mems3 >>= other carl) @?= Just (OtherMember carl Nothing roleNameWireAdmin)

removeUser :: TestM ()
removeUser = do
  c <- view tsCannon
  [alice, alexDel, amy] <- replicateM 3 randomQualifiedUser
  let [alice', alexDel', amy'] = qUnqualified <$> [alice, alexDel, amy]

  let bDomain = Domain "b.example.com"
  bart <- randomQualifiedId bDomain
  berta <- randomQualifiedId bDomain

  let cDomain = Domain "c.example.com"
  carl <- randomQualifiedId cDomain

  let dDomain = Domain "d.example.com"
  dwight <- randomQualifiedId dDomain
  dory <- randomQualifiedId dDomain

  connectUsers alice' (list1 alexDel' [amy'])
  connectWithRemoteUser alice' bart
  connectWithRemoteUser alice' berta
  connectWithRemoteUser alexDel' bart
  connectWithRemoteUser alice' carl
  connectWithRemoteUser alexDel' carl
  connectWithRemoteUser alice' dwight
  connectWithRemoteUser alexDel' dory

  qconvA1 <- decodeQualifiedConvId <$> postConv alice' [alexDel'] (Just "gossip") [] Nothing Nothing
  qconvA2 <- decodeQualifiedConvId <$> postConvWithRemoteUsers alice' defNewProteusConv {newConvQualifiedUsers = [alexDel, amy, berta, dwight]}
  qconvA3 <- decodeQualifiedConvId <$> postConv alice' [amy'] (Just "gossip3") [] Nothing Nothing
  qconvA4 <- decodeQualifiedConvId <$> postConvWithRemoteUsers alice' defNewProteusConv {newConvQualifiedUsers = [alexDel, bart, carl]}
  convB1 <- randomId -- a remote conversation at 'bDomain' that Alice, AlexDel and Bart will be in
  convB2 <- randomId -- a remote conversation at 'bDomain' that AlexDel and Bart will be in
  convC1 <- randomId -- a remote conversation at 'cDomain' that AlexDel and Carl will be in
  convD1 <- randomId -- a remote conversation at 'cDomain' that AlexDel and Dory will be in
  now <- liftIO getCurrentTime
  fedGalleyClient <- view tsFedGalleyClient
  let nc cid creator quids =
        F.ConversationCreated
          { F.ccTime = now,
            F.ccOrigUserId = qUnqualified creator,
            F.ccCnvId = cid,
            F.ccCnvType = RegularConv,
            F.ccCnvAccess = [],
            F.ccCnvAccessRoles = Set.fromList [],
            F.ccCnvName = Just "gossip4",
            F.ccNonCreatorMembers = Set.fromList $ createOtherMember <$> quids,
            F.ccMessageTimer = Nothing,
            F.ccReceiptMode = Nothing,
            F.ccProtocol = ProtocolProteus
          }
  runFedClient @"on-conversation-created" fedGalleyClient bDomain $ nc convB1 bart [alice, alexDel]
  runFedClient @"on-conversation-created" fedGalleyClient bDomain $ nc convB2 bart [alexDel]
  runFedClient @"on-conversation-created" fedGalleyClient cDomain $ nc convC1 carl [alexDel]
  runFedClient @"on-conversation-created" fedGalleyClient dDomain $ nc convD1 dory [alexDel]

  WS.bracketR3 c alice' alexDel' amy' $ \(wsAlice, wsAlexDel, wsAmy) -> do
    let handler = do
          d <- frTargetDomain <$> getRequest
          asum
            [ do
                guard (d == dDomain)
                throw (DiscoveryFailureSrvNotAvailable "dDomain"),
              do
                guard (d `elem` [bDomain, cDomain])
                asum
                  [ "leave-conversation" ~> F.LeaveConversationResponse (Right ()),
                    "on-conversation-updated" ~> ()
                  ]
            ]
    (_, fedRequests) <-
      withTempMockFederator' handler $
        deleteUser alexDel' !!! const 200 === statusCode

    liftIO $ do
      assertEqual ("expect exactly 7 federated requests in : " <> show fedRequests) 7 (length fedRequests)

    liftIO $ do
      bReq <- assertOne $ filter (matchFedRequest bDomain "on-user-deleted-conversations") fedRequests
      frComponent bReq @?= Galley
      frRPC bReq @?= "on-user-deleted-conversations"
      Right udcnB <- pure . eitherDecode . frBody $ bReq
      sort (fromRange (F.udcvConversations udcnB)) @?= sort [convB1, convB2]
      F.udcvUser udcnB @?= qUnqualified alexDel

    liftIO $ do
      cReq <- assertOne $ filter (matchFedRequest cDomain "on-user-deleted-conversations") fedRequests
      frComponent cReq @?= Galley
      frRPC cReq @?= "on-user-deleted-conversations"
      Right udcnC <- pure . eitherDecode . frBody $ cReq
      sort (fromRange (F.udcvConversations udcnC)) @?= sort [convC1]
      F.udcvUser udcnC @?= qUnqualified alexDel

    liftIO $ do
      dReq <- assertOne $ filter (matchFedRequest dDomain "on-user-deleted-conversations") fedRequests
      frComponent dReq @?= Galley
      frRPC dReq @?= "on-user-deleted-conversations"
      Right udcnD <- pure . eitherDecode . frBody $ dReq
      sort (fromRange (F.udcvConversations udcnD)) @?= sort [convD1]
      F.udcvUser udcnD @?= qUnqualified alexDel

    liftIO $ do
      WS.assertMatchN_ (5 # Second) [wsAlice, wsAlexDel] $
        wsAssertMembersLeave qconvA1 alexDel [alexDel]
      WS.assertMatchN_ (5 # Second) [wsAlice, wsAlexDel, wsAmy] $
        wsAssertMembersLeave qconvA2 alexDel [alexDel]

    liftIO $ do
      let bConvUpdateRPCs = filter (matchFedRequest bDomain "on-conversation-updated") fedRequests
      bConvUpdates <- mapM (assertRight . eitherDecode . frBody) bConvUpdateRPCs

      bConvUpdatesA2 <- assertOne $ filter (\cu -> cuConvId cu == qUnqualified qconvA2) bConvUpdates
      cuOrigUserId bConvUpdatesA2 @?= alexDel
      cuAction bConvUpdatesA2 @?= SomeConversationAction (sing @'ConversationLeaveTag) ()
      cuAlreadyPresentUsers bConvUpdatesA2 @?= [qUnqualified berta]

      bConvUpdatesA4 <- assertOne $ filter (\cu -> cuConvId cu == qUnqualified qconvA4) bConvUpdates
      cuOrigUserId bConvUpdatesA4 @?= alexDel
      cuAction bConvUpdatesA4 @?= SomeConversationAction (sing @'ConversationLeaveTag) ()
      cuAlreadyPresentUsers bConvUpdatesA4 @?= [qUnqualified bart]

    liftIO $ do
      cConvUpdateRPC <- assertOne $ filter (matchFedRequest cDomain "on-conversation-updated") fedRequests
      Right convUpdate <- pure . eitherDecode . frBody $ cConvUpdateRPC
      cuConvId convUpdate @?= qUnqualified qconvA4
      cuOrigUserId convUpdate @?= alexDel
      cuAction convUpdate @?= SomeConversationAction (sing @'ConversationLeaveTag) ()
      cuAlreadyPresentUsers convUpdate @?= [qUnqualified carl]

    liftIO $ do
      dConvUpdateRPC <- assertOne $ filter (matchFedRequest dDomain "on-conversation-updated") fedRequests
      Right convUpdate <- pure . eitherDecode . frBody $ dConvUpdateRPC
      cuConvId convUpdate @?= qUnqualified qconvA2
      cuOrigUserId convUpdate @?= alexDel
      cuAction convUpdate @?= SomeConversationAction (sing @'ConversationLeaveTag) ()
      cuAlreadyPresentUsers convUpdate @?= [qUnqualified dwight]

  -- Check memberships
  mems1 <- fmap cnvMembers . responseJsonError =<< getConvQualified alice' qconvA1
  mems2 <- fmap cnvMembers . responseJsonError =<< getConvQualified alice' qconvA2
  mems3 <- fmap cnvMembers . responseJsonError =<< getConvQualified alice' qconvA3
  mems4 <- fmap cnvMembers . responseJsonError =<< getConvQualified alice' qconvA4
  let findOther u = find ((== u) . omQualifiedId) . cmOthers
  liftIO $ do
    findOther alexDel mems1 @?= Nothing
    findOther alexDel mems2 @?= Nothing
    findOther amy mems2 @?= Just (OtherMember amy Nothing roleNameWireAdmin)
    findOther alexDel mems3 @?= Nothing
    findOther amy mems3 @?= Just (OtherMember amy Nothing roleNameWireAdmin)
    findOther alexDel mems4 @?= Nothing
  where
    createOtherMember :: Qualified UserId -> OtherMember
    createOtherMember quid =
      OtherMember
        { omQualifiedId = quid,
          omService = Nothing,
          omConvRoleName = roleNameWireAdmin
        }

testAllOne2OneConversationRequests :: TestM ()
testAllOne2OneConversationRequests = do
  for_ [LocalActor, RemoteActor] $ \actor ->
    for_ [Included, Excluded] $ \desired ->
      for_ [True, False] $ \shouldBeLocal ->
        testOne2OneConversationRequest shouldBeLocal actor desired

testOne2OneConversationRequest :: Bool -> Actor -> DesiredMembership -> TestM ()
testOne2OneConversationRequest shouldBeLocal actor desired = do
  alice <- qTagUnsafe <$> randomQualifiedUser
  (bob, expectedConvId) <- generateRemoteAndConvId shouldBeLocal alice

  convId <- do
    let req = UpsertOne2OneConversationRequest alice bob actor desired Nothing
    res <-
      iUpsertOne2OneConversation req
        <!! statusCode === const 200
    uuorConvId <$> responseJsonError res

  liftIO $ convId @?= expectedConvId

  if shouldBeLocal
    then
      ( do
          members <- case actor of
            LocalActor -> runMaybeT $ do
              resp <- lift $ getConvQualified (tUnqualified alice) convId
              guard $ statusCode resp == 200
              conv <- lift $ responseJsonError resp
              pure . map omQualifiedId . cmOthers . cnvMembers $ conv
            RemoteActor -> do
              fedGalleyClient <- view tsFedGalleyClient
              GetConversationsResponse convs <-
                runFedClient @"get-conversations" fedGalleyClient (tDomain bob) $
                  F.GetConversationsRequest
                    { F.gcrUserId = tUnqualified bob,
                      F.gcrConvIds = [qUnqualified convId]
                    }
              pure
                . fmap (map omQualifiedId . rcmOthers . rcnvMembers)
                . listToMaybe
                $ convs
          liftIO $ case desired of
            Included -> members @?= Just []
            Excluded -> members @?= Nothing
      )
    else
      ( do
          found <- do
            let rconv = mkProteusConv (qUnqualified convId) (tUnqualified bob) roleNameWireAdmin []
            (resp, _) <-
              withTempMockFederator' (mockReply (F.GetConversationsResponse [rconv])) $
                getConvQualified (tUnqualified alice) convId
            pure $ statusCode resp == 200
          liftIO $ found @?= ((actor, desired) == (LocalActor, Included))
      )

updateTypingIndicatorToRemoteUserRemoteConv :: TestM ()
updateTypingIndicatorToRemoteUserRemoteConv = do
  c <- view tsCannon
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice

  -- create a remote conversation with alice
  let remoteDomain = Domain "bobland.example.com"
  qbob <- Qualified <$> randomId <*> pure remoteDomain
  qconv <- Qualified <$> randomId <*> pure remoteDomain
  connectWithRemoteUser alice qbob

  fedGalleyClient <- view tsFedGalleyClient
  now <- liftIO getCurrentTime
  let cu =
        F.ConversationUpdate
          { cuTime = now,
            cuOrigUserId = qbob,
            cuConvId = qUnqualified qconv,
            cuAlreadyPresentUsers = [],
            cuAction =
              SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qalice) roleNameWireMember)
          }
  runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain cu

  -- Fetch remote conversation
  let bobAsLocal =
        LocalMember
          (qUnqualified qbob)
          defMemberStatus
          Nothing
          roleNameWireAdmin
  let mockConversation =
        mkProteusConv
          (qUnqualified qconv)
          (qUnqualified qbob)
          roleNameWireMember
          [localMemberToOther remoteDomain bobAsLocal]
      remoteConversationResponse = GetConversationsResponse [mockConversation]
  void
    $ withTempMockFederator'
      (mockReply remoteConversationResponse)
    $ getConvQualified alice qconv
      <!! const 200 === statusCode

  WS.bracketR c alice $ \wsAlice -> do
    -- Started
    void $
      withTempMockFederator' (mockReply ()) $ do
        -- post typing indicator from bob to alice
        let tcReq =
              TypingDataUpdateRequest
                { tdurTypingStatus = StartedTyping,
                  tdurUserId = qUnqualified qbob,
                  tdurConvId = qUnqualified qconv
                }

        runFedClient @"on-typing-indicator-updated" fedGalleyClient (qDomain qalice) tcReq

    -- backend A generates a notification for alice
    void $
      WS.awaitMatch (5 # Second) wsAlice $ \n -> do
        liftIO $ wsAssertTyping qconv qalice StartedTyping n

    -- stopped
    void $
      withTempMockFederator' (mockReply ()) $ do
        -- post typing indicator from bob to alice
        let tcReq =
              TypingDataUpdateRequest
                { tdurTypingStatus = StoppedTyping,
                  tdurUserId = qUnqualified qbob,
                  tdurConvId = qUnqualified qconv
                }

        runFedClient @"on-typing-indicator-updated" fedGalleyClient (qDomain qalice) tcReq

    -- backend A generates a notification for alice
    void $
      WS.awaitMatch (5 # Second) wsAlice $ \n -> do
        liftIO $ wsAssertTyping qconv qalice StoppedTyping n

updateTypingIndicatorFromRemoteUser :: TestM ()
updateTypingIndicatorFromRemoteUser = do
  localDomain <- viewFederationDomain
  [alice, bob] <- randomUsers 2
  let qAlice = Qualified alice localDomain
      remoteDomain = Domain "far-away.example.com"
      qBob = Qualified bob remoteDomain

  connectWithRemoteUser alice qBob
  convId <-
    decodeConvId
      <$> postConvWithRemoteUsers
        alice
        defNewProteusConv {newConvQualifiedUsers = [qBob]}
  let qconvId = Qualified convId localDomain

  c <- view tsCannon
  WS.bracketR c alice $ \wsAlice -> do
    -- Started
    void $
      withTempMockFederator' (mockReply ()) $ do
        -- post typing indicator from bob to alice
        let tcReq =
              TypingDataUpdateRequest
                { tdurTypingStatus = StartedTyping,
                  tdurUserId = bob,
                  tdurConvId = convId
                }

        fedGalleyClient <- view tsFedGalleyClient
        runFedClient @"on-typing-indicator-updated" fedGalleyClient (qDomain qAlice) tcReq

    -- backend A generates a notification for alice
    void $
      WS.awaitMatch (5 # Second) wsAlice $ \n -> do
        liftIO $ wsAssertTyping qconvId qAlice StartedTyping n

    -- stopped
    void $
      withTempMockFederator' (mockReply ()) $ do
        -- post typing indicator from bob to alice
        let tcReq =
              TypingDataUpdateRequest
                { tdurTypingStatus = StoppedTyping,
                  tdurUserId = bob,
                  tdurConvId = convId
                }

        fedGalleyClient <- view tsFedGalleyClient
        runFedClient @"on-typing-indicator-updated" fedGalleyClient (qDomain qAlice) tcReq

    -- backend A generates a notification for alice
    void $
      WS.awaitMatch (5 # Second) wsAlice $ \n -> do
        liftIO $ wsAssertTyping qconvId qAlice StoppedTyping n

updateTypingIndicatorToRemoteUser :: TestM ()
updateTypingIndicatorToRemoteUser = do
  localDomain <- viewFederationDomain
  [alice, bob] <- randomUsers 2
  let remoteDomain = Domain "far-away.example.com"
      qBob = Qualified bob remoteDomain

  connectWithRemoteUser alice qBob
  convId <-
    decodeConvId
      <$> postConvWithRemoteUsers
        alice
        defNewProteusConv {newConvQualifiedUsers = [qBob]}
  let qconvId = Qualified convId localDomain

  c <- view tsCannon
  WS.bracketR c bob $ \wsBob -> do
    -- started
    void $
      withTempMockFederator' (mockReply ()) $ do
        -- post typing indicator from alice to bob
        let tcReq =
              TypingDataUpdateRequest
                { tdurTypingStatus = StartedTyping,
                  tdurUserId = alice,
                  tdurConvId = convId
                }

        fedGalleyClient <- view tsFedGalleyClient
        runFedClient @"on-typing-indicator-updated" fedGalleyClient (qDomain qBob) tcReq

    -- backend A generates a notification for bob
    void $
      WS.awaitMatch (5 # Second) wsBob $ \n -> do
        liftIO $ wsAssertTyping qconvId qBob StartedTyping n

    -- stopped
    void $
      withTempMockFederator' (mockReply ()) $ do
        -- post typing indicator from alice to bob
        let tcReq =
              TypingDataUpdateRequest
                { tdurTypingStatus = StoppedTyping,
                  tdurUserId = alice,
                  tdurConvId = convId
                }

        fedGalleyClient <- view tsFedGalleyClient
        runFedClient @"on-typing-indicator-updated" fedGalleyClient (qDomain qBob) tcReq

    -- backend A generates a notification for bob
    void $
      WS.awaitMatch (5 # Second) wsBob $ \n -> do
        liftIO $ wsAssertTyping qconvId qBob StoppedTyping n
