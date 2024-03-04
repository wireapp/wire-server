{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

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

import API.CustomBackend qualified as CustomBackend
import API.Federation qualified as Federation
import API.Federation.Util
import API.MLS qualified
import API.MessageTimer qualified as MessageTimer
import API.Roles qualified as Roles
import API.SQS
import API.Teams qualified as Teams
import API.Teams.Feature qualified as TeamFeature
import API.Teams.LegalHold qualified as Teams.LegalHold
import API.Teams.LegalHold.DisabledByDefault qualified
import API.Util
import API.Util qualified as Util
import API.Util.TeamFeature as TeamFeatures
import API.Util.TeamFeature qualified as Util
import Bilge hiding (head, timeout)
import Bilge qualified
import Bilge.Assert
import Control.Concurrent.Async qualified as Async
import Control.Exception (throw)
import Control.Lens hiding ((#), (.=))
import Control.Monad.Trans.Maybe
import Data.Aeson hiding (json)
import Data.ByteString qualified as BS
import Data.ByteString.Conversion
import Data.Code qualified as Code
import Data.Domain
import Data.Id
import Data.Json.Util (toBase64Text, toUTCTimeMillis)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List1 hiding (head)
import Data.List1 qualified as List1
import Data.Map.Strict qualified as Map
import Data.Misc
import Data.Qualified
import Data.Range
import Data.Set qualified as Set
import Data.Singletons
import Data.Text qualified as T
import Data.Text.Ascii qualified as Ascii
import Data.Time.Clock (getCurrentTime)
import Federator.Discovery (DiscoveryFailure (..))
import Federator.MockServer
import Galley.API.Mapping
import Galley.Options (federator, rabbitmq)
import Galley.Types.Conversations.Members
import Imports hiding (id)
import Imports qualified as I
import Network.HTTP.Types.Status qualified as HTTP
import Network.Wai.Utilities.Error
import Test.QuickCheck (arbitrary, generate)
import Test.Tasty
import Test.Tasty.Cannon (TimeoutUnit (..), (#))
import Test.Tasty.Cannon qualified as WS
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Wire.API.Connection
import Wire.API.Conversation
import Wire.API.Conversation qualified as C
import Wire.API.Conversation.Action
import Wire.API.Conversation.Code hiding (Value)
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Event.LeaveReason
import Wire.API.Federation.API
import Wire.API.Federation.API.Brig
import Wire.API.Federation.API.Common
import Wire.API.Federation.API.Galley
import Wire.API.Internal.Notification
import Wire.API.Message
import Wire.API.Message qualified as Message
import Wire.API.Routes.Internal.Galley.ConversationsIntra
import Wire.API.Routes.MultiTablePaging
import Wire.API.Routes.Version
import Wire.API.Routes.Versioned
import Wire.API.Team.Feature qualified as Public
import Wire.API.Team.Member qualified as Teams
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
          test s "create conversation with remote users all reachable" (postConvWithRemoteUsersOk $ Set.fromList [rb1, rb2]),
          test s "create conversation with remote users some unreachable" (postConvWithUnreachableRemoteUsers $ Set.fromList [rb1, rb2, rb3, rb4]),
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
          test s "create conversation with optional remote users when remote user's domain doesn't exist" postConvQualifiedNonExistentDomain,
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
          test s "get conversations/:domain/:cnv - local" testGetQualifiedLocalConv,
          test s "get conversations/:domain/:cnv - local, not found" testGetQualifiedLocalConvNotFound,
          test s "get conversations/:domain/:cnv - local, not participating" testGetQualifiedLocalConvNotParticipating,
          test s "get conversations/:domain/:cnv - remote" testGetQualifiedRemoteConv,
          test s "get conversations/:domain/:cnv - remote, not found" testGetQualifiedRemoteConvNotFound,
          test s "get conversations/:domain/:cnv - remote, not found on remote" testGetQualifiedRemoteConvNotFoundOnRemote,
          test s "post conversations/list/v2" testBulkGetQualifiedConvs,
          test s "add remote members on invalid domain" testAddRemoteMemberInvalidDomain,
          test s "add remote members when federation isn't enabled" testAddRemoteMemberFederationDisabled,
          test s "delete conversations/:domain/:cnv/members/:domain/:usr - fail, self conv" deleteMembersQualifiedFailSelf,
          test s "delete conversations/:domain:/cnv/members/:domain/:usr - fail, 1:1 conv" deleteMembersQualifiedFailO2O,
          test s "delete conversations/:domain/:cnv/members/:domain/:usr - local conv with all locals" deleteMembersConvLocalQualifiedOk,
          test s "delete conversations/:domain/:cnv/members/:domain/:usr - remote conv, leave conv" leaveRemoteConvQualifiedOk,
          test s "delete conversations/:domain/:cnv/members/:domain/:usr - remote conv, leave conv, non-existent" leaveNonExistentRemoteConv,
          test s "delete conversations/:domain/:cnv/members/:domain/:usr - remote conv, leave conv, denied" leaveRemoteConvDenied,
          test s "delete conversations/:domain/:cnv/members/:domain/:usr - remote conv, remove local user, fail" removeLocalMemberConvQualifiedFail,
          test s "delete conversations/:domain/:cnv/members/:domain/:usr - remote conv, remove remote user, fail" removeRemoteMemberConvQualifiedFail,
          test s "rename conversation (deprecated endpoint)" putConvDeprecatedRenameOk,
          test s "rename conversation" putConvRenameOk,
          test s "rename qualified conversation" putQualifiedConvRenameOk,
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
          test s "remote conversation receipt mode update" putRemoteReceiptModeOk,
          test s "leave connect conversation" leaveConnectConversation,
          test s "post conversations/:cnv/otr/message: message delivery and missing clients" postCryptoMessageVerifyMsgSentAndRejectIfMissingClient,
          test s "post conversations/:cnv/otr/message: mismatch and prekey fetching" postCryptoMessageVerifyRejectMissingClientAndRepondMissingPrekeysJson,
          test s "post conversations/:cnv/otr/message: mismatch with protobuf" postCryptoMessageVerifyRejectMissingClientAndRepondMissingPrekeysProto,
          test s "post conversations/:cnv/otr/message: unknown sender client" postCryptoMessageNotAuthorizeUnknownClient,
          test s "post conversations/:cnv/otr/message: ignore_missing and report_missing" postCryptoMessageVerifyCorrectResponseIfIgnoreAndReportMissingQueryParam,
          test s "post message qualified - local owning backend - missing clients" postMessageQualifiedLocalOwningBackendMissingClients,
          test s "post message qualified - local owning backend - redundant and deleted clients" postMessageQualifiedLocalOwningBackendRedundantAndDeletedClients,
          test s "post message qualified - local owning backend - ignore missing" postMessageQualifiedLocalOwningBackendIgnoreMissingClients,
          test s "post message qualified - local owning backend - failed to send clients" postMessageQualifiedLocalOwningBackendFailedToSendClients,
          test s "post message qualified - local owning backend - failed to fetch clients" postMessageQualifiedFailedToSendFetchingClients,
          test s "post message qualified - remote owning backend - federation failure" postMessageQualifiedRemoteOwningBackendFailure,
          test s "post message qualified - remote owning backend - success" postMessageQualifiedRemoteOwningBackendSuccess,
          test s "join conversation" postJoinConvOk,
          test s "get code-access conversation information" testJoinCodeConv,
          test s "join code-access conversation - no password" postJoinCodeConvOk,
          test s "join code-access conversation - password" postJoinCodeConvWithPassword,
          test s "convert invite to code-access conversation" postConvertCodeConv,
          test s "convert code to team-access conversation" postConvertTeamConv,
          test s "team member can't join via guest link if access role removed" testTeamMemberCantJoinViaGuestLinkIfAccessRoleRemoved,
          test s "cannot join private conversation" postJoinConvFail,
          test s "revoke guest links for team conversation" testJoinTeamConvGuestLinksDisabled,
          test s "revoke guest links for non-team conversation" testJoinNonTeamConvGuestLinksDisabled,
          test s "get code rejected if guest links disabled" testGetCodeRejectedIfGuestLinksDisabled,
          test s "post code rejected if guest links disabled" testPostCodeRejectedIfGuestLinksDisabled,
          testGroup
            "conversation code already exists"
            [ test s "existing has no password, requested has no password - 201" postCodeWithoutPasswordExistsWithoutPasswordRequested,
              test s "existing has no password, requested has password - 409" postCodeWithoutPasswordExistsWithPasswordRequested,
              test s "existing has password, requested has no password - 409" postCodeWithPasswordExistsWithoutPasswordRequested,
              test s "existing has password, requested has password - 409" postCodeWithPasswordExistsWithPasswordRequested
            ],
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
              test s "send typing indicators with invalid pyaload" postTypingIndicatorsHandlesNonsense
            ]
        ]
    rb1, rb2, rb3, rb4 :: Remote Backend
    rb1 =
      toRemoteUnsafe
        (Domain "c.example.com")
        ( Backend
            { bReachable = BackendReachable,
              bUsers = 2
            }
        )
    rb2 =
      toRemoteUnsafe
        (Domain "d.example.com")
        ( Backend
            { bReachable = BackendReachable,
              bUsers = 1
            }
        )
    rb3 =
      toRemoteUnsafe
        (Domain "e.example.com")
        ( Backend
            { bReachable = BackendUnreachable,
              bUsers = 2
            }
        )
    rb4 =
      toRemoteUnsafe
        (Domain "f.example.com")
        ( Backend
            { bReachable = BackendUnreachable,
              bUsers = 1
            }
        )

getNotFullyConnectedBackendsMock :: Mock LByteString
getNotFullyConnectedBackendsMock = "get-not-fully-connected-backends" ~> NonConnectedBackends mempty

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
        Nothing
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
    qcid <- assertConv rsp RegularConv (Just alice) qalice [qbob, qjane] (Just nameMaxSize) Nothing
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

postConvWithUnreachableRemoteUsers :: Set (Remote Backend) -> TestM ()
postConvWithUnreachableRemoteUsers rbs = do
  c <- view tsCannon
  (alice, _qAlice) <- randomUserTuple
  (alex, qAlex) <- randomUserTuple
  connectUsers alice (singleton alex)
  (allRemotes, participatingRemotes) <- do
    v <- forM (toList rbs) $ \rb -> do
      users <- connectBackend alice rb
      pure (users, participating rb users)
    pure $ foldr (\(a, p) acc -> bimap ((<>) a) ((<>) p) acc) ([], []) v
  liftIO $
    assertBool "No unreachable backend in the test" (allRemotes /= participatingRemotes)

  let convName = "some chat"
      otherLocals = [qAlex]
      joiners = allRemotes <> otherLocals
      unreachableBackends =
        Set.fromList $
          foldMap
            ( \rb ->
                guard (rbReachable rb == BackendUnreachable)
                  $> tDomain rb
            )
            rbs
  WS.bracketR2 c alice alex $ \(wsAlice, wsAlex) -> do
    void
      $ withTempMockFederator'
        ( asum
            [ "get-not-fully-connected-backends" ~> NonConnectedBackends mempty,
              mockUnreachableFor unreachableBackends,
              "on-conversation-created" ~> EmptyResponse,
              "on-conversation-updated" ~> EmptyResponse
            ]
        )
      $ postConvQualified
        alice
        Nothing
        defNewProteusConv
          { newConvName = checked convName,
            newConvQualifiedUsers = joiners
          }
        <!! const 533 === statusCode
    groupConvs <- filter ((== RegularConv) . cnvmType . cnvMetadata) <$> getAllConvs alice
    liftIO $
      assertEqual
        "Alice does have a group conversation, while she should not!"
        []
        groupConvs
    WS.assertNoEvent (3 # Second) [wsAlice, wsAlex]

postConvWithRemoteUsersOk :: Set (Remote Backend) -> TestM ()
postConvWithRemoteUsersOk rbs = do
  c <- view tsCannon
  (alice, qAlice) <- randomUserTuple
  (alex, qAlex) <- randomUserTuple
  (amy, qAmy) <- randomUserTuple
  connectUsers alice (list1 alex [amy])
  (allRemotes, participatingRemotes) <- do
    v <- forM (toList rbs) $ \rb -> do
      users <- connectBackend alice rb
      pure (users, participating rb users)
    pure $ foldr (\(a, p) acc -> bimap ((<>) a) ((<>) p) acc) ([], []) v
  liftIO $
    assertBool "Not every backend is reachable in the test" (allRemotes == participatingRemotes)

  let convName = "some chat"
      otherLocals = [qAlex, qAmy]
  WS.bracketR3 c alice alex amy $ \(wsAlice, wsAlex, wsAmy) -> do
    let joiners = allRemotes <> otherLocals
        unreachableBackends =
          Set.fromList $
            foldMap
              ( \rb ->
                  guard (rbReachable rb == BackendUnreachable)
                    $> tDomain rb
              )
              rbs
    (rsp, federatedRequests) <-
      withTempMockFederator'
        ( asum
            [ getNotFullyConnectedBackendsMock,
              mockUnreachableFor unreachableBackends,
              "on-conversation-created" ~> EmptyResponse,
              "on-conversation-updated" ~> EmptyResponse
            ]
        )
        $ postConvQualified
          alice
          Nothing
          defNewProteusConv
            { newConvName = checked convName,
              newConvQualifiedUsers = joiners
            }
          <!! const 201 === statusCode
    let minimalShouldBePresent = otherLocals
        minimalShouldBePresentSet = Set.fromList (toOtherMember <$> minimalShouldBePresent)
    qcid <-
      assertConv
        rsp
        RegularConv
        (Just alice)
        qAlice
        (otherLocals <> participatingRemotes)
        (Just convName)
        Nothing
    let cid = qUnqualified qcid
    cvs <- mapM (convView qcid) [alice, alex, amy]
    liftIO $
      mapM_ WS.assertSuccess
        =<< Async.mapConcurrently (checkWs qAlice) (zip cvs [wsAlice, wsAlex, wsAmy])

    liftIO $ do
      let expectedReqs =
            Set.fromList $
              [ "on-conversation-created",
                "on-conversation-updated"
              ]
       in assertBool "Some federated calls are missing" $
            expectedReqs `Set.isSubsetOf` Set.fromList (frRPC <$> federatedRequests)

    -- assertions on the conversation.create event triggering federation request
    let fedReqsCreated = filter (\r -> frRPC r == "on-conversation-created") federatedRequests
    fedReqCreatedBodies <- for fedReqsCreated $ assertRight . parseFedRequest
    forM_ fedReqCreatedBodies $ \(fedReqCreatedBody :: ConversationCreated ConvId) -> liftIO $ do
      fedReqCreatedBody.origUserId @?= alice
      fedReqCreatedBody.cnvId @?= cid
      fedReqCreatedBody.cnvType @?= RegularConv
      fedReqCreatedBody.cnvAccess @?= [InviteAccess]
      fedReqCreatedBody.cnvAccessRoles
        @?= Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole, ServiceAccessRole]
      fedReqCreatedBody.cnvName @?= Just convName
      assertBool "Notifying an incorrect set of conversation members" $
        minimalShouldBePresentSet `Set.isSubsetOf` fedReqCreatedBody.nonCreatorMembers
      fedReqCreatedBody.messageTimer @?= Nothing
      fedReqCreatedBody.receiptMode @?= Nothing

    -- assertions on the conversation.member-join event triggering federation request
    let fedReqsAdd = filter (\r -> frRPC r == "on-conversation-updated") federatedRequests
    fedReqAddBodies <- for fedReqsAdd $ assertRight . parseFedRequest
    forM_ fedReqAddBodies $ \(fedReqAddBody :: ConversationUpdate) -> liftIO $ do
      fedReqAddBody.cuOrigUserId @?= qAlice
      fedReqAddBody.cuConvId @?= cid
      -- This remote backend must already have their users in the conversation,
      -- otherwise they should not be receiving the conversation update message
      assertBool "The list of already present users should be non-empty"
        . not
        . null
        $ fedReqAddBody.cuAlreadyPresentUsers
      case fedReqAddBody.cuAction of
        SomeConversationAction SConversationJoinTag _action -> pure ()
        _ -> assertFailure @() "Unexpected update action"
  where
    toOtherMember qid = OtherMember qid Nothing roleNameWireAdmin
    convView cnv usr =
      responseJsonError =<< getConvQualified usr cnv <!! const 200 === statusCode
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
  postOtrMessage I.id alice ac conv m1 !!! do
    const 412 === statusCode
    assertMismatch [(eve, Set.singleton ec)] [] []
  -- Complete
  WS.bracketR2 c bob eve $ \(wsB, wsE) -> do
    let m2 = [(bob, bc, toBase64Text "ciphertext2"), (eve, ec, toBase64Text "ciphertext2")]
    postOtrMessage I.id alice ac conv m2 !!! do
      const 201 === statusCode
      assertMismatch [] [] []
    void . liftIO $ WS.assertMatch t wsB (wsAssertOtr qconv qalice ac bc (toBase64Text "ciphertext2"))
    void . liftIO $ WS.assertMatch t wsE (wsAssertOtr qconv qalice ac ec (toBase64Text "ciphertext2"))
  -- Redundant self
  WS.bracketR3 c alice bob eve $ \(wsA, wsB, wsE) -> do
    let m3 = [(alice, ac, toBase64Text "ciphertext3"), (bob, bc, toBase64Text "ciphertext3"), (eve, ec, toBase64Text "ciphertext3")]
    postOtrMessage I.id alice ac conv m3 !!! do
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
    postOtrMessage I.id alice ac conv m4 !!! do
      const 201 === statusCode
      assertMismatch [] [] [(eve, Set.singleton ec)]
    void . liftIO $ WS.assertMatch t wsB (wsAssertOtr qconv qalice ac bc (toBase64Text "ciphertext4"))
    -- Eve should not get it
    assertNoMsg wsE (wsAssertOtr qconv qalice ac ec (toBase64Text "ciphertext4"))
  -- Deleted eve & redundant self
  WS.bracketR3 c alice bob eve $ \(wsA, wsB, wsE) -> do
    let m5 = [(bob, bc, toBase64Text "ciphertext5"), (eve, ec, toBase64Text "ciphertext5"), (alice, ac, toBase64Text "ciphertext5")]
    postOtrMessage I.id alice ac conv m5 !!! do
      const 201 === statusCode
      assertMismatch [] [(alice, Set.singleton ac)] [(eve, Set.singleton ec)]
    void . liftIO $ WS.assertMatch t wsB (wsAssertOtr qconv qalice ac bc (toBase64Text "ciphertext5"))
    -- Neither Alice nor Eve should get it
    assertNoMsg wsA (wsAssertOtr qconv qalice ac ac (toBase64Text "ciphertext5"))
    assertNoMsg wsE (wsAssertOtr qconv qalice ac ec (toBase64Text "ciphertext5"))
  -- Missing Bob, deleted eve & redundant self
  let m6 = [(eve, ec, toBase64Text "ciphertext6"), (alice, ac, toBase64Text "ciphertext6")]
  postOtrMessage I.id alice ac conv m6 !!! do
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
      postOtrMessage I.id alice ac conv m7 !!! do
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
    postOtrMessage I.id alice ac conv m <!! do
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
  postProtoOtrMessage alice (ClientId 0x172618352518396) conv m
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
    postOtrMessage I.id alice ac conversationWithAllButChad msgToAllIncludingChad !!! const 201 === statusCode
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
        postOtrMessage I.id sender senderClient conv msgToAllClients !!! do
          const 201 === statusCode
          assertMismatch [] [] []

  let checkSendWitMissingClientsShouldFail =
        postOtrMessage I.id sender senderClient conv msgMissingClients !!! do
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
  postOtrMessage I.id alice ac conv msgMissingChadAndEve
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
  postOtrMessage' (Just [bob]) I.id alice ac conv msgMissingChadAndEve
    !!! const 201 === statusCode
  -- Let's make sure that protobuf works too, when specified in the body only
  postProtoOtrMessage' (Just [bob]) I.id alice ac conv m'
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
      Nothing
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
      assertMismatchQualified mempty expectedMissing mempty mempty mempty
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
      Nothing
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
          getUserClients <- getRequestBody @GetUserClients
          let lookupClients uid
                | uid == deeRemoteUnqualified = Just (uid, Set.fromList [PubClient deeClient Nothing])
                | uid == nonMemberRemoteUnqualified = Just (uid, Set.fromList [PubClient nonMemberRemoteClient Nothing])
                | otherwise = Nothing
          mockReply $ UserMap . Map.fromList $ mapMaybe lookupClients getUserClients.users
        galleyMock = "on-message-sent" ~> EmptyResponse

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
      assertMismatchQualified mempty mempty expectedRedundant expectedDeleted mempty
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
      Nothing
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
      assertMismatchQualified mempty mempty mempty mempty mempty
    let encodedTextForChad = toBase64Text "text-for-chad"
        encodedData = toBase64Text "data"
    WS.assertMatch_ t wsChad (wsAssertOtr' encodedData convId aliceOwningDomain aliceClient chadClient encodedTextForChad)
    WS.assertNoEvent (1 # Second) [wsBob]

  -- Another way to ignore all is to report nobody
  WS.bracketR2 cannon bobUnqualified chadUnqualified $ \(wsBob, wsChad) -> do
    (resp2, _requests) <- postProteusMessageQualifiedWithMockFederator aliceUnqualified aliceClient convId message "data" (Message.MismatchReportOnly mempty) mock
    pure resp2 !!! do
      const 201 === statusCode
      assertMismatchQualified mempty mempty mempty mempty mempty
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
      assertMismatchQualified mempty mempty mempty mempty mempty
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
      assertMismatchQualified mempty expectedMissing mempty mempty mempty

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
      assertMismatchQualified mempty expectedMissing mempty mempty mempty
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
      Nothing
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

    pure resp2 !!! do
      const 201 === statusCode

    liftIO $ do
      let encodedTextForBob = toBase64Text "text-for-bob"
          encodedTextForChad = toBase64Text "text-for-chad"
          encodedData = toBase64Text "data"
      WS.assertMatch_ t wsBob (wsAssertOtr' encodedData convId aliceOwningDomain aliceClient bobClient encodedTextForBob)
      WS.assertMatch_ t wsChad (wsAssertOtr' encodedData convId aliceOwningDomain aliceClient chadClient encodedTextForChad)

postMessageQualifiedFailedToSendFetchingClients :: TestM ()
postMessageQualifiedFailedToSendFetchingClients = do
  (aliceQualified, aliceClient) <- randomUserWithClientQualified (head someLastPrekeys)
  bobId <- randomId
  bobClient <- liftIO $ generate arbitrary
  deeId <- randomId
  let remoteDomain = Domain "offline-unfortunately.example.com"
      bobRemote = Qualified bobId remoteDomain
      deeRemote = Qualified deeId remoteDomain
      aliceUnqualified = qUnqualified aliceQualified
  connectWithRemoteUser aliceUnqualified bobRemote
  connectWithRemoteUser aliceUnqualified deeRemote

  resp <-
    postConvWithRemoteUsers
      aliceUnqualified
      Nothing
      defNewProteusConv {newConvQualifiedUsers = [bobRemote, deeRemote]}

  owningDomain <- viewFederationDomain
  let convId = (`Qualified` owningDomain) . decodeConvId $ resp
  -- dee is part of the conversation but missing from the recipient list of the request,
  -- and we cannot fetch or verify their clients because the remote backend is unreachable.
  -- Therefore we expect dee to be part of the `failed_to_send` list in the `resp2` below,
  -- where a message is sent.
  let message = [(bobRemote, bobClient, "text-for-bob")]
  let mock =
        (guardRPC "get-user-clients" *> throw (MockErrorResponse HTTP.status503 "Down for maintenance."))
          <|> ("on-message-sent" ~> EmptyResponse)

  (resp2, _requests) <- postProteusMessageQualifiedWithMockFederator aliceUnqualified aliceClient convId message "data" Message.MismatchReportAll mock

  let failedToSend = QualifiedUserClients $ Map.fromList [(qDomain deeRemote, Map.fromList [(qUnqualified deeRemote, mempty)])]
      failedToConfirm =
        QualifiedUserClients $
          Map.fromList
            [ (qDomain bobRemote, Map.fromList [(qUnqualified bobRemote, Set.fromList [bobClient]), (qUnqualified deeRemote, mempty)])
            ]

  pure resp2 !!! do
    const 201 === statusCode
    assertMismatchQualified failedToSend mempty mempty mempty failedToConfirm

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
            Message.mssFailedToSend = mempty,
            Message.mssFailedToConfirmClients = mempty
          }
      message = [(bobOwningDomain, bobClient, "text-for-bob"), (deeRemote, deeClient, "text-for-dee")]
      mock = "send-message" ~> MessageSendResponse (Right mss)
  (resp2, _requests) <-
    postProteusMessageQualifiedWithMockFederator aliceUnqualified aliceClient convId message "data" Message.MismatchReportAll mock

  pure resp2 !!! do
    const 201 === statusCode
    assertMismatchQualified mempty mempty redundant mempty mempty

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
  cCode <- (.code) . decodeConvCodeEvent <$> postConvCode alice convId

  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob
  getJoinCodeConv bob (conversationKey cCode) (conversationCode cCode) !!! do
    const (Right (ConversationCoverView convId (Just convName) False)) === responseJsonEither

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
  cCode <- (.code) . decodeConvCodeEvent <$> postConvCode owner convId

  let checkFeatureStatus fstatus =
        Util.getTeamFeatureFlagWithGalley @Public.GuestLinksConfig galley owner teamId !!! do
          const 200 === statusCode
          const (Right (Public.withStatus fstatus Public.LockStatusUnlocked Public.GuestLinksConfig Public.FeatureTTLUnlimited)) === responseJsonEither

  -- guest can join if guest link feature is enabled
  checkFeatureStatus Public.FeatureStatusEnabled
  getJoinCodeConv eve (conversationKey cCode) (conversationCode cCode) !!! do
    const (Right (ConversationCoverView convId (Just convName) False)) === responseJsonEither
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
    const (Right (ConversationCoverView convId (Just convName) False)) === responseJsonEither
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
  cCode <- (.code) . decodeConvCodeEvent <$> postConvCode owner convId

  -- works by default
  getJoinCodeConv userNotInTeam (conversationKey cCode) (conversationCode cCode) !!! do
    const (Right (ConversationCoverView convId (Just convName) False)) === responseJsonEither
    const 200 === statusCode

  -- for non-team conversations it still works if status is disabled for the team but not server wide
  let tfStatus = Public.WithStatusNoLock Public.FeatureStatusDisabled Public.GuestLinksConfig Public.FeatureTTLUnlimited
  TeamFeatures.putTeamFeatureFlagWithGalley @Public.GuestLinksConfig galley owner teamId tfStatus !!! do
    const 200 === statusCode

  getJoinCodeConv userNotInTeam (conversationKey cCode) (conversationCode cCode) !!! do
    const (Right (ConversationCoverView convId (Just convName) False)) === responseJsonEither
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
  info <- decodeConvCodeEvent <$> postConvCode alice conv
  let cCode = info.code
  liftIO $ info.hasPassword @?= False
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

postJoinCodeConvWithPassword :: TestM ()
postJoinCodeConvWithPassword = do
  alice <- randomUser
  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob
  Right accessRoles <- liftIO $ genAccessRolesV2 [TeamMemberAccessRole, NonTeamMemberAccessRole] [GuestAccessRole]
  conv <- decodeConvId <$> postConv alice [] (Just "gossip") [CodeAccess] (Just accessRoles) Nothing
  let pw = plainTextPassword8Unsafe "password"
  info <- decodeConvCodeEvent <$> postConvCode' (Just pw) alice conv
  liftIO $ info.hasPassword @?= True
  let cCode = info.code
  getJoinCodeConv bob (conversationKey cCode) (conversationCode cCode) !!! do
    const (Right (ConversationCoverView conv (Just "gossip") True)) === responseJsonEither
    const 200 === statusCode
  -- join without password should fail
  postJoinCodeConv' Nothing bob cCode !!! const 403 === statusCode
  -- join with wrong password should fail
  postJoinCodeConv' (Just (plainTextPassword8Unsafe "wrong-password")) bob cCode !!! const 403 === statusCode
  -- join with correct password should succeed
  postJoinCodeConv' (Just pw) bob cCode !!! const 200 === statusCode

postCodeWithoutPasswordExistsWithoutPasswordRequested :: TestM ()
postCodeWithoutPasswordExistsWithoutPasswordRequested = do
  alice <- randomUser
  conv <- decodeConvId <$> postConv alice [] (Just "gossip") [CodeAccess] (Just (Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole, GuestAccessRole])) Nothing
  info1 <- decodeConvCodeEvent <$> (postConvCode alice conv <!! const 201 === statusCode)
  info2 <- decodeConvCode <$> (postConvCode alice conv <!! const 200 === statusCode)
  liftIO $ info1 @?= info2

postCodeWithPasswordExistsWithoutPasswordRequested :: TestM ()
postCodeWithPasswordExistsWithoutPasswordRequested = do
  alice <- randomUser
  conv <- decodeConvId <$> postConv alice [] (Just "gossip") [CodeAccess] (Just (Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole, GuestAccessRole])) Nothing
  postConvCode' (Just (plainTextPassword8Unsafe "password")) alice conv !!! const 201 === statusCode
  postConvCode alice conv !!! const 409 === statusCode

postCodeWithoutPasswordExistsWithPasswordRequested :: TestM ()
postCodeWithoutPasswordExistsWithPasswordRequested = do
  alice <- randomUser
  conv <- decodeConvId <$> postConv alice [] (Just "gossip") [CodeAccess] (Just (Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole, GuestAccessRole])) Nothing
  postConvCode alice conv !!! const 201 === statusCode
  postConvCode' (Just (plainTextPassword8Unsafe "password")) alice conv !!! const 409 === statusCode

postCodeWithPasswordExistsWithPasswordRequested :: TestM ()
postCodeWithPasswordExistsWithPasswordRequested = do
  alice <- randomUser
  conv <- decodeConvId <$> postConv alice [] (Just "gossip") [CodeAccess] (Just (Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole, GuestAccessRole])) Nothing
  postConvCode' (Just (plainTextPassword8Unsafe "password")) alice conv !!! const 201 === statusCode
  postConvCode' (Just (plainTextPassword8Unsafe "some-other-password")) alice conv !!! const 409 === statusCode
  postConvCode' (Just (plainTextPassword8Unsafe "password")) alice conv !!! const 409 === statusCode

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
  c1 <- (.code) . decodeConvCodeEvent <$> (postConvCode alice conv <!! const 201 === statusCode)
  postConvCodeCheck c1 !!! const 200 === statusCode
  c1' <- (.code) . decodeConvCode <$> (getConvCode alice conv <!! const 200 === statusCode)
  liftIO $ assertEqual "c1 c1' codes should match" c1 c1'
  postConvCode alice conv !!! const 200 === statusCode
  c2 <- (.code) . decodeConvCode <$> (postConvCode alice conv <!! const 200 === statusCode)
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
  assertTeamUpdate "team member (bob) join" tid 2 [alice]
  refreshIndex
  dave <- view Teams.userId <$> addUserToTeam alice tid
  assertTeamUpdate "team member (dave) join" tid 3 [alice]
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
  j <- (.code) . decodeConvCodeEvent <$> postConvCode alice conv
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
        wsAssertMemberLeave qconv qalice (pure qeve) EdReasonRemoved
      WS.assertMatchN_ (5 # Second) [wsA, wsB, wsE, wsM] $
        wsAssertMemberLeave qconv qalice (pure qmallory) EdReasonRemoved
    -- joining (for mallory) is no longer possible
    postJoinCodeConv mallory j !!! const 403 === statusCode
    -- team members (dave) can still join
    postJoinCodeConv dave j !!! const 200 === statusCode

testTeamMemberCantJoinViaGuestLinkIfAccessRoleRemoved :: TestM ()
testTeamMemberCantJoinViaGuestLinkIfAccessRoleRemoved = do
  -- given alice, bob, charlie and dee are in a team
  (alice, tid, [bob, charlie, dee]) <- createBindingTeamWithNMembers 3

  -- and given alice and bob are in a team conversation and alice created a guest link
  let accessRoles = Set.fromList [TeamMemberAccessRole, GuestAccessRole, ServiceAccessRole]
  qconvId <- decodeQualifiedConvId <$> postTeamConv tid alice [bob] (Just "chit chat") [CodeAccess] (Just accessRoles) Nothing
  cCode <- (.code) . decodeConvCodeEvent <$> postConvCode alice (qUnqualified qconvId)

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
      (Just $ C.cnvName expected)
      (C.cnvName <$> actual)
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
          ConversationUpdate
            { cuTime = now,
              cuOrigUserId = qChad,
              cuConvId = conv,
              cuAlreadyPresentUsers = [],
              cuAction = SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qAlice) roleNameWireMember)
            }
    void $ runFedClient @"on-conversation-updated" fedGalleyClient chadDomain cu

  remoteDee <- randomId
  let deeDomain = Domain "dee.example.com"
      qDee = Qualified remoteDee deeDomain
  connectWithRemoteUser alice qDee
  replicateM_ 31 $ do
    conv <- randomId
    let cu =
          ConversationUpdate
            { cuTime = now,
              cuOrigUserId = qDee,
              cuConvId = conv,
              cuAlreadyPresentUsers = [],
              cuAction = SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qAlice) roleNameWireMember)
            }
    void $ runFedClient @"on-conversation-updated" fedGalleyClient deeDomain cu

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
          ConversationUpdate
            { cuTime = now,
              cuOrigUserId = qChad,
              cuConvId = conv,
              cuAlreadyPresentUsers = [],
              cuAction = SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qAlice) roleNameWireMember)
            }
    void $ runFedClient @"on-conversation-updated" fedGalleyClient chadDomain cu

  remoteDee <- randomId
  let deeDomain = Domain "dee.example.com"
      qDee = Qualified remoteDee deeDomain
  connectWithRemoteUser alice qDee

  -- The 4th and last page will end with this domain
  replicateM_ 16 $ do
    conv <- randomId
    let cu =
          ConversationUpdate
            { cuTime = now,
              cuOrigUserId = qDee,
              cuConvId = conv,
              cuAlreadyPresentUsers = [],
              cuAction = SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qAlice) roleNameWireMember)
            }
    void $ runFedClient @"on-conversation-updated" fedGalleyClient deeDomain cu

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
  postConvQualified alice Nothing defNewProteusConv {newConvQualifiedUsers = [bob, jane]} !!! do
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
  postConvQualified alice Nothing defNewProteusConv {newConvQualifiedUsers = bob : others} !!! do
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
  postConvQualified alice Nothing defNewProteusConv {newConvQualifiedUsers = [bob, jane]} !!! do
    const 403 === statusCode
    const (Just "not-connected") === fmap label . responseJsonUnsafe

postConvQualifiedNoConnection :: TestM ()
postConvQualifiedNoConnection = do
  alice <- randomUser
  bob <- flip Qualified (Domain "far-away.example.com") <$> randomId
  void $ withTempMockFederator' getNotFullyConnectedBackendsMock $ do
    postConvQualified alice Nothing defNewProteusConv {newConvQualifiedUsers = [bob]}
      !!! const 403 === statusCode

postTeamConvQualifiedNoConnection :: TestM ()
postTeamConvQualifiedNoConnection = do
  (tid, alice, _) <- createBindingTeamWithQualifiedMembers 1
  bob <- randomQualifiedId (Domain "bob.example.com")
  charlie <- randomQualifiedUser
  void $ withTempMockFederator' getNotFullyConnectedBackendsMock $ do
    postConvQualified
      (qUnqualified alice)
      Nothing
      defNewProteusConv
        { newConvQualifiedUsers = [bob],
          newConvTeam = Just (ConvTeamInfo tid)
        }
      !!! const 403 === statusCode
    postConvQualified
      (qUnqualified alice)
      Nothing
      defNewProteusConv
        { newConvQualifiedUsers = [charlie],
          newConvTeam = Just (ConvTeamInfo tid)
        }
      !!! const 403 === statusCode

postConvQualifiedNonExistentDomain :: TestM ()
postConvQualifiedNonExistentDomain = do
  let remoteDomain = Domain "non-existent.example.com"
  alice <- randomUser
  uBob <- randomId
  let bob = Qualified uBob remoteDomain
  connectWithRemoteUser alice bob
  let mock = "get-not-fully-connected-backends" ~> NonConnectedBackends mempty
  void $
    withTempMockFederator'
      mock
      ( do
          postConvQualified
            alice
            Nothing
            defNewProteusConv {newConvQualifiedUsers = [bob]}
            !!! do const 533 === statusCode
      )

postConvQualifiedFederationNotEnabled :: TestM ()
postConvQualifiedFederationNotEnabled = do
  alice <- randomUser
  let domain = Domain "some-remote-backend.example.com"
  bob <- flip Qualified domain <$> randomId
  connectWithRemoteUser alice bob
  let federatorNotConfigured o =
        o
          & federator .~ Nothing
          & rabbitmq .~ Nothing
  withSettingsOverrides federatorNotConfigured $ do
    g <- viewGalley
    unreachable :: UnreachableBackends <-
      responseJsonError
        =<< postConvHelper g alice [bob] <!! do
          const 533 === statusCode
    liftIO $ unreachable.backends @?= [domain]

-- like postConvQualified
-- FUTUREWORK: figure out how to use functions in the TestM monad inside withSettingsOverrides and remove this duplication
postConvHelper :: MonadHttp m => (Request -> Request) -> UserId -> [Qualified UserId] -> m ResponseLBS
postConvHelper g zusr newUsers = do
  let conv = NewConv [] newUsers (checked "gossip") (Set.fromList []) Nothing Nothing Nothing Nothing roleNameWireAdmin BaseProtocolProteusTag
  post $ g . path "/conversations" . zUser zusr . zConn "conn" . zType "access" . json conv

postSelfConvOk :: TestM ()
postSelfConvOk = do
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice
  m <- postSelfConv alice <!! const 200 === statusCode
  n <- postSelfConv alice <!! const 200 === statusCode
  mId <- assertConv m SelfConv (Just alice) qalice [] Nothing Nothing
  nId <- assertConv n SelfConv (Just alice) qalice [] Nothing Nothing
  liftIO $ mId @=? nId

postO2OConvOk :: TestM ()
postO2OConvOk = do
  (alice, qalice) <- randomUserTuple
  (bob, qbob) <- randomUserTuple
  connectUsers alice (singleton bob)
  a <- postO2OConv alice bob Nothing <!! const 200 === statusCode
  c <- postO2OConv alice bob Nothing <!! const 200 === statusCode
  aId <- assertConv a One2OneConv (Just alice) qalice [qbob] Nothing Nothing
  cId <- assertConv c One2OneConv (Just alice) qalice [qbob] Nothing Nothing
  liftIO $ aId @=? cId

postConvO2OFailWithSelf :: TestM ()
postConvO2OFailWithSelf = do
  g <- viewGalley
  alice <- randomUser
  let inv = NewConv [alice] [] Nothing mempty Nothing Nothing Nothing Nothing roleNameWireAdmin BaseProtocolProteusTag
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
  mId <- assertConv m ConnectConv (Just alice) qalice [] (Just "Alice") Nothing
  nId <- assertConv n ConnectConv (Just alice) qalice [] (Just "Alice") Nothing
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
    const (Just One2OneConv) === fmap C.cnvType . responseJsonUnsafe
  getConvQualified bob qcnv !!! do
    const 200 === statusCode
    const (Just One2OneConv) === fmap C.cnvType . responseJsonUnsafe

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
  acId <- assertConv ac ConnectConv (Just alice) qalice [] (Just "A") Nothing
  bc <-
    postConnectConv bob alice "B" "b" Nothing
      <!! const 200 === statusCode
  -- The connect conversation was simply accepted, thus the
  -- conversation name and message sent in Bob's request ignored.
  bcId <- assertConv bc One2OneConv (Just alice) qbob [qalice] (Just "A") Nothing
  liftIO $ acId @=? bcId

postRepeatConnectConvCancel :: TestM ()
postRepeatConnectConvCancel = do
  alice <- randomUser
  bob <- randomUser
  -- Alice wants to connect
  rsp1 <- postConnectConv alice bob "A" "a" Nothing <!! const 201 === statusCode
  let cnv = responseJsonUnsafeWithMsg "conversation" rsp1
  liftIO $ do
    ConnectConv @=? C.cnvType cnv
    Just "A" @=? C.cnvName cnv
    [] @=? cmOthers (C.cnvMembers cnv)
    privateAccess @=? C.cnvAccess cnv
  -- Alice blocks / cancels
  cancel alice cnv
  -- Alice makes another connect attempt
  rsp2 <- postConnectConv alice bob "A2" "a2" Nothing <!! const 200 === statusCode
  let cnv2 = responseJsonUnsafeWithMsg "conversation" rsp2
  liftIO $ do
    ConnectConv @=? C.cnvType cnv2
    Just "A2" @=? C.cnvName cnv2
    [] @=? cmOthers (C.cnvMembers cnv2)
    privateAccess @=? C.cnvAccess cnv2
  -- Alice blocks / cancels again
  cancel alice cnv
  -- Now Bob attempts to connect
  rsp3 <- postConnectConv bob alice "B" "b" Nothing <!! const 200 === statusCode
  let cnv3 = responseJsonUnsafeWithMsg "conversation" rsp3
  liftIO $ do
    ConnectConv @=? C.cnvType cnv3
    Just "B" @=? C.cnvName cnv3
    privateAccess @=? C.cnvAccess cnv3
  -- Bob accepting is a no-op, since he is already a member
  let qconvId = C.cnvQualifiedId cnv
  let convId = qUnqualified qconvId
  putConvAccept bob convId !!! const 200 === statusCode
  cnvX <- responseJsonUnsafeWithMsg "conversation" <$> getConvQualified bob qconvId
  liftIO $ do
    ConnectConv @=? C.cnvType cnvX
    Just "B" @=? C.cnvName cnvX
    privateAccess @=? C.cnvAccess cnvX
  -- Alice accepts, finally turning it into a 1-1
  putConvAccept alice convId !!! const 200 === statusCode
  cnv4 <- responseJsonUnsafeWithMsg "conversation" <$> getConvQualified alice qconvId
  liftIO $ do
    One2OneConv @=? C.cnvType cnv4
    Just "B" @=? C.cnvName cnv4
    privateAccess @=? C.cnvAccess cnv4
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
        Nothing
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
          (Just alice)
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

testGetQualifiedLocalConv :: TestM ()
testGetQualifiedLocalConv = do
  alice <- randomUser
  convId <- decodeQualifiedConvId <$> postConv alice [] (Just "gossip") [] Nothing Nothing
  conv :: Conversation <- fmap responseJsonUnsafe $ getConvQualified alice convId <!! const 200 === statusCode
  liftIO $ do
    assertEqual "conversation id" convId (C.cnvQualifiedId conv)
    assertEqual "conversation name" (Just "gossip") (C.cnvName conv)

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
          mockConversation.metadata
          (ConvMembers aliceAsSelfMember mockConversation.members.others)
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
          fmap (.convIds)
            . (decode @GetConversationsRequest . frBody =<<)
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
  let domain = Domain "invalid.example.com"
  let remoteBob = Qualified bobId domain
  convId <- decodeConvId <$> postConv alice [] (Just "remote gossip") [] Nothing Nothing
  localDomain <- viewFederationDomain
  let qconvId = Qualified convId localDomain

  connectWithRemoteUser alice remoteBob

  e :: UnreachableBackends <-
    responseJsonError
      =<< postQualifiedMembers alice (remoteBob :| []) qconvId
        <!! do
          const 533 === statusCode
  liftIO $ e.backends @?= [domain]

-- This test is a safeguard to ensure adding remote members will fail
-- on environments where federation isn't configured (such as our production as of May 2021)
testAddRemoteMemberFederationDisabled :: TestM ()
testAddRemoteMemberFederationDisabled = do
  alice <- randomUser
  remoteBob <- flip Qualified (Domain "some-remote-backend.example.com") <$> randomId
  qconvId <- decodeQualifiedConvId <$> postConv alice [] (Just "remote gossip") [] Nothing Nothing
  connectWithRemoteUser alice remoteBob

  -- federator endpoint not configured is equivalent to federation being disabled
  -- This is the case on staging/production in May 2021.
  let federatorNotConfigured o =
        o
          & federator .~ Nothing
          & rabbitmq .~ Nothing
  withSettingsOverrides federatorNotConfigured $
    postQualifiedMembers alice (remoteBob :| []) qconvId !!! do
      const 400 === statusCode
      const (Right "federation-not-enabled") === fmap label . responseJsonEither

  -- the member is not actually added to the conversation
  conv <- responseJsonError =<< getConvQualified alice qconvId <!! const 200 === statusCode
  liftIO $ map omQualifiedId (cmOthers (cnvMembers conv)) @?= []

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
        Nothing
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
        mockReply (LeaveConversationResponse (Right mempty))
      mockResponses =
        mockedFederatedBrigResponse [(qBob, "Bob")]
          <|> mockedFederatedGalleyResponse

  (resp, fedRequests) <-
    withTempMockFederator' mockResponses $
      deleteMemberQualified alice qAlice qconv
  let leaveRequest :: LeaveConversationRequest =
        fromJust . decode . frBody . Imports.head $
          fedRequests
  liftIO $ do
    statusCode resp @?= 200
    case responseJsonEither resp of
      Left err -> assertFailure err
      Right e -> assertLeaveEvent qconv qAlice [qAlice] e
    leaveRequest.convId @?= conv
    leaveRequest.leaver @?= alice

-- Alice tries to leave a non-existent remote conversation
leaveNonExistentRemoteConv :: TestM ()
leaveNonExistentRemoteConv = do
  alice <- randomQualifiedUser
  let remoteDomain = Domain "faraway.example.com"
  conv <- randomQualifiedId remoteDomain

  let mockResponses = do
        guardComponent Galley
        mockReply $
          LeaveConversationResponse (Left RemoveFromConversationErrorNotFound)

  (resp, fedRequests) <-
    withTempMockFederator' mockResponses $
      responseJsonError
        =<< deleteMemberQualified (qUnqualified alice) alice conv
          <!! const 404 === statusCode
  let leaveRequest :: LeaveConversationRequest =
        fromJust . decode . frBody . Imports.head $
          fedRequests
  liftIO $ do
    fmap label resp @?= Just "no-conversation"
    leaveRequest.convId @?= qUnqualified conv
    leaveRequest.leaver @?= qUnqualified alice

-- Alice tries to leave a conversation of the wrong type
leaveRemoteConvDenied :: TestM ()
leaveRemoteConvDenied = do
  alice <- randomQualifiedUser
  let remoteDomain = Domain "faraway.example.com"
  conv <- randomQualifiedId remoteDomain

  let mockResponses = do
        guardComponent Galley
        mockReply $
          LeaveConversationResponse
            ( Left RemoveFromConversationErrorRemovalNotAllowed
            )

  (resp, fedRequests) <-
    withTempMockFederator' mockResponses $
      responseJsonError
        =<< deleteMemberQualified (qUnqualified alice) alice conv
          <!! const 403 === statusCode
  let leaveRequest :: LeaveConversationRequest =
        fromJust . decode . frBody . Imports.head $
          fedRequests
  liftIO $ do
    fmap label resp @?= Just "action-denied"
    leaveRequest.convId @?= qUnqualified conv
    leaveRequest.leaver @?= qUnqualified alice

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
        ConversationUpdate
          { cuTime = now,
            cuOrigUserId = qbob,
            cuConvId = qUnqualified qconv,
            cuAlreadyPresentUsers = [],
            cuAction =
              SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qalice) roleNameWireMember)
          }
  void $ runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain cu

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
        ConversationUpdate
          { cuTime = now,
            cuOrigUserId = qbob,
            cuConvId = qUnqualified qconv,
            cuAlreadyPresentUsers = [],
            cuAction =
              SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qalice) roleNameWireAdmin)
          }
  void $ runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain cuAddAlice

  -- add another user adam as member
  qadam <- randomQualifiedUser
  let adam = qUnqualified qadam
  connectWithRemoteUser adam qbob
  let cuAddAdam =
        ConversationUpdate
          { cuTime = now,
            cuOrigUserId = qbob,
            cuConvId = qUnqualified qconv,
            cuAlreadyPresentUsers = [],
            cuAction =
              SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qadam) roleNameWireMember)
          }
  void $ runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain cuAddAdam

  let newReceiptMode = ReceiptMode 42
  let action = ConversationReceiptModeUpdate newReceiptMode
  let responseConvUpdate =
        ConversationUpdate
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
    cFedReqBody :: ConversationUpdateRequest <- assertRight $ parseFedRequest cFedReq
    liftIO $ do
      cFedReqBody.user @?= alice
      cFedReqBody.convId @?= qUnqualified qconv
      cFedReqBody.action @?= SomeConversationAction (sing @'ConversationReceiptModeUpdateTag) action

    WS.assertMatch_ (5 # Second) wsAdam $ \n -> do
      liftIO $ wsAssertConvReceiptModeUpdate qconv qalice newReceiptMode n

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
  qconvA2 <- decodeQualifiedConvId <$> postConvWithRemoteUsers alice' Nothing defNewProteusConv {newConvQualifiedUsers = [alexDel, amy, berta, dwight]}
  qconvA3 <- decodeQualifiedConvId <$> postConv alice' [amy'] (Just "gossip3") [] Nothing Nothing
  qconvA4 <- decodeQualifiedConvId <$> postConvWithRemoteUsers alice' Nothing defNewProteusConv {newConvQualifiedUsers = [alexDel, bart, carl]}
  convB1 <- randomId -- a remote conversation at 'bDomain' that Alice, AlexDel and Bart will be in
  convB2 <- randomId -- a remote conversation at 'bDomain' that AlexDel and Bart will be in
  convC1 <- randomId -- a remote conversation at 'cDomain' that AlexDel and Carl will be in
  convD1 <- randomId -- a remote conversation at 'cDomain' that AlexDel and Dory will be in
  now <- liftIO getCurrentTime
  fedGalleyClient <- view tsFedGalleyClient
  let nc cid creator quids =
        ConversationCreated
          { time = now,
            origUserId = qUnqualified creator,
            cnvId = cid,
            cnvType = RegularConv,
            cnvAccess = [],
            cnvAccessRoles = Set.fromList [],
            cnvName = Just "gossip4",
            nonCreatorMembers = Set.fromList $ createOtherMember <$> quids,
            messageTimer = Nothing,
            receiptMode = Nothing,
            protocol = ProtocolProteus
          }
  void $ runFedClient @"on-conversation-created" fedGalleyClient bDomain $ nc convB1 bart [alice, alexDel]
  void $ runFedClient @"on-conversation-created" fedGalleyClient bDomain $ nc convB2 bart [alexDel]
  void $ runFedClient @"on-conversation-created" fedGalleyClient cDomain $ nc convC1 carl [alexDel]
  void $ runFedClient @"on-conversation-created" fedGalleyClient dDomain $ nc convD1 dory [alexDel]

  WS.bracketR3 c alice' alexDel' amy' $ \(wsAlice, wsAlexDel, wsAmy) -> do
    let handler = do
          d <- frTargetDomain <$> getRequest
          asum
            [ do
                guard (d == dDomain)
                throw (DiscoveryFailureSrvNotAvailable "dDomain"),
              do
                guard (d `elem` [bDomain, cDomain])
                "leave-conversation" ~> LeaveConversationResponse (Right mempty)
            ]
    (_, fedRequests) <-
      withTempMockFederator' handler $
        deleteUser alexDel' !!! const 200 === statusCode

    liftIO $ do
      assertEqual ("expect exactly 4 federated requests in : " <> show fedRequests) 4 (length fedRequests)

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
  (bob, convId) <- generateRemoteAndConvId shouldBeLocal alice

  do
    let req = UpsertOne2OneConversationRequest alice bob actor desired convId
    iUpsertOne2OneConversation req !!! statusCode === const 200

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
                  GetConversationsRequest
                    { userId = tUnqualified bob,
                      convIds = [qUnqualified convId]
                    }
              pure
                . fmap (map omQualifiedId . (.members.others))
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
              withTempMockFederator' (mockReply (GetConversationsResponse [rconv])) $
                getConvQualified (tUnqualified alice) convId
            pure $ statusCode resp == 200
          liftIO $ found @?= ((actor, desired) == (LocalActor, Included))
      )
