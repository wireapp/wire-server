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

module API.MLS (tests) where

import API.MLS.Mocks
import API.MLS.Util
import API.Util
import Bilge hiding (empty, head)
import Bilge.Assert
import Cassandra hiding (Set)
import Control.Lens (view)
import Control.Lens.Extras
import Control.Monad.State qualified as State
import Data.Aeson qualified as Aeson
import Data.Domain
import Data.Id
import Data.Json.Util hiding ((#))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List1 hiding (head)
import Data.Map qualified as Map
import Data.Qualified
import Data.Range
import Data.Set qualified as Set
import Data.Singletons
import Data.Text qualified as T
import Data.Time
import Federator.MockServer hiding (withTempMockFederator)
import Imports
import Network.Wai.Utilities.Error qualified as Wai
import Test.Tasty
import Test.Tasty.Cannon (TimeoutUnit (Second), (#))
import Test.Tasty.Cannon qualified as WS
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Wire.API.Conversation
import Wire.API.Conversation.Action
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Federation.API.Galley
import Wire.API.MLS.Credential
import Wire.API.MLS.Serialisation
import Wire.API.MLS.SubConversation
import Wire.API.Message
import Wire.API.Routes.MultiTablePaging
import Wire.API.Routes.Version

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "MLS"
    [ testGroup
        "Message"
        [ test s "sender must be part of conversation" testSenderNotInConversation,
          test s "send other user's commit" testSendAnotherUsersCommit
        ],
      testGroup
        "Welcome"
        [ test s "post a remote MLS welcome message" sendRemoteMLSWelcome
        ],
      testGroup
        "Creation"
        [ test s "fail to create MLS conversation" postMLSConvFail,
          test s "create MLS conversation" postMLSConvOk
        ],
      testGroup
        "Deletion"
        [ test s "delete an MLS conversation" testDeleteMLSConv
        ],
      testGroup
        "Commit"
        [ test s "add user (not connected)" testAddUserNotConnected,
          test s "add client of existing user" testAddClientPartial,
          test s "add user with some non-MLS clients" testAddUserWithProteusClients,
          test s "add remote users to a conversation (some unreachable)" testAddRemotesSomeUnreachable,
          test s "return error when commit is locked" testCommitLock,
          test s "post commit that references an unknown proposal" testUnknownProposalRefCommit
        ],
      testGroup
        "External commit"
        [ test s "non-member attempts to join a conversation" testExternalCommitNotMember,
          test s "join a conversation with the same client" testExternalCommitSameClient,
          test s "join a conversation with a new client" testExternalCommitNewClient,
          test s "join a conversation with a new client and resend backend proposals" testExternalCommitNewClientResendBackendProposal
        ],
      testGroup
        "Application Message"
        [ testGroup
            "Local Sender/Local Conversation"
            [ test s "send application message" testAppMessage,
              test s "another participant sends an application message" testAppMessage2,
              test s "send message, remote users are unreachable" testAppMessageUnreachable
            ],
          testGroup
            "Local Sender/Remote Conversation"
            [ test s "send application message" testLocalToRemote,
              test s "non-member sends application message" testLocalToRemoteNonMember
            ],
          testGroup
            "Remote Sender/Local Conversation"
            [ test s "POST /federation/send-mls-message" testRemoteToLocal,
              test s "POST /federation/send-mls-message, remote user is not a conversation member" testRemoteNonMemberToLocal,
              test s "POST /federation/send-mls-message, remote user sends to wrong conversation" testRemoteToLocalWrongConversation
            ]
        ],
      testGroup
        "Proposal"
        [ test s "add a new client to a non-existing conversation" propNonExistingConv
        ],
      testGroup
        "External Add Proposal"
        [ test s "member adds new client" testExternalAddProposal,
          test s "non-admin commits external add proposal" testExternalAddProposalNonAdminCommit,
          test s "non-member adds new client" testExternalAddProposalWrongUser,
          test s "member adds unknown new client" testExternalAddProposalWrongClient
        ],
      testGroup
        "Backend-side External Remove Proposals"
        [ test s "local conversation, local user deleted" testBackendRemoveProposalLocalConvLocalUser,
          test s "local conversation, recreate client" testBackendRemoveProposalRecreateClient,
          test s "local conversation, remote user deleted" testBackendRemoveProposalLocalConvRemoteUser,
          test s "local conversation, creator leaving" testBackendRemoveProposalLocalConvLocalLeaverCreator,
          test s "local conversation, local committer leaving" testBackendRemoveProposalLocalConvLocalLeaverCommitter,
          test s "local conversation, remote user leaving" testBackendRemoveProposalLocalConvRemoteLeaver,
          test s "local conversation, local client deleted" testBackendRemoveProposalLocalConvLocalClient,
          test s "local conversation, remote client deleted" testBackendRemoveProposalLocalConvRemoteClient
        ],
      testGroup
        "Protocol mismatch"
        [ test s "send a commit to a proteus conversation" testAddUsersToProteus,
          test s "add users bypassing MLS" testAddUsersDirectly,
          test s "remove users bypassing MLS" testRemoveUsersDirectly,
          test s "send proteus message to an MLS conversation" testProteusMessage
        ],
      testGroup
        "GroupInfo"
        [ test s "get group info for a local conversation" testGetGroupInfoOfLocalConv,
          test s "get group info for a remote conversation" testGetGroupInfoOfRemoteConv,
          test s "get group info for a remote user" testFederatedGetGroupInfo
        ],
      testGroup
        "CommitBundle"
        [ test s "add user with a commit bundle" testAddUserWithBundle,
          test s "add user with a commit bundle to a remote conversation" testAddUserToRemoteConvWithBundle
        ],
      testGroup
        "Self conversation"
        [ test s "do not list a self conversation below v3" $ testSelfConversationList True,
          test s "list a self conversation automatically from v3" $ testSelfConversationList False,
          test s "listing conversations without MLS configured" testSelfConversationMLSNotConfigured,
          test s "attempt to add another user to a conversation fails" testSelfConversationOtherUser,
          test s "attempt to leave fails" testSelfConversationLeave
        ],
      testGroup
        "MLS disabled"
        [ test s "cannot create MLS conversations" postMLSConvDisabled,
          test s "cannot send an MLS message" postMLSMessageDisabled,
          test s "cannot send a commit bundle" postMLSBundleDisabled,
          test s "cannot get group info" getGroupInfoDisabled,
          test s "cannot delete a subconversation" deleteSubConversationDisabled
        ],
      testGroup
        "SubConversation"
        [ testGroup
            "Local Sender/Local Subconversation"
            [ test s "rejoin a subconversation with the same client" testExternalCommitSameClientSubConv,
              test s "join subconversation with a client that is not in the parent conv" testJoinSubNonMemberClient,
              test s "fail to add another client to a subconversation via internal commit" testAddClientSubConvFailure,
              test s "remove another client from a subconversation" testRemoveClientSubConv,
              test s "send an application message in a subconversation" testSendMessageSubConv,
              test s "reset a subconversation and assert no leftover proposals" testJoinDeletedSubConvWithRemoval,
              test s "fail to reset a subconversation with wrong epoch" testDeleteSubConvStale,
              test s "last to leave a subconversation" testLastLeaverSubConv,
              test s "leave a subconversation as a non-member" testLeaveSubConvNonMember,
              test s "remove user from parent conversation" testRemoveUserParent,
              test s "remove creator from parent conversation" testRemoveCreatorParent
            ],
          testGroup
            "Local Sender/Remote Subconversation"
            [ test s "get subconversation of remote conversation - member" (testGetRemoteSubConv True),
              test s "get subconversation of remote conversation - not member" (testGetRemoteSubConv False),
              test s "join remote subconversation" testJoinRemoteSubConv,
              test s "backends are notified about subconvs when a user joins" testRemoteSubConvNotificationWhenUserJoins,
              test s "reset a subconversation - member" (testDeleteRemoteSubConv True),
              test s "reset a subconversation - not member" (testDeleteRemoteSubConv False),
              test s "leave a remote subconversation" testLeaveRemoteSubConv
            ],
          testGroup
            "Remote Sender/Local SubConversation"
            [ test s "get subconversation as a remote member" (testRemoteMemberGetSubConv True),
              test s "get subconversation as a remote non-member" (testRemoteMemberGetSubConv False)
            ],
          testGroup
            "Remote Sender/Remote SubConversation"
            [ test s "on-mls-message-sent in subconversation" testRemoteToRemoteInSub
            ]
        ]
    ]

postMLSConvFail :: TestM ()
postMLSConvFail = do
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice
  let aliceClient = ClientId 0
  bob <- randomUser
  connectUsers alice (list1 bob [])
  postConvQualified
    alice
    (Just aliceClient)
    defNewMLSConv
      { newConvQualifiedUsers = [Qualified bob (qDomain qalice)]
      }
    !!! do
      const 400 === statusCode
      const (Just "non-empty-member-list") === fmap Wai.label . responseJsonError

postMLSConvOk :: TestM ()
postMLSConvOk = do
  c <- view tsCannon
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice
  let aliceClient = ClientId 0
  let nameMaxSize = T.replicate 256 "a"
  WS.bracketR c alice $ \wsA -> do
    rsp <-
      postConvQualified
        alice
        (Just aliceClient)
        defNewMLSConv {newConvName = checked nameMaxSize}
    pure rsp !!! do
      const 201 === statusCode
      const Nothing === fmap Wai.label . responseJsonError
    qcid <- assertConv rsp RegularConv (Just alice) qalice [] (Just nameMaxSize) Nothing
    checkConvCreateEvent (qUnqualified qcid) wsA

-- @SF.Separation @TSFI.RESTfulAPI @S2
--
-- This test verifies that a user must be a member of an MLS conversation in order to send messages to it.
testSenderNotInConversation :: TestM ()
testSenderNotInConversation = do
  -- create users
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]

    -- upload key packages
    void $ uploadNewKeyPackage bob1

    -- create group with alice1 and bob1, but do not commit adding Bob
    void $ setupMLSGroup alice1
    mp <- createAddCommit alice1 [bob]

    traverse_ consumeWelcome (mpWelcome mp)

    message <- createApplicationMessage bob1 "some text"

    -- send the message as bob, who is not in the conversation
    err <-
      responseJsonError
        =<< postMessage bob1 (mpMessage message)
          <!! const 404 === statusCode

    liftIO $ Wai.label err @?= "no-conversation"

-- @END

testAddUserWithBundle :: TestM ()
testAddUserWithBundle = do
  [alice, bob] <- createAndConnectUsers [Nothing, Nothing]

  (qcnv, commit) <- runMLSTest $ do
    (alice1 : bobClients) <- traverse createMLSClient [alice, bob, bob]
    traverse_ uploadNewKeyPackage bobClients
    (_, qcnv) <- setupMLSGroup alice1
    commit <- createAddCommit alice1 [bob]
    welcome <- assertJust (mpWelcome commit)

    events <- mlsBracket bobClients $ \wss -> do
      events <- sendAndConsumeCommitBundle commit
      for_ (zip bobClients wss) $ \(_, ws) ->
        WS.assertMatch (5 # Second) ws $
          wsAssertMLSWelcome alice qcnv welcome
      pure events

    event <- assertOne events
    liftIO $ assertJoinEvent qcnv alice [bob] roleNameWireMember event
    pure (qcnv, commit)

  -- check that bob can now see the conversation
  convs <- getAllConvs (qUnqualified bob)
  liftIO $
    assertBool
      "Users added to an MLS group should find it when listing conversations"
      (qcnv `elem` map cnvQualifiedId convs)

  returnedGS <- getGroupInfo alice (fmap Conv qcnv)
  liftIO $ assertBool "Commit does not contain a public group State" (isJust (mpGroupInfo commit))
  liftIO $ mpGroupInfo commit @?= Just returnedGS

testAddUserNotConnected :: TestM ()
testAddUserNotConnected = do
  users@[alice, bob] <- replicateM 2 randomQualifiedUser

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient users
    void $ uploadNewKeyPackage bob1
    void $ setupMLSGroup alice1
    -- add unconnected user with a commit
    commit <- createAddCommit alice1 [bob]
    bundle <- createBundle commit
    err <- mlsBracket [alice1, bob1] $ \wss -> do
      err <-
        responseJsonError
          =<< localPostCommitBundle (mpSender commit) bundle
            <!! const 403 === statusCode
      void . liftIO $ WS.assertNoEvent (1 # WS.Second) wss
      pure err
    liftIO $ Wai.label err @?= "not-connected"

    -- now connect and retry
    liftTest $ connectUsers (qUnqualified alice) (pure (qUnqualified bob))
    void $ sendAndConsumeCommitBundle commit

testAddUserWithProteusClients :: TestM ()
testAddUserWithProteusClients = do
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)
  runMLSTest $ do
    alice1 <- createMLSClient alice
    -- bob has 2 MLS clients
    [bob1, bob2] <- replicateM 2 (createMLSClient bob)
    traverse_ uploadNewKeyPackage [bob1, bob2]
    -- and a non-MLS client
    _bob3 <- createWireClient bob

    void $ setupMLSGroup alice1
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle

testAddClientPartial :: TestM ()
testAddClientPartial = do
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)
  runMLSTest $ do
    alice1 <- createMLSClient alice
    -- bob only has 1 usable client
    [bob1, bob2, bob3] <- replicateM 3 (createMLSClient bob)
    void $ uploadNewKeyPackage bob1

    -- alice1 creates a group with bob1
    void $ setupMLSGroup alice1
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle

    -- now bob2 and bob3 upload key packages, and alice adds bob2 only
    kp <- uploadNewKeyPackage bob2
    void $ uploadNewKeyPackage bob3
    void $
      createAddCommitWithKeyPackages alice1 [(bob2, kp.raw)]
        >>= sendAndConsumeCommitBundle

testSendAnotherUsersCommit :: TestM ()
testSendAnotherUsersCommit = do
  -- create users
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]

    -- upload key packages
    void $ uploadNewKeyPackage bob1

    -- create group with alice1 and bob1
    void $ setupMLSGroup alice1
    createAddCommit alice1 [bob] >>= void . sendAndConsumeCommitBundle

    -- Alice creates a commit that adds bob2
    bob2 <- createMLSClient bob
    -- upload key packages
    void $ uploadNewKeyPackage bob2
    mp <- createAddCommit alice1 [bob]
    -- and the corresponding commit is sent from Bob instead of Alice
    err <-
      responseJsonError
        =<< (localPostCommitBundle bob1 =<< createBundle mp)
          <!! const 400 === statusCode
    liftIO $ Wai.label err @?= "mls-client-sender-user-mismatch"

testAddUsersToProteus :: TestM ()
testAddUsersToProteus = do
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)
  void $ postConvQualified (qUnqualified alice) Nothing defNewProteusConv
  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    void $ uploadNewKeyPackage bob1
    void $ setupFakeMLSGroup alice1 Nothing
    mp <- createAddCommit alice1 [bob]
    bundle <- createBundle mp
    err <-
      responseJsonError
        =<< localPostCommitBundle alice1 bundle <!! const 404 === statusCode
    liftIO $ Wai.label err @?= "no-conversation"

testAddUsersDirectly :: TestM ()
testAddUsersDirectly = do
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)
  charlie <- randomQualifiedUser
  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    void $ uploadNewKeyPackage bob1
    qcnv <- snd <$> setupMLSGroup alice1
    createAddCommit alice1 [bob] >>= void . sendAndConsumeCommitBundle
    e <-
      responseJsonError
        =<< postMembers
          (qUnqualified alice)
          (pure charlie)
          qcnv
          <!! const 403 === statusCode
    liftIO $ Wai.label e @?= "invalid-op"

testRemoveUsersDirectly :: TestM ()
testRemoveUsersDirectly = do
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)
  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    void $ uploadNewKeyPackage bob1
    qcnv <- snd <$> setupMLSGroup alice1
    createAddCommit alice1 [bob] >>= void . sendAndConsumeCommitBundle
    e <-
      responseJsonError
        =<< deleteMemberQualified
          (qUnqualified alice)
          bob
          qcnv
          <!! const 403 === statusCode
    liftIO $ Wai.label e @?= "invalid-op"

testProteusMessage :: TestM ()
testProteusMessage = do
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)
  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    void $ uploadNewKeyPackage bob1
    qcnv <- snd <$> setupMLSGroup alice1
    createAddCommit alice1 [bob] >>= void . sendAndConsumeCommitBundle
    e <-
      responseJsonError
        =<< postProteusMessageQualified
          (qUnqualified alice)
          (ciClient bob1)
          qcnv
          []
          "data"
          MismatchReportAll
          <!! const 404 === statusCode
    liftIO $ Wai.label e @?= "no-conversation"

testAddRemotesSomeUnreachable :: TestM ()
testAddRemotesSomeUnreachable = do
  let bobDomain = Domain "bob.example.com"
      charlieDomain = Domain "charlie.example.com"
  users@[alice, bob, charlie] <-
    createAndConnectUsers $
      domainText
        <$$> [Nothing, Just bobDomain, Just charlieDomain]
  runMLSTest $ do
    [alice1, bob1, _charlie1] <- traverse createMLSClient users
    (_, qcnv) <- setupMLSGroup alice1

    commit <- createAddCommit alice1 [bob, charlie]
    bundle <- createBundle commit
    let unreachable = Set.singleton charlieDomain
    void
      $ withTempMockFederator'
        ( receiveCommitMockByDomain [bob1]
            <|> mockUnreachableFor unreachable
            <|> welcomeMockByDomain [bobDomain]
        )
      $ localPostCommitBundle (mpSender commit) bundle
        <!! const 533 === statusCode

    convAfter <- responseJsonError =<< getConvQualified (qUnqualified alice) qcnv
    liftIO $ do
      memId (cmSelf (cnvMembers convAfter)) @?= alice
      cmOthers (cnvMembers convAfter) @?= []

testCommitLock :: TestM ()
testCommitLock = do
  users <- createAndConnectUsers (replicate 4 Nothing)

  runMLSTest $ do
    [alice1, bob1, charlie1, dee1] <- traverse createMLSClient users
    (groupId, _) <- setupMLSGroup alice1
    traverse_ uploadNewKeyPackage [bob1, charlie1, dee1]

    -- alice adds add bob
    void $ createAddCommit alice1 [cidQualifiedUser bob1] >>= sendAndConsumeCommitBundle

    -- alice adds charlie
    void $ createAddCommit alice1 [cidQualifiedUser charlie1] >>= sendAndConsumeCommitBundle

    -- simulate concurrent commit by blocking epoch
    casClient <- view tsCass
    runClient casClient $ insertLock groupId (Epoch 2)

    -- commit should fail due to competing lock
    do
      commit <- createAddCommit alice1 [cidQualifiedUser dee1]
      bundle <- createBundle commit
      err <-
        responseJsonError
          =<< localPostCommitBundle alice1 bundle
            <!! const 409 === statusCode
      liftIO $ Wai.label err @?= "mls-stale-message"
  where
    lock :: PrepQuery W (GroupId, Epoch) ()
    lock = "insert into mls_commit_locks (group_id, epoch) values (?, ?)"

    insertLock groupId epoch =
      retry x5 $
        write
          lock
          ( params
              LocalQuorum
              (groupId, epoch)
          )

testUnknownProposalRefCommit :: TestM ()
testUnknownProposalRefCommit = do
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)
  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    void $ setupMLSGroup alice1
    void $ uploadNewKeyPackage bob1

    -- create proposal, but don't send it to group
    void $ createAddProposals alice1 [bob]
    commit <- createPendingProposalCommit alice1

    -- send commit before proposal
    bundle <- createBundle commit
    err <-
      responseJsonError
        =<< localPostCommitBundle alice1 bundle
          <!! const 404 === statusCode
    liftIO $ Wai.label err @?= "mls-proposal-not-found"

-- The following test happens within backend B
-- Alice@A is remote and Bob@B is local
-- Alice creates a remote conversation and invites Bob
-- Bob sends a message to the conversion
--
-- In reality, the following steps would happen:
--
-- 1) alice creates a new conversation @A -> convId, groupID
-- 2) alice creates an MLS group (locally) with bob in it -> commit, welcome
-- 3) alice sends commit
-- 4) deprecated & removed: A notifies B about the new conversation
-- 5) A notifies B about bob being in the conversation (Join event)
-- 6) B notifies bob about join event
-- 7) alice sends welcome @A
-- 8) A forwards welcome to B
-- 9) B forwards welcome to bob
-- 10) bob creates his view on the group (locally) using the welcome message
--
-- 11) bob crafts a message (locally)
-- 12) bob sends the message @B
-- 13) B forwards the message to A
-- 14) A forwards the message to alice
--
-- In the test:
--
-- setup: 2 5 10 11
-- skipped: 1 3 6 7 8 9 13
-- faked: 4
-- actual test step: 12 14
testLocalToRemote :: TestM ()
testLocalToRemote = do
  -- create users
  let aliceDomain = Domain "faraway.example.com"
  [alice, bob] <- createAndConnectUsers [Just (domainText aliceDomain), Nothing]

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]

    -- upload key packages
    void $ uploadNewKeyPackage bob1

    -- step 2
    (_groupId, qcnv) <- setupFakeMLSGroup alice1 Nothing
    mp <- createAddCommit alice1 [bob]
    -- step 10
    traverse_ consumeWelcome (mpWelcome mp)
    -- step 11
    message <- createApplicationMessage bob1 "hi"

    -- A notifies B about bob being in the conversation (Join event): step 5
    receiveOnConvUpdated qcnv alice bob

    (_, reqs) <-
      withTempMockFederator' sendMessageMock $
        -- bob sends a message: step 12
        sendAndConsumeMessage message

    -- check requests to mock federator: step 14
    liftIO $ do
      req <- assertOne reqs
      frRPC req @?= "send-mls-message"
      frTargetDomain req @?= qDomain qcnv
      bdy :: MLSMessageSendRequest <- case Aeson.eitherDecode (frBody req) of
        Right b -> pure b
        Left e -> assertFailure $ "Could not parse send-mls-message request body: " <> e
      bdy.convOrSubId @?= Conv (qUnqualified qcnv)
      bdy.sender @?= qUnqualified bob
      bdy.rawMessage @?= Base64ByteString (mpMessage message)

testLocalToRemoteNonMember :: TestM ()
testLocalToRemoteNonMember = do
  -- create users
  let domain = Domain "faraway.example.com"
  [alice, bob] <- createAndConnectUsers [Just (domainText domain), Nothing]

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]

    void $ uploadNewKeyPackage bob1

    -- step 2
    void $ setupFakeMLSGroup alice1 Nothing

    mp <- createAddCommit alice1 [bob]
    -- step 10
    traverse_ consumeWelcome (mpWelcome mp)
    -- step 11
    message <- createApplicationMessage bob1 "hi"

    void $
      withTempMockFederator' sendMessageMock $ do
        galley <- viewGalley

        -- bob sends a message: step 12
        post
          ( galley
              . paths ["mls", "messages"]
              . zUser (qUnqualified bob)
              . zConn "conn"
              . zClient (ciClient bob1)
              . Bilge.content "message/mls"
              . bytes (mpMessage message)
          )
          !!! do
            const 404 === statusCode
            const (Just "no-conversation-member")
              === fmap Wai.label . responseJsonError

-- @SF.Separation @TSFI.RESTfulAPI @S2
--
-- This test verifies that only the members of an MLS conversation are allowed
-- to join via external commit.
testExternalCommitNotMember :: TestM ()
testExternalCommitNotMember = do
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)

  runMLSTest $ do
    [alice1, alice2, bob1] <- traverse createMLSClient [alice, alice, bob]
    traverse_ uploadNewKeyPackage [bob1, alice2]
    (_, qcnv) <- setupMLSGroup alice1

    -- so that we have the public group state
    void $ createAddCommit alice1 [alice] >>= sendAndConsumeCommitBundle

    pgs <- liftTest $ getGroupInfo (cidQualifiedUser alice1) (fmap Conv qcnv)
    mp <- createExternalCommit bob1 (Just pgs) (fmap Conv qcnv)
    bundle <- createBundle mp
    localPostCommitBundle (mpSender mp) bundle
      !!! const 404 === statusCode

-- @END

testExternalCommitSameClient :: TestM ()
testExternalCommitSameClient = do
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    void $ uploadNewKeyPackage bob1
    (_, qcnv) <- setupMLSGroup alice1
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle

    let rejoiner = alice1
    ecEvents <-
      createExternalCommit rejoiner Nothing (fmap Conv qcnv)
        >>= sendAndConsumeCommitBundle
    liftIO $
      assertBool "No events after external commit expected" (null ecEvents)

    message <- createApplicationMessage bob1 "hello"
    void $ sendAndConsumeMessage message

testExternalCommitNewClient :: TestM ()
testExternalCommitNewClient = do
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    void $ uploadNewKeyPackage bob1
    (_, qcnv) <- setupMLSGroup alice1
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle

    nc <- createMLSClient bob
    ecEvents <-
      createExternalCommit nc Nothing (fmap Conv qcnv)
        >>= sendAndConsumeCommitBundle
    liftIO $
      assertBool "No events after external commit expected" (null ecEvents)

    message <- createApplicationMessage nc "hello"
    void $ sendAndConsumeMessage message

-- the list of members should be [alice1, bob1]

-- | Check that external backend proposals are replayed after external commits
-- AND that (external) client proposals are NOT replayed by the backend in the
-- same case (since this is up to the clients).
testExternalCommitNewClientResendBackendProposal :: TestM ()
testExternalCommitNewClientResendBackendProposal = do
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)

  runMLSTest $ do
    [alice1, bob1, bob2] <- traverse createMLSClient [alice, bob, bob]
    forM_ [bob1, bob2] uploadNewKeyPackage
    (_, qcnv) <- setupMLSGroup alice1
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle
    Just (_, bobIdx2) <- find (\(ci, _) -> ci == bob2) <$> getClientsFromGroupState alice1 bob

    mlsBracket [alice1, bob1] $ \[wsA, wsB] -> do
      liftTest $
        deleteClient (qUnqualified bob) (ciClient bob2) (Just defPassword)
          !!! statusCode === const 200
      WS.assertMatchN_ (5 # WS.Second) [wsB] $
        wsAssertClientRemoved (ciClient bob2)

      State.modify $ \mls ->
        mls
          { mlsMembers = Set.difference (mlsMembers mls) (Set.fromList [bob2])
          }

      WS.assertMatchN_ (5 # WS.Second) [wsA, wsB] $
        wsAssertBackendRemoveProposalWithEpoch bob qcnv bobIdx2 (Epoch 1)

      [bob3, bob4] <- for [bob, bob] $ \qusr' -> do
        ci <- createMLSClient qusr'
        WS.assertMatchN_ (5 # WS.Second) [wsB] $
          wsAssertClientAdded (ciClient ci)
        pure ci

      void $
        createExternalAddProposal bob3
          >>= sendAndConsumeMessage

      WS.assertMatchN_ (5 # WS.Second) [wsA, wsB] $
        void . wsAssertAddProposal bob qcnv

      mp <- createExternalCommit bob4 Nothing (fmap Conv qcnv)
      ecEvents <- sendAndConsumeCommitBundle mp
      liftIO $
        assertBool "No events after external commit expected" (null ecEvents)

      WS.assertMatchN_ (5 # WS.Second) [wsA, wsB] $
        wsAssertMLSMessage (fmap Conv qcnv) bob (mpMessage mp)

      -- The backend proposals for bob2 are replayed, but the external add
      -- proposal for bob3 has to replayed by the client and is thus not found
      -- here.
      WS.assertMatchN_ (5 # WS.Second) [wsA, wsB] $
        wsAssertBackendRemoveProposalWithEpoch bob qcnv bobIdx2 (Epoch 2)
      WS.assertNoEvent (2 # WS.Second) [wsA, wsB]

testAppMessage :: TestM ()
testAppMessage = do
  users@(alice : _) <- createAndConnectUsers (replicate 4 Nothing)

  runMLSTest $ do
    clients@(alice1 : _) <- traverse createMLSClient users
    traverse_ uploadNewKeyPackage (tail clients)
    (_, qcnv) <- setupMLSGroup alice1
    void $ createAddCommit alice1 (tail users) >>= sendAndConsumeCommitBundle
    message <- createApplicationMessage alice1 "some text"

    mlsBracket clients $ \wss -> do
      events <- sendAndConsumeMessage message
      liftIO $ events @?= []
      liftIO $ do
        WS.assertMatchN_ (5 # WS.Second) (tail wss) $
          wsAssertMLSMessage (fmap Conv qcnv) alice (mpMessage message)
        WS.assertNoEvent (2 # WS.Second) [head wss]

testAppMessage2 :: TestM ()
testAppMessage2 = do
  -- create users
  [alice, bob, charlie] <- createAndConnectUsers (replicate 3 Nothing)

  runMLSTest $ do
    alice1 : clients@[bob1, _bob2, _charlie1] <-
      traverse createMLSClient [alice, bob, bob, charlie]

    -- upload key packages
    traverse_ uploadNewKeyPackage clients

    -- create group with alice1 and other clients
    conversation <- snd <$> setupMLSGroup alice1
    mp <- createAddCommit alice1 [bob, charlie]
    void $ sendAndConsumeCommitBundle mp

    traverse_ consumeWelcome (mpWelcome mp)

    message <- createApplicationMessage bob1 "some text"

    mlsBracket (alice1 : clients) $ \[wsAlice1, wsBob1, wsBob2, wsCharlie1] -> do
      events <- sendAndConsumeMessage message
      liftIO $ events @?= []

      -- check that the corresponding event is received by everyone except bob1
      -- (the sender) and no message is received by bob1
      liftIO $ do
        WS.assertMatchN_ (5 # WS.Second) [wsAlice1, wsBob2, wsCharlie1] $
          wsAssertMLSMessage (fmap Conv conversation) bob (mpMessage message)
        WS.assertNoEvent (2 # WS.Second) [wsBob1]

testAppMessageUnreachable :: TestM ()
testAppMessageUnreachable = do
  -- alice is local, bob is remote
  -- alice creates a local conversation and invites bob
  -- alice then sends a message to the conversation, but bob is not reachable anymore
  -- since we did not properly setup federation, we can't reach the remote server with bob's msg
  users@[_alice, bob] <- createAndConnectUsers [Nothing, Just "bob.example.com"]
  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient users
    void $ setupMLSGroup alice1

    commit <- createAddCommit alice1 [bob]
    ([event], _) <-
      withTempMockFederator' (receiveCommitMock [bob1] <|> welcomeMock) $
        sendAndConsumeCommitBundle commit

    message <- createApplicationMessage alice1 "hi, bob!"
    _ <- sendAndConsumeMessage message
    liftIO $ do
      assertBool "Event should be member join" $ is _EdMembersJoin (evtData event)

testRemoteToRemoteInSub :: TestM ()
testRemoteToRemoteInSub = do
  localDomain <- viewFederationDomain
  c <- view tsCannon
  alice <- randomUser
  eve <- randomUser
  bob <- randomId
  conv <- randomId
  let subConvId = SubConvId "conference"
      aliceC1 = ClientId 0
      aliceC2 = ClientId 1
      eveC = ClientId 0
      bdom = Domain "bob.example.com"
      qconv = Qualified conv bdom
      qbob = Qualified bob bdom
      qalice = Qualified alice localDomain
  now <- liftIO getCurrentTime
  fedGalleyClient <- view tsFedGalleyClient

  -- only add alice to the remote conversation
  connectWithRemoteUser alice qbob
  let cu =
        ConversationUpdate
          { time = now,
            origUserId = qbob,
            convId = conv,
            alreadyPresentUsers = [],
            action =
              SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qalice) roleNameWireMember InternalAdd)
          }
  void $ runFedClient @"on-conversation-updated" fedGalleyClient bdom cu

  let txt = "Hello from another backend"
      rcpts = Map.fromList [(alice, aliceC1 :| [aliceC2]), (eve, eveC :| [])]
      rm =
        RemoteMLSMessage
          { time = now,
            metadata = defMessageMetadata,
            sender = qbob,
            conversation = conv,
            subConversation = Just subConvId,
            recipients = rcpts,
            message = Base64ByteString txt
          }

  -- send message to alice and check reception
  WS.bracketAsClientRN c [(alice, aliceC1), (alice, aliceC2), (eve, eveC)] $ \[wsA1, wsA2, wsE] -> do
    void $ runFedClient @"on-mls-message-sent" fedGalleyClient bdom rm
    liftIO $ do
      -- alice should receive the message on her first client
      WS.assertMatch_ (5 # Second) wsA1 $ \n -> wsAssertMLSMessage (fmap (flip SubConv subConvId) qconv) qbob txt n
      WS.assertMatch_ (5 # Second) wsA2 $ \n -> wsAssertMLSMessage (fmap (flip SubConv subConvId) qconv) qbob txt n

      -- eve should not receive the message
      WS.assertNoEvent (1 # Second) [wsE]

testRemoteToLocal :: TestM ()
testRemoteToLocal = do
  -- alice is local, bob is remote
  -- alice creates a local conversation and invites bob
  -- bob then sends a message to the conversation

  let bobDomain = Domain "faraway.example.com"
  -- create users
  [alice, bob] <-
    createAndConnectUsers
      [ Nothing,
        Just (domainText bobDomain)
      ]

  -- Simulate the whole MLS setup for both clients first. In reality,
  -- backend calls would need to happen in order for bob to get ahold of a
  -- welcome message, but that should not affect the correctness of the test.
  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]

    (_groupId, qcnv) <- setupMLSGroup alice1
    kpb <- claimKeyPackages alice1 bob
    mp <- createAddCommit alice1 [bob]

    let mock = receiveCommitMock [bob1] <|> welcomeMock <|> claimKeyPackagesMock kpb
    void . withTempMockFederator' mock $
      sendAndConsumeCommitBundle mp

    traverse_ consumeWelcome (mpWelcome mp)
    message <- createApplicationMessage bob1 "hello from another backend"

    fedGalleyClient <- view tsFedGalleyClient
    cannon <- view tsCannon

    -- actual test
    let msr =
          MLSMessageSendRequest
            { convOrSubId = Conv (qUnqualified qcnv),
              sender = qUnqualified bob,
              senderClient = ciClient bob1,
              rawMessage = Base64ByteString (mpMessage message)
            }

    WS.bracketR cannon (qUnqualified alice) $ \ws -> do
      MLSMessageResponseUpdates updates <- runFedClient @"send-mls-message" fedGalleyClient bobDomain msr
      liftIO $ do
        updates @?= []
        WS.assertMatch_ (5 # Second) ws $
          wsAssertMLSMessage (fmap Conv qcnv) bob (mpMessage message)

testRemoteToLocalWrongConversation :: TestM ()
testRemoteToLocalWrongConversation = do
  -- alice is local, bob is remote
  -- alice creates a local conversation and invites bob
  -- bob then sends a message to the conversation

  let bobDomain = Domain "faraway.example.com"
  [alice, bob] <- createAndConnectUsers [Nothing, Just (domainText bobDomain)]

  -- Simulate the whole MLS setup for both clients first. In reality,
  -- backend calls would need to happen in order for bob to get ahold of a
  -- welcome message, but that should not affect the correctness of the test.

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]

    void $ claimKeyPackages alice1 bob
    void $ setupMLSGroup alice1
    mp <- createAddCommit alice1 [bob]

    let mock = receiveCommitMock [bob1] <|> welcomeMock
    void . withTempMockFederator' mock $ sendAndConsumeCommitBundle mp
    traverse_ consumeWelcome (mpWelcome mp)
    message <- createApplicationMessage bob1 "hello from another backend"

    fedGalleyClient <- view tsFedGalleyClient

    -- actual test
    randomConfId <- randomId
    let msr =
          MLSMessageSendRequest
            { convOrSubId = Conv randomConfId,
              sender = qUnqualified bob,
              senderClient = ciClient bob1,
              rawMessage = Base64ByteString (mpMessage message)
            }

    resp <- runFedClient @"send-mls-message" fedGalleyClient bobDomain msr
    liftIO $ resp @?= MLSMessageResponseError MLSGroupConversationMismatch

testRemoteNonMemberToLocal :: TestM ()
testRemoteNonMemberToLocal = do
  -- alice is local, bob is remote
  -- alice creates a local conversation and invites bob
  -- bob then sends a message to the conversation

  let bobDomain = Domain "faraway.example.com"
  [alice, bob] <- createAndConnectUsers [Nothing, Just (domainText bobDomain)]

  -- Simulate the whole MLS setup for both clients first. In reality,
  -- backend calls would need to happen in order for bob to get ahold of a
  -- welcome message, but that should not affect the correctness of the test.

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]

    qcnv <- snd <$> setupMLSGroup alice1
    void $ claimKeyPackages alice1 bob
    mp <- createAddCommit alice1 [bob]
    traverse_ consumeWelcome (mpWelcome mp)

    message <- createApplicationMessage bob1 "hello from another backend"

    let msr =
          MLSMessageSendRequest
            { convOrSubId = Conv (qUnqualified qcnv),
              sender = qUnqualified bob,
              senderClient = ciClient bob1,
              rawMessage = Base64ByteString (mpMessage message)
            }

    fedGalleyClient <- view tsFedGalleyClient
    resp <- runFedClient @"send-mls-message" fedGalleyClient bobDomain msr
    liftIO $ do
      resp @?= MLSMessageResponseError ConvNotFound

-- | The group exists in mls-test-cli's store, but not in wire-server's database.
propNonExistingConv :: TestM ()
propNonExistingConv = do
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)
  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    void $ uploadNewKeyPackage bob1
    void $ setupFakeMLSGroup alice1 Nothing

    [prop] <- createAddProposals alice1 [bob]
    postMessage alice1 (mpMessage prop) !!! do
      const 404 === statusCode
      const (Just "no-conversation") === fmap Wai.label . responseJsonError

-- scenario:
-- alice1 creates a group and adds bob1
-- bob2 joins with external proposal (alice1 commits it)
-- bob2 adds charlie1
-- alice1 sends a message
testExternalAddProposal :: TestM ()
testExternalAddProposal = do
  -- create users
  [alice, bob, charlie] <-
    createAndConnectUsers (replicate 3 Nothing)

  void . runMLSTest $ do
    -- create clients
    alice1 <- createMLSClient alice
    bob1 <- createMLSClient bob
    charlie1 <- createMLSClient charlie

    -- upload key packages
    void $ uploadNewKeyPackage bob1
    void $ uploadNewKeyPackage charlie1

    -- create group with alice1 and bob1
    (_, qcnv) <- setupMLSGroup alice1
    void $
      createAddCommit alice1 [bob]
        >>= sendAndConsumeCommitBundle

    -- bob joins with an external proposal
    bob2 <- createMLSClient bob
    mlsBracket [alice1, bob1] $ \wss -> do
      void $
        createExternalAddProposal bob2
          >>= sendAndConsumeMessage
      liftTest $
        WS.assertMatchN_ (5 # Second) wss $
          void . wsAssertAddProposal bob qcnv

    void $
      createPendingProposalCommit alice1
        >>= sendAndConsumeCommitBundle

    -- alice sends a message
    do
      msg <- createApplicationMessage alice1 "hi bob"
      mlsBracket [bob1, bob2] $ \wss -> do
        void $ sendAndConsumeMessage msg
        liftTest $
          WS.assertMatchN_ (5 # Second) wss $
            wsAssertMLSMessage (fmap Conv qcnv) alice (mpMessage msg)

    -- bob adds charlie
    putOtherMemberQualified
      (qUnqualified alice)
      bob
      (OtherMemberUpdate (Just roleNameWireAdmin))
      qcnv
      !!! const 200 === statusCode
    createAddCommit bob2 [charlie]
      >>= sendAndConsumeCommitBundle

testExternalAddProposalNonAdminCommit :: TestM ()
testExternalAddProposalNonAdminCommit = do
  -- create users
  [alice, bob, charlie] <-
    createAndConnectUsers (replicate 3 Nothing)

  void . runMLSTest $ do
    -- create clients
    alice1 <- createMLSClient alice
    [bob1, bob2] <- replicateM 2 (createMLSClient bob)
    charlie1 <- createMLSClient charlie

    -- upload key packages
    void $ uploadNewKeyPackage bob1
    void $ uploadNewKeyPackage charlie1

    -- create group with alice1 and bob1
    (_, qcnv) <- setupMLSGroup alice1
    void $
      createAddCommit alice1 [bob]
        >>= sendAndConsumeCommitBundle

    -- bob joins with an external proposal
    mlsBracket [alice1, bob1] $ \wss -> do
      void $
        createExternalAddProposal bob2
          >>= sendAndConsumeMessage
      liftTest $
        WS.assertMatchN_ (5 # Second) wss $
          void . wsAssertAddProposal bob qcnv

    -- bob1 commits
    void $
      createPendingProposalCommit bob1
        >>= sendAndConsumeCommitBundle

-- scenario:
-- alice adds bob and charlie
-- charlie sends an external proposal for bob
testExternalAddProposalWrongClient :: TestM ()
testExternalAddProposalWrongClient = do
  [alice, bob, charlie] <-
    createAndConnectUsers (replicate 3 Nothing)

  runMLSTest $ do
    -- setup clients
    [alice1, bob1, bob2, charlie1] <-
      traverse
        createMLSClient
        [alice, bob, bob, charlie]
    void $ uploadNewKeyPackage bob1
    void $ uploadNewKeyPackage charlie1

    void $ setupMLSGroup alice1
    void $
      createAddCommit alice1 [bob, charlie]
        >>= sendAndConsumeCommitBundle

    prop <- createExternalAddProposal bob2
    postMessage charlie1 (mpMessage prop)
      !!! do
        const 422 === statusCode
        const (Just "mls-unsupported-proposal") === fmap Wai.label . responseJsonError

-- scenario:
-- alice adds bob
-- charlie attempts to join with an external add proposal
testExternalAddProposalWrongUser :: TestM ()
testExternalAddProposalWrongUser = do
  users@[_, bob, _charlie] <- createAndConnectUsers (replicate 3 Nothing)

  runMLSTest $ do
    -- setup clients
    [alice1, bob1, charlie1] <- traverse createMLSClient users
    void $ uploadNewKeyPackage bob1

    void $ setupMLSGroup alice1
    void $
      createAddCommit alice1 [bob]
        >>= sendAndConsumeCommitBundle

    prop <- createExternalAddProposal charlie1
    postMessage charlie1 (mpMessage prop)
      !!! do
        const 404 === statusCode
        const (Just "no-conversation") === fmap Wai.label . responseJsonError

testBackendRemoveProposalRecreateClient :: TestM ()
testBackendRemoveProposalRecreateClient = do
  alice <- randomQualifiedUser
  runMLSTest $ do
    alice1 <- createMLSClient alice
    (_, qcnv) <- setupMLSSelfGroup alice1

    let cnv = Conv <$> qcnv

    void $ createPendingProposalCommit alice1 >>= sendAndConsumeCommitBundle

    (_, idx) <- assertOne =<< getClientsFromGroupState alice1 alice

    liftTest $
      deleteClient (qUnqualified alice) (ciClient alice1) (Just defPassword)
        !!! const 200 === statusCode
    State.modify $ \mls ->
      mls
        { mlsMembers = Set.difference (mlsMembers mls) (Set.singleton alice1)
        }

    alice2 <- createMLSClient alice
    proposal <- mlsBracket [alice2] $ \[wsA] -> do
      -- alice2 joins the conversation, causing the external remove proposal to
      -- be re-established
      void $
        createExternalCommit alice2 Nothing cnv
          >>= sendAndConsumeCommitBundle
      WS.assertMatch (5 # WS.Second) wsA $
        wsAssertBackendRemoveProposal alice (Conv <$> qcnv) idx

    consumeMessage1 alice2 proposal
    void $ createPendingProposalCommit alice2 >>= sendAndConsumeCommitBundle

    void $ createApplicationMessage alice2 "hello" >>= sendAndConsumeMessage

testBackendRemoveProposalLocalConvLocalUser :: TestM ()
testBackendRemoveProposalLocalConvLocalUser = do
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)
  runMLSTest $ do
    [alice1, bob1, bob2] <- traverse createMLSClient [alice, bob, bob]
    traverse_ uploadNewKeyPackage [bob1, bob2]
    (_, qcnv) <- setupMLSGroup alice1
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle

    bobClients <- getClientsFromGroupState alice1 bob
    mlsBracket [alice1] $ \wss -> void $ do
      liftTest $ deleteUser (qUnqualified bob) !!! const 200 === statusCode
      -- remove bob clients from the test state
      State.modify $ \mls ->
        mls
          { mlsMembers = Set.difference (mlsMembers mls) (Set.fromList [bob1, bob2])
          }

      for bobClients $ \(_, idx) -> do
        [msg] <- WS.assertMatchN (5 # Second) wss $ \n ->
          wsAssertBackendRemoveProposal bob (Conv <$> qcnv) idx n
        consumeMessage1 alice1 msg

    -- alice commits the external proposals
    events <- createPendingProposalCommit alice1 >>= sendAndConsumeCommitBundle
    liftIO $ events @?= []

testBackendRemoveProposalLocalConvRemoteUser :: TestM ()
testBackendRemoveProposalLocalConvRemoteUser = do
  [alice, bob] <- createAndConnectUsers [Nothing, Just "bob.example.com"]
  runMLSTest $ do
    [alice1, bob1, bob2] <- traverse createMLSClient [alice, bob, bob]
    (_, qcnv) <- setupMLSGroup alice1
    commit <- createAddCommit alice1 [bob]

    let mock = receiveCommitMock [bob1, bob2] <|> welcomeMock <|> messageSentMock
    void . withTempMockFederator' mock $ do
      mlsBracket [alice1] $ \[wsA] -> do
        void $ sendAndConsumeCommitBundle commit

        bobClients <- getClientsFromGroupState alice1 bob
        fedGalleyClient <- view tsFedGalleyClient
        void $
          runFedClient
            @"on-user-deleted-conversations"
            fedGalleyClient
            (qDomain bob)
            ( UserDeletedConversationsNotification
                { user = qUnqualified bob,
                  conversations = unsafeRange [qUnqualified qcnv]
                }
            )

        for_ bobClients $ \(_, idx) ->
          WS.assertMatch (5 # WS.Second) wsA $
            wsAssertBackendRemoveProposal bob (Conv <$> qcnv) idx

sendRemoteMLSWelcome :: TestM ()
sendRemoteMLSWelcome = do
  -- Alice is from the originating domain and Bob is local, i.e., on the receiving domain
  [alice, bob] <- createAndConnectUsers [Just "alice.example.com", Nothing]
  (commit, bob1, qcid) <- runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    (_, qcid) <- setupFakeMLSGroup alice1 Nothing
    void $ uploadNewKeyPackage bob1
    commit <- createAddCommit alice1 [bob]
    pure (commit, bob1, qcid)

  welcome <- assertJust (mpWelcome commit)

  fedGalleyClient <- view tsFedGalleyClient
  cannon <- view tsCannon

  WS.bracketR cannon (qUnqualified bob) $ \wsB -> do
    -- send welcome message
    void $
      runFedClient @"mls-welcome" fedGalleyClient (qDomain alice) $
        MLSWelcomeRequest
          (qUnqualified alice)
          (Base64ByteString welcome)
          [qUnqualified (cidQualifiedClient bob1)]
          qcid

    -- check that the corresponding event is received
    liftIO $ do
      WS.assertMatch_ (5 # WS.Second) wsB $
        wsAssertMLSWelcome alice qcid welcome

testBackendRemoveProposalLocalConvLocalLeaverCreator :: TestM ()
testBackendRemoveProposalLocalConvLocalLeaverCreator = do
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)

  runMLSTest $ do
    [alice1, bob1, bob2] <- traverse createMLSClient [alice, bob, bob]
    traverse_ uploadNewKeyPackage [bob1, bob2]
    (_, qcnv) <- setupMLSGroup alice1
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle

    aliceClients <- getClientsFromGroupState alice1 alice
    mlsBracket [alice1, bob1, bob2] $ \wss -> void $ do
      liftTest $
        deleteMemberQualified (qUnqualified alice) alice qcnv
          !!! const 200 === statusCode
      -- remove alice's client from the test state
      State.modify $ \mls ->
        mls
          { mlsMembers = Set.difference (mlsMembers mls) (Set.fromList [alice1])
          }

      for_ aliceClients $ \(_, idx) -> do
        -- only bob's clients should receive the external proposals
        msgs <- WS.assertMatchN (5 # Second) (drop 1 wss) $ \n ->
          wsAssertBackendRemoveProposal alice (Conv <$> qcnv) idx n
        traverse_ (uncurry consumeMessage1) (zip [bob1, bob2] msgs)

      -- but everyone should receive leave events
      WS.assertMatchN_ (5 # WS.Second) wss $
        wsAssertMembersLeave qcnv alice [alice]

      -- check that no more events are sent, so in particular alice does not
      -- receive any MLS messages
      WS.assertNoEvent (1 # WS.Second) wss

    -- bob commits the external proposals
    events <- createPendingProposalCommit bob1 >>= sendAndConsumeCommitBundle
    liftIO $ events @?= []

testBackendRemoveProposalLocalConvLocalLeaverCommitter :: TestM ()
testBackendRemoveProposalLocalConvLocalLeaverCommitter = do
  [alice, bob, charlie] <- createAndConnectUsers (replicate 3 Nothing)

  runMLSTest $ do
    [alice1, bob1, bob2, charlie1] <- traverse createMLSClient [alice, bob, bob, charlie]
    traverse_ uploadNewKeyPackage [bob1, bob2, charlie1]
    (_, qcnv) <- setupMLSGroup alice1
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle

    -- promote bob
    putOtherMemberQualified (ciUser alice1) bob (OtherMemberUpdate (Just roleNameWireAdmin)) qcnv
      !!! const 200 === statusCode

    void $ createAddCommit bob1 [charlie] >>= sendAndConsumeCommitBundle

    bobClients <- getClientsFromGroupState alice1 bob
    mlsBracket [alice1, charlie1, bob1, bob2] $ \wss -> void $ do
      liftTest $
        deleteMemberQualified (qUnqualified bob) bob qcnv
          !!! const 200 === statusCode
      -- remove bob clients from the test state
      State.modify $ \mls ->
        mls
          { mlsMembers = Set.difference (mlsMembers mls) (Set.fromList [bob1, bob2])
          }

      for_ bobClients $ \(_, idx) -> do
        -- only alice and charlie should receive the external proposals
        msgs <- WS.assertMatchN (5 # Second) (take 2 wss) $ \n ->
          wsAssertBackendRemoveProposal bob (Conv <$> qcnv) idx n
        traverse_ (uncurry consumeMessage1) (zip [alice1, charlie1] msgs)

      -- but everyone should receive leave events
      WS.assertMatchN_ (5 # WS.Second) wss $
        wsAssertMembersLeave qcnv bob [bob]

      -- check that no more events are sent, so in particular bob does not
      -- receive any MLS messages
      WS.assertNoEvent (1 # WS.Second) wss

    -- alice commits the external proposals
    events <- createPendingProposalCommit alice1 >>= sendAndConsumeCommitBundle
    liftIO $ events @?= []

testBackendRemoveProposalLocalConvRemoteLeaver :: TestM ()
testBackendRemoveProposalLocalConvRemoteLeaver = do
  [alice, bob] <- createAndConnectUsers [Nothing, Just "bob.example.com"]

  runMLSTest $ do
    [alice1, bob1, bob2] <- traverse createMLSClient [alice, bob, bob]
    (_, qcnv) <- setupMLSGroup alice1
    commit <- createAddCommit alice1 [bob]

    let mock = receiveCommitMock [bob1, bob2] <|> welcomeMock <|> messageSentMock
    bobClients <- getClientsFromGroupState alice1 bob
    void . withTempMockFederator' mock $ do
      mlsBracket [alice1] $ \[wsA] -> void $ do
        void $ sendAndConsumeCommitBundle commit
        fedGalleyClient <- view tsFedGalleyClient
        void $
          runFedClient
            @"update-conversation"
            fedGalleyClient
            (qDomain bob)
            ConversationUpdateRequest
              { user = qUnqualified bob,
                convId = qUnqualified qcnv,
                action = SomeConversationAction SConversationLeaveTag ()
              }

        for_ bobClients $ \(_, idx) ->
          WS.assertMatch_ (5 # WS.Second) wsA $
            wsAssertBackendRemoveProposal bob (Conv <$> qcnv) idx

testBackendRemoveProposalLocalConvLocalClient :: TestM ()
testBackendRemoveProposalLocalConvLocalClient = do
  [alice, bob, charlie] <- createAndConnectUsers (replicate 3 Nothing)

  runMLSTest $ do
    [alice1, bob1, bob2, charlie1] <- traverse createMLSClient [alice, bob, bob, charlie]
    traverse_ uploadNewKeyPackage [bob1, bob2, charlie1]
    (_, qcnv) <- setupMLSGroup alice1
    void $ createAddCommit alice1 [bob, charlie] >>= sendAndConsumeCommitBundle
    Just (_, idxBob1) <- find (\(ci, _) -> ci == bob1) <$> getClientsFromGroupState alice1 bob

    mlsBracket [alice1, bob1, charlie1] $ \[wsA, wsB, wsC] -> do
      liftTest $
        deleteClient (ciUser bob1) (ciClient bob1) (Just defPassword)
          !!! statusCode === const 200

      State.modify $ \mls ->
        mls
          { mlsMembers = Set.difference (mlsMembers mls) (Set.fromList [bob1])
          }

      WS.assertMatch_ (5 # WS.Second) wsB $
        wsAssertClientRemoved (ciClient bob1)

      (msg : _) <- WS.assertMatchN (5 # WS.Second) [wsA, wsC] $ \notification -> do
        wsAssertBackendRemoveProposal bob (Conv <$> qcnv) idxBob1 notification

      for_ [alice1, bob2, charlie1] $
        flip consumeMessage1 msg

      mp <- createPendingProposalCommit charlie1
      events <- sendAndConsumeCommitBundle mp
      liftIO $ events @?= []
      WS.assertMatchN_ (5 # WS.Second) [wsA] $ \n -> do
        wsAssertMLSMessage (Conv <$> qcnv) charlie (mpMessage mp) n
      WS.assertNoEvent (2 # WS.Second) [wsC]

testBackendRemoveProposalLocalConvRemoteClient :: TestM ()
testBackendRemoveProposalLocalConvRemoteClient = do
  [alice, bob] <- createAndConnectUsers [Nothing, Just "faraway.example.com"]

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    (_, qcnv) <- setupMLSGroup alice1
    commit <- createAddCommit alice1 [bob]

    [(_, idxBob1)] <- getClientsFromGroupState alice1 bob
    let mock = receiveCommitMock [bob1] <|> welcomeMock <|> messageSentMock
    void . withTempMockFederator' mock $ do
      mlsBracket [alice1] $ \[wsA] -> void $ do
        void $ sendAndConsumeCommitBundle commit

        fedGalleyClient <- view tsFedGalleyClient
        void $
          runFedClient
            @"on-client-removed"
            fedGalleyClient
            (ciDomain bob1)
            (ClientRemovedRequest (ciUser bob1) (ciClient bob1) [qUnqualified qcnv])

        WS.assertMatch_ (5 # WS.Second) wsA $
          \notification ->
            void $ wsAssertBackendRemoveProposal bob (Conv <$> qcnv) idxBob1 notification

testGetGroupInfoOfLocalConv :: TestM ()
testGetGroupInfoOfLocalConv = do
  [alice, bob] <- createAndConnectUsers [Nothing, Nothing]

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    traverse_ uploadNewKeyPackage [bob1]
    (_, qcnv) <- setupMLSGroup alice1
    commit <- createAddCommit alice1 [bob]

    void $ sendAndConsumeCommitBundle commit

    -- check the group info matches
    gs <- assertJust (mpGroupInfo commit)
    returnedGS <- liftTest $ getGroupInfo alice (fmap Conv qcnv)
    liftIO $ gs @=? returnedGS

testGetGroupInfoOfRemoteConv :: TestM ()
testGetGroupInfoOfRemoteConv = do
  let aliceDomain = Domain "faraway.example.com"
  [alice, bob, charlie] <- createAndConnectUsers [Just (domainText aliceDomain), Nothing, Nothing]

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]

    void $ uploadNewKeyPackage bob1
    (_groupId, qcnv) <- setupFakeMLSGroup alice1 Nothing
    mp <- createAddCommit alice1 [bob]
    traverse_ consumeWelcome (mpWelcome mp)

    receiveOnConvUpdated qcnv alice bob

    let fakeGroupState = "\xde\xad\xbe\xef"
        mock = queryGroupStateMock fakeGroupState bob
    (_, reqs) <- withTempMockFederator' mock $ do
      res <- liftTest $ getGroupInfo bob (fmap Conv qcnv)
      liftIO $ res @?= fakeGroupState

      localGetGroupInfo (qUnqualified charlie) (fmap Conv qcnv)
        !!! const 404 === statusCode

    -- check requests to mock federator: step 14
    liftIO $ do
      let (req, _req2) = assertTwo reqs
      frRPC req @?= "query-group-info"
      frTargetDomain req @?= qDomain qcnv

testFederatedGetGroupInfo :: TestM ()
testFederatedGetGroupInfo = do
  [alice, bob, charlie] <- createAndConnectUsers [Nothing, Just "faraway.example.com", Just "faraway.example.com"]

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    (_, qcnv) <- setupMLSGroup alice1
    commit <- createAddCommit alice1 [bob]
    groupState <- assertJust (mpGroupInfo commit)

    let mock = receiveCommitMock [bob1] <|> welcomeMock
    void . withTempMockFederator' mock $ do
      void $ sendAndConsumeCommitBundle commit

      fedGalleyClient <- view tsFedGalleyClient
      do
        resp <-
          runFedClient
            @"query-group-info"
            fedGalleyClient
            (ciDomain bob1)
            (GetGroupInfoRequest (Conv (qUnqualified qcnv)) (qUnqualified bob))

        liftIO $ case resp of
          GetGroupInfoResponseError err -> assertFailure ("Unexpected error: " <> show err)
          GetGroupInfoResponseState gs ->
            fromBase64ByteString gs @=? groupState

      do
        resp <-
          runFedClient
            @"query-group-info"
            fedGalleyClient
            (ciDomain bob1)
            (GetGroupInfoRequest (Conv (qUnqualified qcnv)) (qUnqualified charlie))

        liftIO $ case resp of
          GetGroupInfoResponseError err ->
            err @?= ConvNotFound
          GetGroupInfoResponseState _ ->
            assertFailure "Unexpected success"

testDeleteMLSConv :: TestM ()
testDeleteMLSConv = do
  localDomain <- viewFederationDomain
  -- c <- view tsCannon
  (tid, aliceUnq, [bobUnq]) <- API.Util.createBindingTeamWithMembers 2
  let alice = Qualified aliceUnq localDomain
      bob = Qualified bobUnq localDomain

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    void $ uploadNewKeyPackage bob1

    (_, qcnv) <- setupMLSGroup alice1
    commit <- createAddCommit alice1 [bob]
    void $ sendAndConsumeCommitBundle commit

    deleteTeamConv tid (qUnqualified qcnv) aliceUnq
      !!! statusCode === const 200

testAddUserToRemoteConvWithBundle :: TestM ()
testAddUserToRemoteConvWithBundle = do
  let aliceDomain = Domain "faraway.example.com"
  [alice, bob, charlie] <- createAndConnectUsers [Just (domainText aliceDomain), Nothing, Nothing]

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]

    void $ uploadNewKeyPackage bob1
    (_groupId, qcnv) <- setupFakeMLSGroup alice1 Nothing

    mp <- createAddCommit alice1 [bob]
    traverse_ consumeWelcome (mpWelcome mp)

    receiveOnConvUpdated qcnv alice bob

    -- NB. this commit would be rejected by the owning backend, but for the
    -- purpose of this test it's good enough.
    [charlie1] <- traverse createMLSClient [charlie]
    void $ uploadNewKeyPackage charlie1
    commit <- createAddCommit bob1 [charlie]
    commitBundle <- createBundle commit

    let mock = "send-mls-commit-bundle" ~> MLSMessageResponseUpdates []
    (_, reqs) <- withTempMockFederator' mock $ do
      void $ sendAndConsumeCommitBundle commit

    req <- liftIO $ assertOne reqs
    liftIO $ do
      frRPC req @?= "send-mls-commit-bundle"
      frTargetDomain req @?= qDomain qcnv

      msr :: MLSMessageSendRequest <- case Aeson.eitherDecode (frBody req) of
        Right b -> pure b
        Left e -> assertFailure $ "Could not parse send-mls-commit-bundle request body: " <> e

      msr.convOrSubId @?= Conv (qUnqualified qcnv)
      msr.sender @?= qUnqualified bob
      fromBase64ByteString (msr.rawMessage) @?= commitBundle

-- | The MLS self-conversation should be available even without explicitly
-- creating it by calling `GET /conversations/mls-self` starting from version 3
-- of the client API and should not be listed in versions less than 3.
testSelfConversationList :: Bool -> TestM ()
testSelfConversationList isBelowV3 = do
  let (errMsg, justOrNothing, listCnvs) =
        if isBelowV3
          then ("The MLS self-conversation is listed", isNothing, getConvPageV2)
          else ("The MLS self-conversation is not listed", isJust, getConvPage)
  alice <- randomUser
  do
    mMLSSelf <- findSelfConv alice listCnvs
    liftIO $ assertBool errMsg (justOrNothing mMLSSelf)

  -- make sure that the self-conversation is not listed below V3 even once it
  -- has been created.
  unless isBelowV3 $ do
    mMLSSelf <- findSelfConv alice getConvPageV2
    liftIO $ assertBool errMsg (isNothing mMLSSelf)
  where
    isMLSSelf u conv = mlsSelfConvId u == qUnqualified conv

    findSelfConv u listEndpoint = do
      convIds :: ConvIdsPage <-
        responseJsonError
          =<< listEndpoint u Nothing (Just 100)
            <!! const 200 === statusCode
      pure . getAlt $
        foldMap (Alt . guard . isMLSSelf u) $
          mtpResults convIds

    getConvPageV2 u s c = do
      g <- view tsUnversionedGalley
      getConvPageWithGalley (addPrefixAtVersion V2 . g) u s c

testSelfConversationMLSNotConfigured :: TestM ()
testSelfConversationMLSNotConfigured = do
  alice <- randomUser
  withMLSDisabled $
    getConvPage alice Nothing (Just 100) !!! const 200 === statusCode

testSelfConversationOtherUser :: TestM ()
testSelfConversationOtherUser = do
  users@[_alice, bob] <- createAndConnectUsers [Nothing, Nothing]
  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient users
    void $ uploadNewKeyPackage bob1
    void $ setupMLSSelfGroup alice1
    commit <- createAddCommit alice1 [bob]
    bundle <- createBundle commit
    mlsBracket [alice1, bob1] $ \wss -> do
      localPostCommitBundle (mpSender commit) bundle
        !!! do
          const 403 === statusCode
          const (Just "invalid-op") === fmap Wai.label . responseJsonError
      WS.assertNoEvent (1 # WS.Second) wss

testSelfConversationLeave :: TestM ()
testSelfConversationLeave = do
  alice <- randomQualifiedUser
  runMLSTest $ do
    (creator : others) <- traverse createMLSClient (replicate 3 alice)
    traverse_ uploadNewKeyPackage others
    (_, qcnv) <- setupMLSSelfGroup creator
    void $ createAddCommit creator [alice] >>= sendAndConsumeCommitBundle
    liftTest $
      deleteMemberQualified (qUnqualified alice) alice qcnv
        !!! do
          const 403 === statusCode
          const (Just "invalid-op") === fmap Wai.label . responseJsonError

assertMLSNotEnabled :: Assertions ()
assertMLSNotEnabled = do
  const 400 === statusCode
  const (Just "mls-not-enabled") === fmap Wai.label . responseJsonError

postMLSConvDisabled :: TestM ()
postMLSConvDisabled = do
  alice <- randomQualifiedUser
  withMLSDisabled $
    postConvQualified
      (qUnqualified alice)
      (Just (ClientId 0))
      defNewMLSConv
      !!! assertMLSNotEnabled

postMLSMessageDisabled :: TestM ()
postMLSMessageDisabled = do
  [alice, bob] <- createAndConnectUsers [Nothing, Nothing]
  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    void $ uploadNewKeyPackage bob1
    void $ setupMLSGroup alice1
    mp <- createAddCommit alice1 [bob]
    bundle <- createBundle mp
    withMLSDisabled $
      localPostCommitBundle (mpSender mp) bundle
        !!! assertMLSNotEnabled

postMLSBundleDisabled :: TestM ()
postMLSBundleDisabled = do
  [alice, bob] <- createAndConnectUsers [Nothing, Nothing]
  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    void $ uploadNewKeyPackage bob1
    void $ setupMLSGroup alice1
    mp <- createAddCommit alice1 [bob]
    withMLSDisabled $ do
      bundle <- createBundle mp
      localPostCommitBundle (mpSender mp) bundle
        !!! assertMLSNotEnabled

getGroupInfoDisabled :: TestM ()
getGroupInfoDisabled = do
  [alice, bob] <- createAndConnectUsers [Nothing, Nothing]
  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    void $ uploadNewKeyPackage bob1
    (_, qcnv) <- setupMLSGroup alice1
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle

    withMLSDisabled $
      localGetGroupInfo (qUnqualified alice) (fmap Conv qcnv)
        !!! assertMLSNotEnabled

deleteSubConversationDisabled :: TestM ()
deleteSubConversationDisabled = do
  alice <- randomUser
  cnvId <- Qualified <$> randomId <*> pure (Domain "www.example.com")
  let scnvId = SubConvId "conference"
      dsc =
        DeleteSubConversationRequest
          (GroupId "MLS")
          (Epoch 0)
  withMLSDisabled $
    deleteSubConv alice cnvId scnvId dsc !!! assertMLSNotEnabled

testExternalCommitSameClientSubConv :: TestM ()
testExternalCommitSameClientSubConv = do
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    void $ uploadNewKeyPackage bob1
    (_, qcnv) <- setupMLSGroup alice1
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle

    let subId = SubConvId "conference"

    -- alice1 and bob1 create and join a subconversation, respectively
    qsub <- createSubConv qcnv alice1 subId
    void $
      createExternalCommit bob1 Nothing qsub
        >>= sendAndConsumeCommitBundle

    Just (_, idxBob1) <- find (\(ci, _) -> ci == bob1) <$> getClientsFromGroupState alice1 bob

    -- bob1 leaves and immediately rejoins
    mlsBracket [alice1, bob1] $ \[wsA, wsB] -> do
      void $ leaveCurrentConv bob1 qsub
      WS.assertMatchN_ (5 # WS.Second) [wsA] $
        wsAssertBackendRemoveProposal bob qsub idxBob1
      void $
        createExternalCommit bob1 Nothing qsub
          >>= sendAndConsumeCommitBundle
      WS.assertNoEvent (2 # WS.Second) [wsB]

testJoinSubNonMemberClient :: TestM ()
testJoinSubNonMemberClient = do
  [alice, bob] <- createAndConnectUsers [Nothing, Nothing]

  runMLSTest $ do
    [alice1, alice2, bob1] <-
      traverse createMLSClient [alice, alice, bob]
    traverse_ uploadNewKeyPackage [bob1, alice2]
    (_, qcnv) <- setupMLSGroup alice1
    void $ createAddCommit alice1 [alice] >>= sendAndConsumeCommitBundle

    qcs <- createSubConv qcnv alice1 (SubConvId "conference")

    -- now Bob attempts to get the group info so he can join via external commit
    -- with his own client, but he cannot because he is not a member of the
    -- parent conversation
    localGetGroupInfo (ciUser bob1) qcs
      !!! const 404 === statusCode

testAddClientSubConvFailure :: TestM ()
testAddClientSubConvFailure = do
  [alice, bob] <- createAndConnectUsers [Nothing, Nothing]
  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    void $ uploadNewKeyPackage bob1
    (_, qcnv) <- setupMLSGroup alice1
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle

    let subId = SubConvId "conference"
    void $ createSubConv qcnv alice1 subId

    void $ uploadNewKeyPackage bob1

    commit <- createAddCommit alice1 [bob]
    (createBundle commit >>= localPostCommitBundle (mpSender commit))
      !!! do
        const 400 === statusCode
        const (Just "Add proposals in subconversations are not supported")
          === fmap Wai.message . responseJsonError

    finalSub <-
      liftTest $
        responseJsonError
          =<< getSubConv (qUnqualified alice) qcnv subId
            <!! const 200 === statusCode
    liftIO $ do
      assertEqual
        "The subconversation has Bob in it, while it shouldn't"
        [alice1]
        (pscMembers finalSub)
      assertEqual
        "The subconversation epoch has moved beyond 1"
        (Epoch 1)
        (fromJust (pscActiveData finalSub)).epoch

-- FUTUREWORK: implement the following test

testRemoveClientSubConv :: TestM ()
testRemoveClientSubConv = pure ()

testJoinRemoteSubConv :: TestM ()
testJoinRemoteSubConv = do
  [alice, bob] <- createAndConnectUsers [Just "alice.example.com", Nothing]

  runMLSTest $ do
    alice1 <- createFakeMLSClient alice
    bob1 <- createMLSClient bob
    void $ uploadNewKeyPackage bob1

    -- setup fake group for the subconversation
    let subId = SubConvId "conference"
    (_subGroupId, qcnv) <- setupFakeMLSGroup alice1 (Just subId)
    let qcs = fmap (flip SubConv subId) qcnv
    initialCommit <- createPendingProposalCommit alice1

    receiveOnConvUpdated qcnv alice bob

    -- bob joins subconversation
    let pgs = mpGroupInfo initialCommit
    let mock =
          ("send-mls-commit-bundle" ~> MLSMessageResponseUpdates [])
            <|> queryGroupStateMock (fold pgs) bob
            <|> sendMessageMock
    (_, reqs) <- withTempMockFederator' mock $ do
      commit <- createExternalCommit bob1 Nothing qcs
      sendAndConsumeCommitBundle commit

    -- check that commit bundle is sent to remote backend
    fr <- assertOne (filter ((== "send-mls-commit-bundle") . frRPC) reqs)
    liftIO $ do
      mmsr :: MLSMessageSendRequest <- assertJust (Aeson.decode (frBody fr))
      mmsr.convOrSubId @?= qUnqualified qcs
      mmsr.sender @?= ciUser bob1
      mmsr.senderClient @?= ciClient bob1

testRemoteSubConvNotificationWhenUserJoins :: TestM ()
testRemoteSubConvNotificationWhenUserJoins = do
  [alice, bob] <- createAndConnectUsers [Nothing, Just "bob.example.com"]

  runMLSTest $ do
    alice1 <- createMLSClient alice
    bob1 <- createFakeMLSClient bob

    (_, qcnv) <- setupMLSGroup alice1
    gsBackup <- getClientGroupState alice1
    void $ createPendingProposalCommit alice1 >>= sendAndConsumeCommitBundle
    let subId = SubConvId "conference"
    s <- State.get
    void $ createSubConv qcnv alice1 subId

    -- revert first commit and subconv
    setClientGroupState alice1 gsBackup

    State.put s

    void $
      withTempMockFederator' (receiveCommitMock [bob1] <|> welcomeMock) $
        createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle

testSendMessageSubConv :: TestM ()
testSendMessageSubConv = do
  [alice, bob] <- createAndConnectUsers [Nothing, Nothing]

  runMLSTest $ do
    [alice1, bob1, bob2] <- traverse createMLSClient [alice, bob, bob]
    traverse_ uploadNewKeyPackage [bob1, bob2]
    (_, qcnv) <- setupMLSGroup alice1
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle

    qcs <- createSubConv qcnv bob1 (SubConvId "conference")

    void $ createExternalCommit alice1 Nothing qcs >>= sendAndConsumeCommitBundle
    void $ createExternalCommit bob2 Nothing qcs >>= sendAndConsumeCommitBundle

    message <- createApplicationMessage alice1 "some text"
    mlsBracket [bob1, bob2] $ \wss -> do
      events <- sendAndConsumeMessage message
      liftIO $ events @?= []
      liftIO $
        WS.assertMatchN_ (5 # WS.Second) wss $ \n -> do
          wsAssertMLSMessage qcs alice (mpMessage message) n

testGetRemoteSubConv :: Bool -> TestM ()
testGetRemoteSubConv isAMember = do
  alice <- randomQualifiedUser
  let remoteDomain = Domain "faraway.example.com"
  conv <- randomId
  let qconv = Qualified conv remoteDomain
      sconv = SubConvId "conference"
      fakeSubConv =
        PublicSubConversation
          { pscParentConvId = qconv,
            pscSubConvId = sconv,
            pscGroupId = GroupId "deadbeef",
            pscActiveData = Nothing,
            pscMembers = []
          }
  let mock = do
        guardRPC "get-sub-conversation"
        mockReply $
          if isAMember
            then GetSubConversationsResponseSuccess fakeSubConv
            else GetSubConversationsResponseError ConvNotFound
  (_, reqs) <-
    withTempMockFederator' mock $
      getSubConv (qUnqualified alice) qconv sconv
        <!! const (if isAMember then 200 else 404) === statusCode
  fedSubConv <- assertOne (filter ((== "get-sub-conversation") . frRPC) reqs)
  let req :: Maybe GetSubConversationsRequest = Aeson.decode (frBody fedSubConv)
  liftIO $
    req
      @?= Just (GetSubConversationsRequest (qUnqualified alice) conv sconv)

testRemoteMemberGetSubConv :: Bool -> TestM ()
testRemoteMemberGetSubConv isAMember = do
  -- alice is local, bob is remote
  -- alice creates a local conversation and invites bob
  -- bob gets a subconversation via federated enpdoint

  let bobDomain = Domain "faraway.example.com"
  [alice, bob] <- createAndConnectUsers [Nothing, Just (domainText bobDomain)]

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]

    (_groupId, qcnv) <- setupMLSGroup alice1
    kpb <- claimKeyPackages alice1 bob
    mp <- createAddCommit alice1 [bob]

    let mock = receiveCommitMock [bob1] <|> welcomeMock <|> claimKeyPackagesMock kpb
    void . withTempMockFederator' mock $
      sendAndConsumeCommitBundle mp

    let subconv = SubConvId "conference"

    randUser <- randomId
    let gscr =
          GetSubConversationsRequest
            { gsreqUser = if isAMember then qUnqualified bob else randUser,
              gsreqConv = qUnqualified qcnv,
              gsreqSubConv = subconv
            }

    fedGalleyClient <- view tsFedGalleyClient
    res <- runFedClient @"get-sub-conversation" fedGalleyClient bobDomain gscr

    liftTest $ do
      if isAMember
        then do
          sub <- expectSubConvSuccess res
          liftIO $ do
            pscParentConvId sub @?= qcnv
            pscSubConvId sub @?= subconv
        else do
          expectSubConvError ConvNotFound res
  where
    expectSubConvSuccess :: GetSubConversationsResponse -> TestM PublicSubConversation
    expectSubConvSuccess (GetSubConversationsResponseSuccess fakeSubConv) = pure fakeSubConv
    expectSubConvSuccess (GetSubConversationsResponseError err) = liftIO $ assertFailure ("Unexpected GetSubConversationsResponseError: " <> show err)

    expectSubConvError :: GalleyError -> GetSubConversationsResponse -> TestM ()
    expectSubConvError _errExpected (GetSubConversationsResponseSuccess _) = liftIO $ assertFailure "Unexpected GetSubConversationsResponseSuccess"
    expectSubConvError errExpected (GetSubConversationsResponseError err) = liftIO $ err @?= errExpected

-- In this test case, Alice creates a subconversation, Bob joins and Alice
-- leaves. The leaving causes the backend to generate an external remove
-- proposal for the client by Alice. Next, Bob does not commit (simulating his
-- client crashing), and then deleting the subconversation after coming back up.
-- Then Bob creates a subconversation with the same subconversation ID and the
-- test asserts that both Alice and Bob get no events, which means the backend
-- does not resubmit the pending remove proposal for Alice's client.
testJoinDeletedSubConvWithRemoval :: TestM ()
testJoinDeletedSubConvWithRemoval = do
  [alice, bob] <- createAndConnectUsers [Nothing, Nothing]
  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    void $ uploadNewKeyPackage bob1
    (_, qcnv) <- setupMLSGroup alice1
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle
    let subConvId = SubConvId "conference"
    qsconvId <- createSubConv qcnv alice1 subConvId
    void $
      createExternalCommit bob1 Nothing qsconvId
        >>= sendAndConsumeCommitBundle
    liftTest $
      leaveSubConv (ciUser alice1) (ciClient alice1) qcnv subConvId
        !!! const 200 === statusCode
    -- no committing by Bob of the backend-generated remove proposal for alice1
    -- (simulating his client crashing)

    do
      sub <-
        liftTest $
          responseJsonError
            =<< getSubConv (qUnqualified bob) qcnv subConvId
              <!! const 200 === statusCode
      let Just activeData = pscActiveData sub
      let dsc = DeleteSubConversationRequest (pscGroupId sub) activeData.epoch
      liftTest $
        deleteSubConv (qUnqualified bob) qcnv subConvId dsc
          !!! const 200 === statusCode

    mlsBracket [alice1, bob1] $ \wss -> do
      void $ createSubConv qcnv bob1 subConvId
      void . liftIO $ WS.assertNoEvent (3 # WS.Second) wss

testDeleteSubConvStale :: TestM ()
testDeleteSubConvStale = do
  alice <- randomQualifiedUser
  let sconv = SubConvId "conference"
  (qcnv, sub) <- runMLSTest $ do
    alice1 <- createMLSClient alice
    (_, qcnv) <- setupMLSGroup alice1
    sub <-
      liftTest $
        responseJsonError
          =<< getSubConv (qUnqualified alice) qcnv sconv
            <!! do const 200 === statusCode

    resetGroup alice1 (fmap (flip SubConv sconv) qcnv) (pscGroupId sub)

    void $
      createPendingProposalCommit alice1 >>= sendAndConsumeCommitBundle
    pure (qcnv, sub)

  -- the commit was made, yet the epoch for the request body is old
  let epoch = maybe (Epoch 0) (.epoch) (pscActiveData sub)
  let dsc = DeleteSubConversationRequest (pscGroupId sub) epoch
  deleteSubConv (qUnqualified alice) qcnv sconv dsc
    !!! do const 409 === statusCode

testDeleteRemoteSubConv :: Bool -> TestM ()
testDeleteRemoteSubConv isAMember = do
  alice <- randomQualifiedUser
  let remoteDomain = Domain "faraway.example.com"
  conv <- randomId
  let qconv = Qualified conv remoteDomain
      sconv = SubConvId "conference"
      groupId = GroupId "deadbeef"
      epoch = Epoch 0
      expectedReq =
        DeleteSubConversationFedRequest
          { dscreqUser = qUnqualified alice,
            dscreqConv = conv,
            dscreqSubConv = sconv,
            dscreqGroupId = groupId,
            dscreqEpoch = epoch
          }

  let mock = do
        guardRPC "delete-sub-conversation"
        mockReply $
          if isAMember
            then DeleteSubConversationResponseSuccess
            else DeleteSubConversationResponseError ConvNotFound
      dsc = DeleteSubConversationRequest groupId epoch

  (_, reqs) <-
    withTempMockFederator' mock $
      deleteSubConv (qUnqualified alice) qconv sconv dsc
        <!! const (if isAMember then 200 else 404) === statusCode
  do
    actualReq <- assertOne (filter ((== "delete-sub-conversation") . frRPC) reqs)
    let req :: Maybe DeleteSubConversationFedRequest =
          Aeson.decode (frBody actualReq)
    liftIO $ req @?= Just expectedReq

testLastLeaverSubConv :: TestM ()
testLastLeaverSubConv = do
  alice <- randomQualifiedUser

  runMLSTest $ do
    [alice1, alice2] <- traverse createMLSClient [alice, alice]
    void $ uploadNewKeyPackage alice2
    (_, qcnv) <- setupMLSGroup alice1
    void $ createAddCommit alice1 [alice] >>= sendAndConsumeCommitBundle

    let subId = SubConvId "conference"
    qsub <- createSubConv qcnv alice1 subId
    prePsc <-
      liftTest $
        responseJsonError
          =<< getSubConv (qUnqualified alice) qcnv subId
            <!! do
              const 200 === statusCode
    void $ leaveCurrentConv alice1 qsub

    psc <-
      liftTest $
        responseJsonError
          =<< getSubConv (qUnqualified alice) qcnv subId
            <!! do
              const 200 === statusCode
    liftIO $ do
      pscActiveData psc @?= Nothing
      assertBool "group ID unchanged" $ pscGroupId prePsc /= pscGroupId psc
      length (pscMembers psc) @?= 0

testLeaveSubConvNonMember :: TestM ()
testLeaveSubConvNonMember = do
  [alice, bob] <- createAndConnectUsers [Nothing, Nothing]

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    void $ uploadNewKeyPackage bob1
    (_, qcnv) <- setupMLSGroup alice1
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle

    let subId = SubConvId "conference"
    _qsub <- createSubConv qcnv bob1 subId

    -- alice attempts to leave
    liftTest $ do
      e <-
        responseJsonError
          =<< leaveSubConv (ciUser alice1) (ciClient alice1) qcnv subId
            <!! const 400 === statusCode
      liftIO $ Wai.label e @?= "mls-protocol-error"

    -- alice attempts to leave a non-existing subconversation
    liftTest $ do
      e <-
        responseJsonError
          =<< leaveSubConv (ciUser alice1) (ciClient alice1) qcnv (SubConvId "foo")
            <!! const 404 === statusCode
      liftIO $ Wai.label e @?= "no-conversation"

testLeaveRemoteSubConv :: TestM ()
testLeaveRemoteSubConv = do
  -- setup fake remote conversation
  [alice, bob] <- createAndConnectUsers [Just "alice.example.com", Nothing]
  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]

    -- setup fake group for the subconversation
    let subId = SubConvId "conference"
    (_subGroupId, qcnv) <- setupFakeMLSGroup alice1 (Just subId)
    -- TODO: refactor setupFakeMLSGroup to make it consistent with createSubConv
    let qcs = fmap (flip SubConv subId) qcnv
    initialCommit <- createPendingProposalCommit alice1

    -- inform backend about the main conversation
    receiveOnConvUpdated qcnv alice bob

    let pgs = mpGroupInfo initialCommit
    let mock =
          ("send-mls-commit-bundle" ~> MLSMessageResponseUpdates [])
            <|> queryGroupStateMock (fold pgs) bob
            <|> sendMessageMock
            <|> ("leave-sub-conversation" ~> LeaveSubConversationResponseOk)
    (_, reqs) <- withTempMockFederator' mock $ do
      -- bob joins subconversation
      void $ createExternalCommit bob1 Nothing qcs >>= sendAndConsumeCommitBundle

      -- bob leaves
      liftTest $
        leaveSubConv (ciUser bob1) (ciClient bob1) qcnv subId
          !!! const 200 === statusCode

    -- check that leave-sub-conversation is called
    void $ assertOne (filter ((== "leave-sub-conversation") . frRPC) reqs)

testRemoveUserParent :: TestM ()
testRemoveUserParent = do
  [alice, bob, charlie] <- createAndConnectUsers [Nothing, Nothing, Nothing]
  let subname = SubConvId "conference"

  (qcnv, [alice1, bob1, bob2, _charlie1, _charlie2]) <- runMLSTest $
    do
      clients@[alice1, bob1, bob2, charlie1, charlie2] <-
        traverse
          createMLSClient
          [alice, bob, bob, charlie, charlie]
      traverse_ uploadNewKeyPackage [bob1, bob2, charlie1, charlie2]
      (_, qcnv) <- setupMLSGroup alice1
      void $ createAddCommit alice1 [bob, charlie] >>= sendAndConsumeCommitBundle

      void $ createSubConv qcnv bob1 subname
      let qcs = fmap (flip SubConv subname) qcnv

      -- all clients join
      for_ [alice1, bob2, charlie1, charlie2] $ \c ->
        void $ createExternalCommit c Nothing qcs >>= sendAndConsumeCommitBundle

      pure (qcnv, clients)

  -- charlie leaves the main conversation
  deleteMemberQualified (qUnqualified charlie) charlie qcnv
    !!! const 200 === statusCode

  getSubConv (qUnqualified charlie) qcnv subname
    !!! const 403 === statusCode

  sub :: PublicSubConversation <-
    responseJsonError
      =<< getSubConv (qUnqualified bob) qcnv subname
        <!! const 200 === statusCode
  liftIO $
    assertEqual
      "subconv membership mismatch after removal"
      (sort [bob1, bob2, alice1])
      (sort $ pscMembers sub)

testRemoveCreatorParent :: TestM ()
testRemoveCreatorParent = do
  [alice, bob, charlie] <- createAndConnectUsers [Nothing, Nothing, Nothing]
  let subname = SubConvId "conference"

  (qcnv, [_alice1, bob1, bob2, charlie1, charlie2]) <- runMLSTest $
    do
      clients@[alice1, bob1, bob2, charlie1, charlie2] <-
        traverse
          createMLSClient
          [alice, bob, bob, charlie, charlie]
      traverse_ uploadNewKeyPackage [bob1, bob2, charlie1, charlie2]
      (_, qcnv) <- setupMLSGroup alice1
      void $ createAddCommit alice1 [bob, charlie] >>= sendAndConsumeCommitBundle

      void $ createSubConv qcnv alice1 subname
      let qcs = fmap (flip SubConv subname) qcnv

      -- all clients join
      for_ [bob1, bob2, charlie1, charlie2] $ \c ->
        void $ createExternalCommit c Nothing qcs >>= sendAndConsumeCommitBundle

      pure (qcnv, clients)

  -- creator leaves the main conversation
  deleteMemberQualified (qUnqualified alice) alice qcnv
    !!! const 200 === statusCode

  getSubConv (qUnqualified alice) qcnv subname
    !!! const 403 === statusCode

  -- charlie sees updated memberlist
  sub :: PublicSubConversation <-
    responseJsonError
      =<< getSubConv (qUnqualified charlie) qcnv subname
        <!! const 200 === statusCode
  liftIO $
    assertEqual
      "1. subconv membership mismatch after removal"
      (sort [charlie1, charlie2, bob1, bob2])
      (sort $ pscMembers sub)

  -- bob also sees updated memberlist
  sub1 :: PublicSubConversation <-
    responseJsonError
      =<< getSubConv (qUnqualified bob) qcnv subname
        <!! const 200 === statusCode
  liftIO $
    assertEqual
      "2. subconv membership mismatch after removal"
      (sort [charlie1, charlie2, bob1, bob2])
      (sort $ pscMembers sub1)
