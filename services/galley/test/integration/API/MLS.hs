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
import Bilge hiding (head)
import Bilge.Assert
import Cassandra hiding (Set)
import Control.Exception (throw)
import Control.Lens (view)
import Control.Lens.Extras
import qualified Control.Monad.State as State
import Crypto.Error
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.Aeson as Aeson
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Domain
import Data.Id
import Data.Json.Util hiding ((#))
import Data.List1 hiding (head)
import qualified Data.Map as Map
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import Data.Singletons
import Data.String.Conversions
import qualified Data.Text as T
import Data.Time
import Federator.MockServer hiding (withTempMockFederator)
import Imports
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai.Utilities.Error as Wai
import Test.QuickCheck (Arbitrary (arbitrary), generate)
import Test.Tasty
import Test.Tasty.Cannon (TimeoutUnit (Second), (#))
import qualified Test.Tasty.Cannon as WS
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
import Wire.API.MLS.Keys
import Wire.API.MLS.Message
import Wire.API.MLS.Serialisation
import Wire.API.MLS.SubConversation
import Wire.API.MLS.Welcome
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
        [ test s "local welcome" testLocalWelcome,
          test s "local welcome (client with no public key)" testWelcomeNoKey,
          test s "remote welcome" testRemoteWelcome,
          test s "post a remote MLS welcome message" sendRemoteMLSWelcome,
          test s "post a remote MLS welcome message (key package ref not found)" sendRemoteMLSWelcomeKPNotFound
        ],
      testGroup
        "Creation"
        [ test s "fail to create MLS conversation" postMLSConvFail,
          test s "create MLS conversation" postMLSConvOk
        ],
      testGroup
        "Deletion"
        [ test s "delete a MLS conversation" testDeleteMLSConv
        ],
      testGroup
        "Commit"
        [ test s "add user to a conversation" testAddUser,
          test s "add user with an incomplete welcome" testAddUserWithBundleIncompleteWelcome,
          test s "add user (not connected)" testAddUserNotConnected,
          test s "add user (partial client list)" testAddUserPartial,
          test s "add client of existing user" testAddClientPartial,
          test s "add user with some non-MLS clients" testAddUserWithProteusClients,
          test s "send a stale commit" testStaleCommit,
          test s "add remote user to a conversation" testAddRemoteUser,
          test s "return error when commit is locked" testCommitLock,
          test s "add user to a conversation with proposal + commit" testAddUserBareProposalCommit,
          test s "post commit that references an unknown proposal" testUnknownProposalRefCommit,
          test s "post commit that is not referencing all proposals" testCommitNotReferencingAllProposals,
          test s "admin removes user from a conversation" testAdminRemovesUserFromConv,
          test s "admin removes user from a conversation but doesn't list all clients" testRemoveClientsIncomplete
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
              test s "send remote application message" testRemoteAppMessage,
              test s "another participant sends an application message" testAppMessage2,
              test s "send message, some remotes are reachable" testAppMessageSomeReachable,
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
            ],
          testGroup
            "Remote Sender/Remote Conversation"
            [ test s "POST /federation/on-mls-message-sent" testRemoteToRemote
            ] -- all is mocked
        ],
      testGroup
        "Proposal"
        [ test s "add a new client to a non-existing conversation" propNonExistingConv,
          test s "add a new client to an existing conversation" propExistingConv,
          test s "add a new client in an invalid epoch" propInvalidEpoch,
          test s "forward an unsupported proposal" propUnsupported
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
      test s "public keys" testPublicKeys,
      testGroup
        "GroupInfo"
        [ test s "get group info for a local conversation" testGetGroupInfoOfLocalConv,
          test s "get group info for a remote conversation" testGetGroupInfoOfRemoteConv,
          test s "get group info for a remote user" testFederatedGetGroupInfo
        ],
      testGroup
        "CommitBundle"
        [ test s "add user with a commit bundle" testAddUserWithBundle,
          test s "add user with a commit bundle to a remote conversation" testAddUserToRemoteConvWithBundle,
          test s "remote user posts commit bundle" testRemoteUserPostsCommitBundle
        ],
      testGroup
        "Self conversation"
        [ test s "create a self conversation" testSelfConversation,
          test s "do not list a self conversation below v3" $ testSelfConversationList True,
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
          test s "cannot get group info" getGroupInfoDisabled
        ]
    ]

postMLSConvFail :: TestM ()
postMLSConvFail = do
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice
  let aliceClient = newClientId 0
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
  let aliceClient = newClientId 0
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
    qcid <- assertConv rsp RegularConv alice qalice [] (Just nameMaxSize) Nothing
    checkConvCreateEvent (qUnqualified qcid) wsA

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

testLocalWelcome :: TestM ()
testLocalWelcome = do
  users@[alice, bob] <- createAndConnectUsers [Nothing, Nothing]
  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient users
    void $ uploadNewKeyPackage bob1
    (_, qcnv) <- setupMLSGroup alice1
    commit <- createAddCommit alice1 [bob]
    welcome <- liftIO $ case mpWelcome commit of
      Nothing -> assertFailure "Expected welcome message"
      Just w -> pure w
    events <- mlsBracket [bob1] $ \wss -> do
      es <- sendAndConsumeCommit commit

      WS.assertMatchN_ (5 # Second) wss $
        wsAssertMLSWelcome (cidQualifiedUser bob1) welcome

      pure es

    event <- assertOne events
    liftIO $ assertJoinEvent qcnv alice [bob] roleNameWireMember event

testWelcomeNoKey :: TestM ()
testWelcomeNoKey = do
  users <- createAndConnectUsers [Nothing, Nothing]
  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient users
    void $ setupMLSGroup alice1

    -- add bob using an "out-of-band" key package
    (_, ref) <- generateKeyPackage bob1
    kp <- keyPackageFile bob1 ref
    commit <- createAddCommitWithKeyPackages alice1 [(bob1, kp)]
    welcome <- liftIO $ case mpWelcome commit of
      Nothing -> assertFailure "Expected welcome message"
      Just w -> pure w

    err <-
      responseJsonError
        =<< postWelcome (ciUser alice1) welcome
          <!! do
            const 404 === statusCode
    liftIO $ Wai.label err @?= "mls-key-package-ref-not-found"

testRemoteWelcome :: TestM ()
testRemoteWelcome = do
  [alice, bob] <- createAndConnectUsers [Nothing, Just "bob.example.com"]

  runMLSTest $ do
    alice1 <- createMLSClient alice
    _bob1 <- createFakeMLSClient bob

    void $ setupMLSGroup alice1
    commit <- createAddCommit alice1 [bob]
    welcome <- liftIO $ case mpWelcome commit of
      Nothing -> assertFailure "Expected welcome message"
      Just w -> pure w
    (_, reqs) <-
      withTempMockFederator' welcomeMock $
        postWelcome (ciUser (mpSender commit)) welcome
          !!! const 201 === statusCode
    consumeWelcome welcome
    fedWelcome <- assertOne (filter ((== "mls-welcome") . frRPC) reqs)
    let req :: Maybe MLSWelcomeRequest = Aeson.decode (frBody fedWelcome)
    liftIO $ req @?= (Just . MLSWelcomeRequest . Base64ByteString) welcome

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
      for_ (zip bobClients wss) $ \(c, ws) ->
        WS.assertMatch (5 # Second) ws $
          wsAssertMLSWelcome (cidQualifiedUser c) welcome
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

  returnedGS <-
    fmap responseBody $
      getGroupInfo (qUnqualified alice) qcnv
        <!! const 200 === statusCode
  liftIO $ assertBool "Commit does not contain a public group State" (isJust (mpPublicGroupState commit))
  liftIO $ mpPublicGroupState commit @=? LBS.toStrict <$> returnedGS

testAddUserWithBundleIncompleteWelcome :: TestM ()
testAddUserWithBundleIncompleteWelcome = do
  [alice, bob] <- createAndConnectUsers [Nothing, Nothing]

  runMLSTest $ do
    (alice1 : bobClients) <- traverse createMLSClient [alice, bob, bob]
    traverse_ uploadNewKeyPackage bobClients
    void $ setupMLSGroup alice1

    -- create commit, but remove first recipient from welcome message
    commit <- do
      commit <- createAddCommit alice1 [bob]
      liftIO $ do
        welcome <- assertJust (mpWelcome commit)
        w <- either (assertFailure . T.unpack) pure $ decodeMLS' welcome
        let w' = w {welSecrets = take 1 (welSecrets w)}
            welcome' = encodeMLS' w'
            commit' = commit {mpWelcome = Just welcome'}
        pure commit'

    bundle <- createBundle commit
    err <-
      responseJsonError
        =<< postCommitBundle (mpSender commit) bundle
          <!! const 400 === statusCode
    liftIO $ Wai.label err @?= "mls-welcome-mismatch"

testAddUser :: TestM ()
testAddUser = do
  [alice, bob] <- createAndConnectUsers [Nothing, Nothing]

  qcnv <- runMLSTest $ do
    [alice1, bob1, bob2] <- traverse createMLSClient [alice, bob, bob]
    traverse_ uploadNewKeyPackage [bob1, bob2]
    (_, qcnv) <- setupMLSGroup alice1
    events <- createAddCommit alice1 [bob] >>= sendAndConsumeCommit
    event <- assertOne events
    liftIO $ assertJoinEvent qcnv alice [bob] roleNameWireMember event
    pure qcnv

  -- check that bob can now see the conversation
  convs <- getAllConvs (qUnqualified bob)
  liftIO $
    assertBool
      "Users added to an MLS group should find it when listing conversations"
      (qcnv `elem` map cnvQualifiedId convs)

testAddUserNotConnected :: TestM ()
testAddUserNotConnected = do
  users@[alice, bob] <- replicateM 2 randomQualifiedUser

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient users
    void $ uploadNewKeyPackage bob1
    void $ setupMLSGroup alice1
    -- add unconnected user with a commit
    commit <- createAddCommit alice1 [bob]
    err <- mlsBracket [alice1, bob1] $ \wss -> do
      err <-
        responseJsonError
          =<< postMessage (mpSender commit) (mpMessage commit)
            <!! const 403 === statusCode
      void . liftIO $ WS.assertNoEvent (1 # WS.Second) wss
      pure err
    liftIO $ Wai.label err @?= "not-connected"

    -- now connect and retry
    liftTest $ connectUsers (qUnqualified alice) (pure (qUnqualified bob))
    void $ sendAndConsumeCommit commit

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
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommit

testAddUserPartial :: TestM ()
testAddUserPartial = do
  [alice, bob, charlie] <- createAndConnectUsers (replicate 3 Nothing)

  runMLSTest $ do
    -- Bob has 3 clients, Charlie has 2
    alice1 <- createMLSClient alice
    bobClients@[_bob1, _bob2, bob3] <- replicateM 3 (createMLSClient bob)
    charlieClients <- replicateM 2 (createMLSClient charlie)

    -- Only the first 2 clients of Bob's have uploaded key packages
    traverse_ uploadNewKeyPackage (take 2 bobClients <> charlieClients)

    -- alice adds bob's first 2 clients
    void $ setupMLSGroup alice1
    commit <- createAddCommit alice1 [bob, charlie]

    -- before alice can commit, bob3 uploads a key package
    void $ uploadNewKeyPackage bob3

    -- alice sends a commit now, and should get a conflict error
    err <-
      responseJsonError
        =<< postMessage (mpSender commit) (mpMessage commit)
          <!! const 409 === statusCode
    liftIO $ Wai.label err @?= "mls-client-mismatch"

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
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommit

    -- now bob2 and bob3 upload key packages, and alice adds bob2 only
    kp <- uploadNewKeyPackage bob2 >>= keyPackageFile bob2
    void $ uploadNewKeyPackage bob3
    void $
      createAddCommitWithKeyPackages alice1 [(bob2, kp)]
        >>= sendAndConsumeCommit

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
    createAddCommit alice1 [bob] >>= void . sendAndConsumeCommit

    -- Alice creates a commit that adds bob2
    bob2 <- createMLSClient bob
    -- upload key packages
    void $ uploadNewKeyPackage bob2
    mp <- createAddCommit alice1 [bob]
    -- and the corresponding commit is sent from Bob instead of Alice
    err <-
      responseJsonError
        =<< postMessage bob1 (mpMessage mp)
          <!! const 400 === statusCode
    liftIO $ Wai.label err @?= "mls-client-sender-user-mismatch"

testAddUsersToProteus :: TestM ()
testAddUsersToProteus = do
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)
  void $ postConvQualified (qUnqualified alice) Nothing defNewProteusConv
  groupId <-
    liftIO $ fmap (GroupId . BS.pack) (replicateM 32 (generate arbitrary))
  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    void $ uploadNewKeyPackage bob1
    createGroup alice1 groupId
    mp <- createAddCommit alice1 [bob]
    err <-
      responseJsonError
        =<< postMessage alice1 (mpMessage mp) <!! const 404 === statusCode
    liftIO $ Wai.label err @?= "no-conversation"

testAddUsersDirectly :: TestM ()
testAddUsersDirectly = do
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)
  charlie <- randomQualifiedUser
  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    void $ uploadNewKeyPackage bob1
    qcnv <- snd <$> setupMLSGroup alice1
    createAddCommit alice1 [bob] >>= void . sendAndConsumeCommit
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
    createAddCommit alice1 [bob] >>= void . sendAndConsumeCommit
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
    createAddCommit alice1 [bob] >>= void . sendAndConsumeCommit
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

testStaleCommit :: TestM ()
testStaleCommit = do
  (alice : users) <- createAndConnectUsers (replicate 5 Nothing)
  let (users1, users2) = splitAt 2 users

  runMLSTest $ do
    (alice1 : clients) <- traverse createMLSClient (alice : users)
    traverse_ uploadNewKeyPackage clients
    void $ setupMLSGroup alice1

    -- add the first batch of users to the conversation
    void $ createAddCommit alice1 users1 >>= sendAndConsumeCommit

    -- now roll back alice1 and try to add the second batch of users
    void $ rollBackClient alice1
    commit <- createAddCommit alice1 users2
    err <-
      responseJsonError
        =<< postMessage (mpSender commit) (mpMessage commit)
          <!! const 409 === statusCode
    liftIO $ Wai.label err @?= "mls-stale-message"

testAddRemoteUser :: TestM ()
testAddRemoteUser = do
  users@[alice, bob] <- createAndConnectUsers [Nothing, Just "bob.example.com"]
  (events, reqs, qcnv) <- runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient users
    (_, qcnv) <- setupMLSGroup alice1

    commit <- createAddCommit alice1 [bob]
    (events, reqs) <-
      withTempMockFederator' (receiveCommitMock [bob1] <|> welcomeMock) $
        sendAndConsumeCommit commit
    pure (events, reqs, qcnv)

  liftIO $ do
    req <- assertOne $ filter ((== "on-conversation-updated") . frRPC) reqs
    frTargetDomain req @?= qDomain bob
    bdy <- case Aeson.eitherDecode (frBody req) of
      Right b -> pure b
      Left e -> assertFailure $ "Could not parse on-conversation-updated request body: " <> e
    cuOrigUserId bdy @?= alice
    cuConvId bdy @?= qUnqualified qcnv
    cuAlreadyPresentUsers bdy @?= [qUnqualified bob]
    cuAction bdy
      @?= SomeConversationAction
        SConversationJoinTag
        ConversationJoin
          { cjUsers = pure bob,
            cjRole = roleNameWireMember
          }

  liftIO $ do
    event <- assertOne events
    assertJoinEvent qcnv alice [bob] roleNameWireMember event

testCommitLock :: TestM ()
testCommitLock = do
  users <- createAndConnectUsers (replicate 4 Nothing)

  runMLSTest $ do
    [alice1, bob1, charlie1, dee1] <- traverse createMLSClient users
    (groupId, _) <- setupMLSGroup alice1
    traverse_ uploadNewKeyPackage [bob1, charlie1, dee1]

    -- alice adds add bob
    void $ createAddCommit alice1 [cidQualifiedUser bob1] >>= sendAndConsumeCommit

    -- alice adds charlie
    void $ createAddCommit alice1 [cidQualifiedUser charlie1] >>= sendAndConsumeCommit

    -- simulate concurrent commit by blocking epoch
    casClient <- view tsCass
    runClient casClient $ insertLock groupId (Epoch 2)

    -- commit should fail due to competing lock
    do
      commit <- createAddCommit alice1 [cidQualifiedUser dee1]
      err <-
        responseJsonError
          =<< postMessage alice1 (mpMessage commit)
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

testAddUserBareProposalCommit :: TestM ()
testAddUserBareProposalCommit = do
  users <- createAndConnectUsers (replicate 2 Nothing)
  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient users
    (_, qcnv) <- setupMLSGroup alice1
    void $ uploadNewKeyPackage bob1

    createAddProposals alice1 [cidQualifiedUser bob1]
      >>= traverse_ sendAndConsumeMessage
    commit <- createPendingProposalCommit alice1
    void $ assertJust (mpWelcome commit)
    void $ sendAndConsumeCommit commit

    -- check that bob can now see the conversation
    liftTest $ do
      convs <- getAllConvs (ciUser bob1)
      liftIO $
        assertBool
          "Users added to an MLS group should find it when listing conversations"
          (qcnv `elem` map cnvQualifiedId convs)

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
    err <-
      responseJsonError
        =<< postMessage alice1 (mpMessage commit)
          <!! const 404 === statusCode
    liftIO $ Wai.label err @?= "mls-proposal-not-found"

testCommitNotReferencingAllProposals :: TestM ()
testCommitNotReferencingAllProposals = do
  users@[_alice, bob, charlie] <- createAndConnectUsers (replicate 3 Nothing)

  runMLSTest $ do
    [alice1, bob1, charlie1] <- traverse createMLSClient users
    void $ setupMLSGroup alice1
    traverse_ uploadNewKeyPackage [bob1, charlie1]

    -- create proposals for bob and charlie
    createAddProposals alice1 [bob, charlie]
      >>= traverse_ sendAndConsumeMessage

    -- now create a commit referencing only the first proposal
    void $ rollBackClient alice1
    commit <- createPendingProposalCommit alice1

    -- send commit and expect and error
    err <-
      responseJsonError
        =<< postMessage alice1 (mpMessage commit)
          <!! const 400 === statusCode
    liftIO $ Wai.label err @?= "mls-commit-missing-references"

testAdminRemovesUserFromConv :: TestM ()
testAdminRemovesUserFromConv = do
  [alice, bob] <- createAndConnectUsers [Nothing, Nothing]
  (qcnv, events) <- runMLSTest $ do
    [alice1, bob1, bob2] <- traverse createMLSClient [alice, bob, bob]
    void $ createWireClient bob -- also create one extra non-MLS client
    traverse_ uploadNewKeyPackage [bob1, bob2]
    (_, qcnv) <- setupMLSGroup alice1
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommit
    events <- createRemoveCommit alice1 [bob1, bob2] >>= sendAndConsumeCommit
    pure (qcnv, events)

  liftIO $ assertOne events >>= assertLeaveEvent qcnv alice [bob]

  do
    convs <- getAllConvs (qUnqualified bob)
    liftIO $
      assertBool
        "bob is not longer part of conversation after the commit"
        (qcnv `notElem` map cnvQualifiedId convs)

testRemoveClientsIncomplete :: TestM ()
testRemoveClientsIncomplete = do
  [alice, bob] <- createAndConnectUsers [Nothing, Nothing]
  runMLSTest $ do
    [alice1, bob1, bob2] <- traverse createMLSClient [alice, bob, bob]
    traverse_ uploadNewKeyPackage [bob1, bob2]
    void $ setupMLSGroup alice1
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommit
    commit <- createRemoveCommit alice1 [bob1]

    err <-
      responseJsonError
        =<< postMessage alice1 (mpMessage commit)
          <!! statusCode === const 409
    liftIO $ Wai.label err @?= "mls-client-mismatch"

testRemoteAppMessage :: TestM ()
testRemoteAppMessage = do
  users@[alice, bob] <- createAndConnectUsers [Nothing, Just "bob.example.com"]

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient users

    (_, qcnv) <- setupMLSGroup alice1

    let mock = receiveCommitMock [bob1] <|> messageSentMock <|> welcomeMock

    ((message, events), reqs) <- withTempMockFederator' mock $ do
      void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommit
      message <- createApplicationMessage alice1 "hello"
      (events, _) <- sendAndConsumeMessage message
      pure (message, events)

    liftIO $ do
      req <- assertOne $ filter ((== "on-mls-message-sent") . frRPC) reqs
      frTargetDomain req @?= qDomain bob
      bdy <- case Aeson.eitherDecode (frBody req) of
        Right b -> pure b
        Left e -> assertFailure $ "Could not parse on-mls-message-sent request body: " <> e
      rmmSender bdy @?= alice
      rmmConversation bdy @?= qUnqualified qcnv
      rmmRecipients bdy @?= [(ciUser bob1, ciClient bob1)]
      rmmMessage bdy @?= Base64ByteString (mpMessage message)

    liftIO $ assertBool "Unexpected events returned" (null events)

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
-- 4) A notifies B about the new conversation
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
    (groupId, qcnv) <- setupFakeMLSGroup alice1
    mp <- createAddCommit alice1 [bob]
    -- step 10
    traverse_ consumeWelcome (mpWelcome mp)
    -- step 11
    message <- createApplicationMessage bob1 "hi"

    -- register remote conversation: step 4
    receiveNewRemoteConv qcnv groupId
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
      bdy <- case Aeson.eitherDecode (frBody req) of
        Right b -> pure b
        Left e -> assertFailure $ "Could not parse send-mls-message request body: " <> e
      mmsrConvOrSubId bdy @?= Conv (qUnqualified qcnv)
      mmsrSender bdy @?= qUnqualified bob
      mmsrRawMessage bdy @?= Base64ByteString (mpMessage message)

testLocalToRemoteNonMember :: TestM ()
testLocalToRemoteNonMember = do
  -- create users
  let domain = Domain "faraway.example.com"
  [alice, bob] <- createAndConnectUsers [Just (domainText domain), Nothing]

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]

    void $ uploadNewKeyPackage bob1

    -- step 2
    (groupId, qcnv) <- setupFakeMLSGroup alice1

    mp <- createAddCommit alice1 [bob]
    -- step 10
    traverse_ consumeWelcome (mpWelcome mp)
    -- step 11
    message <- createApplicationMessage bob1 "hi"

    -- register remote conversation: step 4
    receiveNewRemoteConv qcnv groupId

    void $
      withTempMockFederator' sendMessageMock $ do
        galley <- viewGalley

        -- bob sends a message: step 12
        post
          ( galley
              . paths ["mls", "messages"]
              . zUser (qUnqualified bob)
              . zConn "conn"
              . content "message/mls"
              . bytes (mpMessage message)
          )
          !!! do
            const 404 === statusCode
            const (Just "no-conversation-member")
              === fmap Wai.label . responseJsonError

testExternalCommitNotMember :: TestM ()
testExternalCommitNotMember = do
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)

  runMLSTest $ do
    [alice1, alice2, bob1] <- traverse createMLSClient [alice, alice, bob]
    traverse_ uploadNewKeyPackage [bob1, alice2]
    (_, qcnv) <- setupMLSGroup alice1

    -- so that we have the public group state
    void $ createAddCommit alice1 [alice] >>= sendAndConsumeCommitBundle

    pgs <-
      LBS.toStrict . fromJust . responseBody
        <$> getGroupInfo (ciUser alice1) qcnv
    mp <- createExternalCommit bob1 (Just pgs) qcnv
    bundle <- createBundle mp
    postCommitBundle (mpSender mp) bundle
      !!! const 404 === statusCode

testExternalCommitSameClient :: TestM ()
testExternalCommitSameClient = do
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    void $ uploadNewKeyPackage bob1
    (_, qcnv) <- setupMLSGroup alice1
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle

    let rejoiner = alice1
    ecEvents <- createExternalCommit rejoiner Nothing qcnv >>= sendAndConsumeCommitBundle
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
    ecEvents <- createExternalCommit nc Nothing qcnv >>= sendAndConsumeCommitBundle
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
    Just (_, kpBob2) <- find (\(ci, _) -> ci == bob2) <$> getClientsFromGroupState alice1 bob

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
        wsAssertBackendRemoveProposalWithEpoch bob qcnv kpBob2 (Epoch 1)

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

      mp <- createExternalCommit bob4 Nothing qcnv
      ecEvents <- sendAndConsumeCommitBundle mp
      liftIO $
        assertBool "No events after external commit expected" (null ecEvents)
      WS.assertMatchN_ (5 # WS.Second) [wsA, wsB] $
        wsAssertMLSMessage qcnv bob (mpMessage mp)

      -- The backend proposals for bob2 are replayed, but the external add
      -- proposal for bob3 has to replayed by the client and is thus not found
      -- here.
      WS.assertMatchN_ (5 # WS.Second) [wsA, wsB] $
        wsAssertBackendRemoveProposalWithEpoch bob qcnv kpBob2 (Epoch 2)
      WS.assertNoEvent (2 # WS.Second) [wsA, wsB]

testAppMessage :: TestM ()
testAppMessage = do
  users@(alice : _) <- createAndConnectUsers (replicate 4 Nothing)

  runMLSTest $ do
    clients@(alice1 : _) <- traverse createMLSClient users
    traverse_ uploadNewKeyPackage (tail clients)
    (_, qcnv) <- setupMLSGroup alice1
    void $ createAddCommit alice1 (tail users) >>= sendAndConsumeCommit
    message <- createApplicationMessage alice1 "some text"

    mlsBracket clients $ \wss -> do
      (events, _) <- sendAndConsumeMessage message
      liftIO $ events @?= []
      liftIO $
        WS.assertMatchN_ (5 # WS.Second) wss $
          wsAssertMLSMessage qcnv alice (mpMessage message)

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
    void $ sendAndConsumeCommit mp

    traverse_ consumeWelcome (mpWelcome mp)

    message <- createApplicationMessage bob1 "some text"

    mlsBracket (alice1 : clients) $ \wss -> do
      (events, _) <- sendAndConsumeMessage message
      liftIO $ events @?= []

      -- check that the corresponding event is received

      liftIO $
        WS.assertMatchN_ (5 # WS.Second) wss $
          wsAssertMLSMessage conversation bob (mpMessage message)

testAppMessageSomeReachable :: TestM ()
testAppMessageSomeReachable = do
  users@[_alice, bob, charlie] <-
    createAndConnectUsers
      [ Nothing,
        Just "bob.example.com",
        Just "charlie.example.com"
      ]

  void $ runMLSTest $ do
    [alice1, bob1, charlie1] <-
      traverse createMLSClient users

    void $ setupMLSGroup alice1
    commit <- createAddCommit alice1 [bob, charlie]

    let mocks =
          receiveCommitMockByDomain [bob1, charlie1]
            <|> welcomeMock
    ([event], _) <-
      withTempMockFederator' mocks $ do
        sendAndConsumeCommit commit

    let unreachables = Set.singleton (Domain "charlie.example.com")
    withTempMockFederator' (mockUnreachableFor unreachables) $ do
      message <- createApplicationMessage alice1 "hi, bob!"
      (_, us) <- sendAndConsumeMessage message
      liftIO $ do
        assertBool "Event should be member join" $ is _EdMembersJoin (evtData event)
        us @?= UnreachableUserList [charlie]
  where
    mockUnreachableFor :: Set Domain -> Mock LByteString
    mockUnreachableFor backends = do
      r <- getRequest
      if Set.member (frTargetDomain r) backends
        then throw (MockErrorResponse HTTP.status503 "Down for maintenance.")
        else mockReply ("RemoteMLSMessageOk" :: String)

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
        sendAndConsumeCommit commit

    message <- createApplicationMessage alice1 "hi, bob!"
    (_, us) <- sendAndConsumeMessage message
    liftIO $ do
      assertBool "Event should be member join" $ is _EdMembersJoin (evtData event)
      us @?= UnreachableUserList [bob]

testRemoteToRemote :: TestM ()
testRemoteToRemote = do
  localDomain <- viewFederationDomain
  c <- view tsCannon
  alice <- randomUser
  eve <- randomUser
  bob <- randomId
  conv <- randomId
  let aliceC1 = newClientId 0
      aliceC2 = newClientId 1
      eveC = newClientId 0
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
          { cuTime = now,
            cuOrigUserId = qbob,
            cuConvId = conv,
            cuAlreadyPresentUsers = [],
            cuAction =
              SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qalice) roleNameWireMember)
          }
  runFedClient @"on-conversation-updated" fedGalleyClient bdom cu

  let txt = "Hello from another backend"
      rcpts = [(alice, aliceC1), (alice, aliceC2), (eve, eveC)]
      rm =
        RemoteMLSMessage
          { rmmTime = now,
            rmmMetadata = defMessageMetadata,
            rmmSender = qbob,
            rmmConversation = conv,
            rmmRecipients = rcpts,
            rmmMessage = Base64ByteString txt
          }

  -- send message to alice and check reception
  WS.bracketAsClientRN c [(alice, aliceC1), (alice, aliceC2), (eve, eveC)] $ \[wsA1, wsA2, wsE] -> do
    void $ runFedClient @"on-mls-message-sent" fedGalleyClient bdom rm
    liftIO $ do
      -- alice should receive the message on her first client
      WS.assertMatch_ (5 # Second) wsA1 $ \n -> wsAssertMLSMessage qconv qbob txt n
      WS.assertMatch_ (5 # Second) wsA2 $ \n -> wsAssertMLSMessage qconv qbob txt n

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
      sendAndConsumeCommit mp

    traverse_ consumeWelcome (mpWelcome mp)
    message <- createApplicationMessage bob1 "hello from another backend"

    fedGalleyClient <- view tsFedGalleyClient
    cannon <- view tsCannon

    -- actual test
    let msr =
          MLSMessageSendRequest
            { mmsrConvOrSubId = Conv (qUnqualified qcnv),
              mmsrSender = qUnqualified bob,
              mmsrRawMessage = Base64ByteString (mpMessage message)
            }

    WS.bracketR cannon (qUnqualified alice) $ \ws -> do
      MLSMessageResponseUpdates updates _ <- runFedClient @"send-mls-message" fedGalleyClient bobDomain msr
      liftIO $ do
        updates @?= []
        WS.assertMatch_ (5 # Second) ws $
          wsAssertMLSMessage qcnv bob (mpMessage message)

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
    void . withTempMockFederator' mock $ sendAndConsumeCommit mp
    traverse_ consumeWelcome (mpWelcome mp)
    message <- createApplicationMessage bob1 "hello from another backend"

    fedGalleyClient <- view tsFedGalleyClient

    -- actual test
    randomConfId <- randomId
    let msr =
          MLSMessageSendRequest
            { mmsrConvOrSubId = Conv randomConfId,
              mmsrSender = qUnqualified bob,
              mmsrRawMessage = Base64ByteString (mpMessage message)
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
            { mmsrConvOrSubId = Conv (qUnqualified qcnv),
              mmsrSender = qUnqualified bob,
              mmsrRawMessage = Base64ByteString (mpMessage message)
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
    createGroup alice1 "test_group"

    [prop] <- createAddProposals alice1 [bob]
    postMessage alice1 (mpMessage prop) !!! do
      const 404 === statusCode
      const (Just "no-conversation") === fmap Wai.label . responseJsonError

propExistingConv :: TestM ()
propExistingConv = do
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)
  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    void $ uploadNewKeyPackage bob1
    void $ setupMLSGroup alice1
    res <- traverse sendAndConsumeMessage =<< createAddProposals alice1 [bob]

    liftIO $ (fst <$> res) @?= [[]]

propInvalidEpoch :: TestM ()
propInvalidEpoch = do
  users@[_alice, bob, charlie, dee] <- createAndConnectUsers (replicate 4 Nothing)
  runMLSTest $ do
    [alice1, bob1, charlie1, dee1] <- traverse createMLSClient users
    void $ setupMLSGroup alice1

    -- Add bob -> epoch 1
    void $ uploadNewKeyPackage bob1
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommit

    -- try to send a proposal from an old epoch (0)
    do
      groupState <- rollBackClient alice1
      void $ uploadNewKeyPackage dee1
      [prop] <- createAddProposals alice1 [dee]
      err <-
        responseJsonError
          =<< postMessage alice1 (mpMessage prop)
            <!! const 409 === statusCode
      liftIO $ Wai.label err @?= "mls-stale-message"
      setGroupState alice1 groupState

    -- try to send a proposal from a newer epoch (2)
    do
      void $ uploadNewKeyPackage dee1
      void $ uploadNewKeyPackage charlie1
      void $ createAddCommit alice1 [charlie]
      [prop] <- createAddProposals alice1 [dee]
      err <-
        responseJsonError
          =<< postMessage alice1 (mpMessage prop)
            <!! const 404 === statusCode
      liftIO $ Wai.label err @?= "mls-key-package-ref-not-found"
      replicateM_ 2 (rollBackClient alice1)
      -- remove charlie from users expected to get a welcome message
      State.modify $ \mls -> mls {mlsNewMembers = mempty}

    -- alice send a well-formed proposal and commits it
    void $ uploadNewKeyPackage dee1
    createAddProposals alice1 [dee] >>= traverse_ sendAndConsumeMessage
    void $ createPendingProposalCommit alice1 >>= sendAndConsumeCommit

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
        >>= sendAndConsumeCommit

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
        >>= sendAndConsumeCommit

    -- alice sends a message
    do
      msg <- createApplicationMessage alice1 "hi bob"
      mlsBracket [bob1, bob2] $ \wss -> do
        void $ sendAndConsumeMessage msg
        liftTest $
          WS.assertMatchN_ (5 # Second) wss $
            wsAssertMLSMessage qcnv alice (mpMessage msg)

    -- bob adds charlie
    putOtherMemberQualified
      (qUnqualified alice)
      bob
      (OtherMemberUpdate (Just roleNameWireAdmin))
      qcnv
      !!! const 200 === statusCode
    createAddCommit bob2 [charlie]
      >>= sendAndConsumeCommit

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
        >>= sendAndConsumeCommit

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
        >>= sendAndConsumeCommit

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
        >>= sendAndConsumeCommit

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
        >>= sendAndConsumeCommit

    prop <- createExternalAddProposal charlie1
    postMessage charlie1 (mpMessage prop)
      !!! do
        const 404 === statusCode
        const (Just "no-conversation") === fmap Wai.label . responseJsonError

-- FUTUREWORK: test processing a commit containing the external proposal
testPublicKeys :: TestM ()
testPublicKeys = do
  u <- randomId
  g <- viewGalley
  keys <-
    responseJsonError
      =<< get
        ( g
            . paths ["mls", "public-keys"]
            . zUser u
        )
        <!! const 200 === statusCode

  liftIO $
    Map.keys
      ( Map.findWithDefault
          mempty
          RemovalPurpose
          (unMLSPublicKeys keys)
      )
      @?= [Ed25519]

-- | The test manually reads from mls-test-cli's store and extracts a private
-- key. The key is needed for signing an AppAck proposal, which as of August 24,
-- 2022 only gets forwarded by the backend, i.e., there's no action taken by the
-- backend.
propUnsupported :: TestM ()
propUnsupported = do
  users@[_alice, bob] <- createAndConnectUsers (replicate 2 Nothing)
  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient users
    void $ uploadNewKeyPackage bob1
    (gid, _) <- setupMLSGroup alice1
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommit

    mems <- currentGroupFile alice1 >>= liftIO . readGroupState
    (_, ref) <- assertJust $ find ((== alice1) . fst) mems
    (priv, pub) <- clientKeyPair alice1
    msg <-
      assertJust $
        maybeCryptoError $
          mkAppAckProposalMessage
            gid
            (Epoch 1)
            ref
            []
            <$> Ed25519.secretKey priv
            <*> Ed25519.publicKey pub
    let msgData = LBS.toStrict (runPut (serialiseMLS msg))

    -- we cannot use sendAndConsumeMessage here, because openmls does not yet
    -- support AppAck proposals
    postMessage alice1 msgData !!! const 201 === statusCode

testBackendRemoveProposalLocalConvLocalUser :: TestM ()
testBackendRemoveProposalLocalConvLocalUser = do
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)
  runMLSTest $ do
    [alice1, bob1, bob2] <- traverse createMLSClient [alice, bob, bob]
    traverse_ uploadNewKeyPackage [bob1, bob2]
    (_, qcnv) <- setupMLSGroup alice1
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommit

    bobClients <- getClientsFromGroupState alice1 bob
    mlsBracket [alice1] $ \wss -> void $ do
      liftTest $ deleteUser (qUnqualified bob) !!! const 200 === statusCode
      -- remove bob clients from the test state
      State.modify $ \mls ->
        mls
          { mlsMembers = Set.difference (mlsMembers mls) (Set.fromList [bob1, bob2])
          }

      for bobClients $ \(_, ref) -> do
        [msg] <- WS.assertMatchN (5 # Second) wss $ \n ->
          wsAssertBackendRemoveProposal bob qcnv ref n
        consumeMessage1 alice1 msg

    -- alice commits the external proposals
    events <- createPendingProposalCommit alice1 >>= sendAndConsumeCommit
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
        void $ sendAndConsumeCommit commit

        bobClients <- getClientsFromGroupState alice1 bob
        fedGalleyClient <- view tsFedGalleyClient
        void $
          runFedClient
            @"on-user-deleted-conversations"
            fedGalleyClient
            (qDomain bob)
            ( UserDeletedConversationsNotification
                { udcvUser = qUnqualified bob,
                  udcvConversations = unsafeRange [qUnqualified qcnv]
                }
            )

        for_ bobClients $ \(_, ref) ->
          WS.assertMatch (5 # WS.Second) wsA $
            wsAssertBackendRemoveProposal bob qcnv ref

sendRemoteMLSWelcome :: TestM ()
sendRemoteMLSWelcome = do
  -- Alice is from the originating domain and Bob is local, i.e., on the receiving domain
  [alice, bob] <- createAndConnectUsers [Just "alice.example.com", Nothing]
  commit <- runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    void $ setupFakeMLSGroup alice1
    void $ uploadNewKeyPackage bob1
    createAddCommit alice1 [bob]

  welcome <- assertJust (mpWelcome commit)

  fedGalleyClient <- view tsFedGalleyClient
  cannon <- view tsCannon

  WS.bracketR cannon (qUnqualified bob) $ \wsB -> do
    -- send welcome message
    void $
      runFedClient @"mls-welcome" fedGalleyClient (qDomain alice) $
        MLSWelcomeRequest
          (Base64ByteString welcome)

    -- check that the corresponding event is received
    liftIO $ do
      WS.assertMatch_ (5 # WS.Second) wsB $
        wsAssertMLSWelcome bob welcome

sendRemoteMLSWelcomeKPNotFound :: TestM ()
sendRemoteMLSWelcomeKPNotFound = do
  [alice, bob] <- createAndConnectUsers [Just "alice.example.com", Nothing]
  commit <- runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    void $ setupFakeMLSGroup alice1
    kp <- generateKeyPackage bob1 >>= keyPackageFile bob1 . snd
    createAddCommitWithKeyPackages alice1 [(bob1, kp)]
  welcome <- assertJust (mpWelcome commit)

  fedGalleyClient <- view tsFedGalleyClient
  cannon <- view tsCannon
  WS.bracketR cannon (qUnqualified bob) $ \wsB -> do
    -- send welcome message
    void $
      runFedClient @"mls-welcome" fedGalleyClient (qDomain alice) $
        MLSWelcomeRequest
          (Base64ByteString welcome)

    liftIO $ do
      -- check that no event is received
      WS.assertNoEvent (1 # Second) [wsB]

testBackendRemoveProposalLocalConvLocalLeaverCreator :: TestM ()
testBackendRemoveProposalLocalConvLocalLeaverCreator = do
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)

  runMLSTest $ do
    [alice1, bob1, bob2] <- traverse createMLSClient [alice, bob, bob]
    traverse_ uploadNewKeyPackage [bob1, bob2]
    (_, qcnv) <- setupMLSGroup alice1
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommit

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

      for_ aliceClients $ \(_, ref) -> do
        -- only bob's clients should receive the external proposals
        msgs <- WS.assertMatchN (5 # Second) (drop 1 wss) $ \n ->
          wsAssertBackendRemoveProposal alice qcnv ref n
        traverse_ (uncurry consumeMessage1) (zip [bob1, bob2] msgs)

      -- but everyone should receive leave events
      WS.assertMatchN_ (5 # WS.Second) wss $
        wsAssertMembersLeave qcnv alice [alice]

      -- check that no more events are sent, so in particular alice does not
      -- receive any MLS messages
      WS.assertNoEvent (1 # WS.Second) wss

    -- bob commits the external proposals
    events <- createPendingProposalCommit bob1 >>= sendAndConsumeCommit
    liftIO $ events @?= []

testBackendRemoveProposalLocalConvLocalLeaverCommitter :: TestM ()
testBackendRemoveProposalLocalConvLocalLeaverCommitter = do
  [alice, bob, charlie] <- createAndConnectUsers (replicate 3 Nothing)

  runMLSTest $ do
    [alice1, bob1, bob2, charlie1] <- traverse createMLSClient [alice, bob, bob, charlie]
    traverse_ uploadNewKeyPackage [bob1, bob2, charlie1]
    (_, qcnv) <- setupMLSGroup alice1
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommit

    -- promote bob
    putOtherMemberQualified (ciUser alice1) bob (OtherMemberUpdate (Just roleNameWireAdmin)) qcnv
      !!! const 200 === statusCode

    void $ createAddCommit bob1 [charlie] >>= sendAndConsumeCommit

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

      for_ bobClients $ \(_, ref) -> do
        -- only alice and charlie should receive the external proposals
        msgs <- WS.assertMatchN (5 # Second) (take 2 wss) $ \n ->
          wsAssertBackendRemoveProposal bob qcnv ref n
        traverse_ (uncurry consumeMessage1) (zip [alice1, charlie1] msgs)

      -- but everyone should receive leave events
      WS.assertMatchN_ (5 # WS.Second) wss $
        wsAssertMembersLeave qcnv bob [bob]

      -- check that no more events are sent, so in particular bob does not
      -- receive any MLS messages
      WS.assertNoEvent (1 # WS.Second) wss

    -- alice commits the external proposals
    events <- createPendingProposalCommit alice1 >>= sendAndConsumeCommit
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
        void $ sendAndConsumeCommit commit
        fedGalleyClient <- view tsFedGalleyClient
        void $
          runFedClient
            @"update-conversation"
            fedGalleyClient
            (qDomain bob)
            ConversationUpdateRequest
              { curUser = qUnqualified bob,
                curConvId = qUnqualified qcnv,
                curAction = SomeConversationAction SConversationLeaveTag ()
              }

        for_ bobClients $ \(_, ref) ->
          WS.assertMatch_ (5 # WS.Second) wsA $
            wsAssertBackendRemoveProposal bob qcnv ref

testBackendRemoveProposalLocalConvLocalClient :: TestM ()
testBackendRemoveProposalLocalConvLocalClient = do
  [alice, bob, charlie] <- createAndConnectUsers (replicate 3 Nothing)

  runMLSTest $ do
    [alice1, bob1, bob2, charlie1] <- traverse createMLSClient [alice, bob, bob, charlie]
    traverse_ uploadNewKeyPackage [bob1, bob2, charlie1]
    (_, qcnv) <- setupMLSGroup alice1
    void $ createAddCommit alice1 [bob, charlie] >>= sendAndConsumeCommit
    Just (_, kpBob1) <- find (\(ci, _) -> ci == bob1) <$> getClientsFromGroupState alice1 bob

    mlsBracket [alice1, bob1] $ \[wsA, wsB] -> do
      liftTest $
        deleteClient (ciUser bob1) (ciClient bob1) (Just defPassword)
          !!! statusCode === const 200

      State.modify $ \mls ->
        mls
          { mlsMembers = Set.difference (mlsMembers mls) (Set.fromList [bob1])
          }

      WS.assertMatch_ (5 # WS.Second) wsB $
        wsAssertClientRemoved (ciClient bob1)

      msg <- WS.assertMatch (5 # WS.Second) wsA $ \notification -> do
        wsAssertBackendRemoveProposal bob qcnv kpBob1 notification

      for_ [alice1, bob2, charlie1] $
        flip consumeMessage1 msg

      mp <- createPendingProposalCommit charlie1
      events <- sendAndConsumeCommit mp
      liftIO $ events @?= []

testBackendRemoveProposalLocalConvRemoteClient :: TestM ()
testBackendRemoveProposalLocalConvRemoteClient = do
  [alice, bob] <- createAndConnectUsers [Nothing, Just "faraway.example.com"]

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    (_, qcnv) <- setupMLSGroup alice1
    commit <- createAddCommit alice1 [bob]

    [(_, bob1KP)] <- getClientsFromGroupState alice1 bob
    let mock = receiveCommitMock [bob1] <|> welcomeMock <|> messageSentMock
    void . withTempMockFederator' mock $ do
      mlsBracket [alice1] $ \[wsA] -> void $ do
        void $ sendAndConsumeCommit commit

        fedGalleyClient <- view tsFedGalleyClient
        void $
          runFedClient
            @"on-client-removed"
            fedGalleyClient
            (ciDomain bob1)
            (ClientRemovedRequest (ciUser bob1) (ciClient bob1) [qUnqualified qcnv])

        WS.assertMatch_ (5 # WS.Second) wsA $
          \notification ->
            void $ wsAssertBackendRemoveProposal bob qcnv bob1KP notification

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
    gs <- assertJust (mpPublicGroupState commit)
    returnedGS <-
      fmap responseBody $
        getGroupInfo (qUnqualified alice) qcnv
          <!! const 200 === statusCode
    liftIO $ Just gs @=? LBS.toStrict <$> returnedGS

testGetGroupInfoOfRemoteConv :: TestM ()
testGetGroupInfoOfRemoteConv = do
  let aliceDomain = Domain "faraway.example.com"
  [alice, bob, charlie] <- createAndConnectUsers [Just (domainText aliceDomain), Nothing, Nothing]

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]

    void $ uploadNewKeyPackage bob1
    (groupId, qcnv) <- setupFakeMLSGroup alice1
    mp <- createAddCommit alice1 [bob]
    traverse_ consumeWelcome (mpWelcome mp)

    receiveNewRemoteConv qcnv groupId
    receiveOnConvUpdated qcnv alice bob

    let fakeGroupState = "\xde\xad\xbe\xef"
    let mock = queryGroupStateMock fakeGroupState bob
    (_, reqs) <- withTempMockFederator' mock $ do
      res <-
        fmap responseBody $
          getGroupInfo (qUnqualified bob) qcnv
            <!! const 200 === statusCode

      getGroupInfo (qUnqualified charlie) qcnv
        !!! const 404 === statusCode

      liftIO $ res @?= Just (LBS.fromStrict fakeGroupState)

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
    groupState <- assertJust (mpPublicGroupState commit)

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
            (GetGroupInfoRequest (qUnqualified qcnv) (qUnqualified bob))

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
            (GetGroupInfoRequest (qUnqualified qcnv) (qUnqualified charlie))

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
    (groupId, qcnv) <- setupFakeMLSGroup alice1

    mp <- createAddCommit alice1 [bob]
    traverse_ consumeWelcome (mpWelcome mp)

    receiveNewRemoteConv qcnv groupId
    receiveOnConvUpdated qcnv alice bob

    -- NB. this commit would be rejected by the owning backend, but for the
    -- purpose of this test it's good enough.
    [charlie1] <- traverse createMLSClient [charlie]
    void $ uploadNewKeyPackage charlie1
    commit <- createAddCommit bob1 [charlie]
    commitBundle <- createBundle commit

    let mock = "send-mls-commit-bundle" ~> MLSMessageResponseUpdates [] (UnreachableUserList [])
    (_, reqs) <- withTempMockFederator' mock $ do
      void $ sendAndConsumeCommitBundle commit

    req <- liftIO $ assertOne reqs
    liftIO $ do
      frRPC req @?= "send-mls-commit-bundle"
      frTargetDomain req @?= qDomain qcnv

      msr <- case Aeson.eitherDecode (frBody req) of
        Right b -> pure b
        Left e -> assertFailure $ "Could not parse send-mls-commit-bundle request body: " <> e

      mmsrConvOrSubId msr @?= Conv (qUnqualified qcnv)
      mmsrSender msr @?= qUnqualified bob
      fromBase64ByteString (mmsrRawMessage msr) @?= commitBundle

testRemoteUserPostsCommitBundle :: TestM ()
testRemoteUserPostsCommitBundle = do
  let bobDomain = "bob.example.com"
  [alice, bob, charlie] <- createAndConnectUsers [Nothing, Just bobDomain, Just bobDomain]
  fedGalleyClient <- view tsFedGalleyClient

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    (_, qcnv) <- setupMLSGroup alice1

    commit <- createAddCommit alice1 [bob]
    void $ do
      let mock = receiveCommitMock [bob1] <|> welcomeMock
      withTempMockFederator' mock $ do
        void $ sendAndConsumeCommit commit
        putOtherMemberQualified (qUnqualified alice) bob (OtherMemberUpdate (Just roleNameWireAdmin)) qcnv
          !!! const 200 === statusCode

        [_charlie1] <- traverse createMLSClient [charlie]
        commitAddCharlie <- createAddCommit bob1 [charlie]
        commitBundle <- createBundle commitAddCharlie

        let msr = MLSMessageSendRequest (Conv (qUnqualified qcnv)) (qUnqualified bob) (Base64ByteString commitBundle)
        -- we can't fully test it, because remote admins are not implemeted, but
        -- at least this proves that proposal processing has started on the
        -- backend
        MLSMessageResponseError MLSUnsupportedProposal <- runFedClient @"send-mls-commit-bundle" fedGalleyClient (Domain bobDomain) msr

        pure ()

testSelfConversation :: TestM ()
testSelfConversation = do
  alice <- randomQualifiedUser
  runMLSTest $ do
    creator : others <- traverse createMLSClient (replicate 3 alice)
    traverse_ uploadNewKeyPackage others
    void $ setupMLSSelfGroup creator
    commit <- createAddCommit creator [alice]
    welcome <- assertJust (mpWelcome commit)
    mlsBracket others $ \wss -> do
      void $ sendAndConsumeCommitBundle commit
      WS.assertMatchN_ (5 # Second) wss $
        wsAssertMLSWelcome alice welcome
      WS.assertNoEvent (1 # WS.Second) wss

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
      pure $ foldr (<|>) Nothing $ guard . isMLSSelf u <$> mtpResults convIds

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
    mlsBracket [alice1, bob1] $ \wss -> do
      postMessage (mpSender commit) (mpMessage commit)
        !!! do
          const 403 === statusCode
          const (Just "invalid-op") === fmap Wai.label . responseJsonError
      WS.assertNoEvent (1 # WS.Second) wss

testSelfConversationLeave :: TestM ()
testSelfConversationLeave = do
  alice <- randomQualifiedUser
  runMLSTest $ do
    clients@(creator : others) <- traverse createMLSClient (replicate 3 alice)
    traverse_ uploadNewKeyPackage others
    (_, qcnv) <- setupMLSSelfGroup creator
    void $ createAddCommit creator [alice] >>= sendAndConsumeCommit
    mlsBracket clients $ \wss -> do
      liftTest $
        deleteMemberQualified (qUnqualified alice) alice qcnv
          !!! do
            const 403 === statusCode
            const (Just "invalid-op") === fmap Wai.label . responseJsonError
      WS.assertNoEvent (1 # WS.Second) wss

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
      (Just (newClientId 0))
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
    withMLSDisabled $
      postMessage (mpSender mp) (mpMessage mp)
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
      postCommitBundle (mpSender mp) bundle
        !!! assertMLSNotEnabled

getGroupInfoDisabled :: TestM ()
getGroupInfoDisabled = do
  [alice, bob] <- createAndConnectUsers [Nothing, Nothing]
  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    void $ uploadNewKeyPackage bob1
    (_, qcnv) <- setupMLSGroup alice1
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommit

    withMLSDisabled $
      getGroupInfo (qUnqualified alice) qcnv
        !!! assertMLSNotEnabled
