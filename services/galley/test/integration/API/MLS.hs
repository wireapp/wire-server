{-# LANGUAGE RecordWildCards #-}
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

import API.MLS.Util
import API.Util
import Bilge hiding (head)
import Bilge.Assert
import Cassandra
import Control.Lens (view, (^..))
import Crypto.Error
import qualified Crypto.PubKey.Ed25519 as C
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Lazy as LBS
import Data.Default
import Data.Domain
import Data.Id
import Data.Json.Util hiding ((#))
import qualified Data.List.NonEmpty as NE
import qualified Data.List.NonEmpty as NonEmpty
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
import qualified Network.Wai.Utilities.Error as Wai
import System.FilePath
import System.IO.Temp
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
import Wire.API.Federation.API.Common
import Wire.API.Federation.API.Galley
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Keys
import Wire.API.MLS.Message
import Wire.API.MLS.Serialisation
import Wire.API.Message
import Wire.API.User.Client

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
          test s "remote welcome" testRemoteWelcome
        ],
      testGroup
        "Creation"
        [ test s "fail to create MLS conversation" postMLSConvFail,
          test s "create MLS conversation" postMLSConvOk
        ],
      testGroup
        "Commit"
        [ test s "add user to a conversation" testAddUser,
          test s "add user (not connected)" testAddUserNotConnected,
          test s "add user (partial client list)" testAddUserPartial,
          test s "add client of existing user" testAddClientPartial,
          test s "add user with some non-MLS clients" testAddUserWithProteusClients,
          test s "send a stale commit" testStaleCommit,
          test s "add remote user to a conversation" testAddRemoteUser,
          test s "return error when commit is locked" testCommitLock,
          test s "add user to a conversation with proposal + commit" testAddUserBareProposalCommit,
          test s "post commit that references a unknown proposal" testUnknownProposalRefCommit,
          test s "post commit that is not referencing all proposals" testCommitNotReferencingAllProposals,
          test s "admin removes user from a conversation" testAdminRemovesUserFromConv,
          test s "admin removes user from a conversation but doesn't list all clients" testRemoveClientsIncomplete,
          test s "anyone removes a non-existing client from a group" (testRemoveDeletedClient True),
          test s "anyone removes an existing client from group, but the user has other clients" (testRemoveDeletedClient False),
          test s "admin removes only strict subset of clients from a user" testRemoveSubset
        ],
      testGroup
        "Application Message"
        [ testGroup
            "Local Sender/Local Conversation"
            [ test s "send application message" testAppMessage,
              test s "send remote application message" testRemoteAppMessage,
              test s "another participant sends an application message" testAppMessage2
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
          test s "non-member adds new client" testExternalAddProposalWrongUser,
          test s "member adds unknown new client" testExternalAddProposalWrongClient
        ],
      testGroup
        "Backend-side External Remove Proposals"
        [ test s "local conversation, local user deleted" testBackendRemoveProposalLocalConvLocalUser,
          test s "local conversation, remote user deleted" testBackendRemoveProposalLocalConvRemoteUser
        ],
      testGroup
        "Protocol mismatch"
        [ test s "send a commit to a proteus conversation" testAddUsersToProteus,
          test s "add users bypassing MLS" testAddUsersDirectly,
          test s "remove users bypassing MLS" testRemoveUsersDirectly,
          test s "send proteus message to an MLS conversation" testProteusMessage
        ],
      test s "public keys" testPublicKeys
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
    (defNewMLSConv aliceClient)
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
    rsp <- postConvQualified alice (defNewMLSConv aliceClient) {newConvName = checked nameMaxSize}
    pure rsp !!! do
      const 201 === statusCode
      const Nothing === fmap Wai.label . responseJsonError
    cid <- assertConv rsp RegularConv alice qalice [] (Just nameMaxSize) Nothing
    checkConvCreateEvent cid wsA

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
        =<< postMessage (qUnqualified bob) (mpMessage message)
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
      responseJsonError =<< postWelcome (ciUser alice1) welcome
        <!! do
          const 404 === statusCode
    liftIO $ Wai.label err @?= "mls-key-package-ref-not-found"

testRemoteWelcome :: TestM ()
testRemoteWelcome = do
  [alice, bob] <- createAndConnectUsers [Nothing, Just "bob.example.com"]

  let mockedResponse fedReq =
        case frRPC fedReq of
          "mls-welcome" -> pure (Aeson.encode EmptyResponse)
          ms -> assertFailure ("unmocked endpoint called: " <> cs ms)

  runMLSTest $ do
    alice1 <- createMLSClient alice
    _bob1 <- createFakeMLSClient bob

    void $ setupMLSGroup alice1
    commit <- createAddCommit alice1 [bob]
    welcome <- liftIO $ case mpWelcome commit of
      Nothing -> assertFailure "Expected welcome message"
      Just w -> pure w
    (_, reqs) <-
      withTempMockFederator' mockedResponse $
        postWelcome (ciUser (mpSender commit)) welcome
          !!! const 201 === statusCode
    consumeWelcome welcome
    fedWelcome <- assertOne (filter ((== "mls-welcome") . frRPC) reqs)
    let req :: Maybe MLSWelcomeRequest = Aeson.decode (frBody fedWelcome)
    liftIO $ req @?= (Just . MLSWelcomeRequest . Base64ByteString) welcome

-- | Send a commit message, and assert that all participants see an event with
-- the given list of new members.
testSuccessfulCommitWithNewUsers :: HasCallStack => MessagingSetup -> [Qualified UserId] -> TestM ()
testSuccessfulCommitWithNewUsers setup@MessagingSetup {..} newUsers = do
  cannon <- view tsCannon

  WS.bracketRN cannon (map (qUnqualified . pUserId) users) $ \wss -> do
    -- send commit message
    events <- postCommit setup

    let alreadyPresent =
          map snd
            . filter (\(p, _) -> pUserId p `notElem` newUsers)
            $ zip users wss

    liftIO $
      if null newUsers
        then do
          -- check that alice receives no events
          events @?= []

          -- check that no users receive join events
          when (null alreadyPresent) $
            WS.assertNoEvent (1 # WS.Second) wss
        else do
          -- check that alice receives a join event
          case events of
            [e] -> assertJoinEvent conversation (pUserId creator) newUsers roleNameWireMember e
            [] -> assertFailure "expected join event to be returned to alice"
            es -> assertFailure $ "expected one event, found: " <> show es

          -- check that all users receive a join event,
          for_ wss $ \ws -> do
            WS.assertMatch_ (5 # WS.Second) ws $
              wsAssertMemberJoinWithRole conversation (pUserId creator) newUsers roleNameWireMember

    -- and that the already-present users in the conversation receive a commit
    for_ alreadyPresent $ \ws -> do
      WS.assertMatch_ (5 # WS.Second) ws $
        wsAssertMLSMessage conversation (pUserId creator) commit

testFailedCommit :: HasCallStack => MessagingSetup -> Int -> TestM Wai.Error
testFailedCommit MessagingSetup {..} status = do
  cannon <- view tsCannon

  WS.bracketRN cannon (map (qUnqualified . pUserId) users) $ \wss -> do
    galley <- viewGalley
    err <-
      responseJsonError
        =<< post
          ( galley . paths ["mls", "messages"]
              . zUser (qUnqualified (pUserId creator))
              . zConn "conn"
              . content "message/mls"
              . bytes commit
          )
        <!! const status === statusCode

    -- check that users did not receive any event
    void . liftIO $ WS.assertNoEvent (1 # WS.Second) wss
    pure err

testSuccessfulCommit :: HasCallStack => MessagingSetup -> TestM ()
testSuccessfulCommit setup = testSuccessfulCommitWithNewUsers setup (map pUserId (users setup))

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
  convs <-
    responseJsonError =<< getConvs (qUnqualified bob) Nothing Nothing
      <!! const 200 === statusCode
  liftIO $
    assertBool
      "Users added to an MLS group should find it when listing conversations"
      (qcnv `elem` map cnvQualifiedId (convList convs))

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
          =<< postMessage (ciUser (mpSender commit)) (mpMessage commit)
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
        =<< postMessage (ciUser (mpSender commit)) (mpMessage commit)
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
        =<< postMessage (qUnqualified bob) (mpMessage mp)
          <!! const 409 === statusCode
    liftIO $ Wai.label err @?= "mls-client-sender-user-mismatch"

testAddUsersToProteus :: TestM ()
testAddUsersToProteus = do
  setup <- aliceInvitesBob (1, LocalUser) def {createConv = CreateProteusConv}
  err <- testFailedCommit setup 404
  liftIO $ Wai.label err @?= "no-conversation"

testAddUsersDirectly :: TestM ()
testAddUsersDirectly = do
  setup@MessagingSetup {..} <- aliceInvitesBob (1, LocalUser) def {createConv = CreateConv}
  void $ postCommit setup
  charlie <- randomQualifiedUser
  e <-
    responseJsonError
      =<< postMembers
        (qUnqualified (pUserId creator))
        (pure charlie)
        conversation
      <!! const 403 === statusCode
  liftIO $ Wai.label e @?= "invalid-op"

testRemoveUsersDirectly :: TestM ()
testRemoveUsersDirectly = do
  setup@MessagingSetup {..} <- aliceInvitesBob (1, LocalUser) def {createConv = CreateConv}
  void $ postCommit setup
  e <-
    responseJsonError
      =<< deleteMemberQualified
        (qUnqualified (pUserId creator))
        (pUserId (head users))
        conversation
      <!! const 403 === statusCode
  liftIO $ Wai.label e @?= "invalid-op"

testProteusMessage :: TestM ()
testProteusMessage = do
  setup@MessagingSetup {..} <- aliceInvitesBob (1, LocalUser) def {createConv = CreateConv}
  void $ postCommit setup
  e <-
    responseJsonError
      =<< postProteusMessageQualified
        (qUnqualified (pUserId creator))
        (snd (NonEmpty.head (pClients creator)))
        conversation
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
    rollBackClient alice1
    commit <- createAddCommit alice1 users2
    err <-
      responseJsonError
        =<< postMessage (ciUser (mpSender commit)) (mpMessage commit)
        <!! const 409 === statusCode
    liftIO $ Wai.label err @?= "mls-stale-message"

testAddRemoteUser :: TestM ()
testAddRemoteUser = do
  users@[alice, bob] <- createAndConnectUsers [Nothing, Just "bob.example.com"]
  (events, reqs, qcnv) <- runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient users
    (_, qcnv) <- setupMLSGroup alice1

    let mock req = case frRPC req of
          "on-conversation-updated" -> pure (Aeson.encode ())
          "on-new-remote-conversation" -> pure (Aeson.encode EmptyResponse)
          "get-mls-clients" ->
            pure
              . Aeson.encode
              . Set.fromList
              . map (flip ClientInfo True . ciClient)
              $ [bob1]
          "mls-welcome" -> pure (Aeson.encode EmptyResponse)
          ms -> assertFailure ("unmocked endpoint called: " <> cs ms)

    commit <- createAddCommit alice1 [bob]
    (events, reqs) <-
      withTempMockFederator' mock $
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
          =<< postMessage (ciUser alice1) (mpMessage commit)
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
      convs <-
        responseJsonError =<< getConvs (ciUser bob1) Nothing Nothing
          <!! const 200 === statusCode
      liftIO $
        assertBool
          "Users added to an MLS group should find it when listing conversations"
          (qcnv `elem` map cnvQualifiedId (convList convs))

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
      responseJsonError =<< postMessage (ciUser alice1) (mpMessage commit)
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
    rollBackClient alice1
    commit <- createPendingProposalCommit alice1

    -- send commit and expect and error
    err <-
      responseJsonError =<< postMessage (ciUser alice1) (mpMessage commit)
        <!! const 409 === statusCode
    liftIO $ Wai.label err @?= "mls-commit-missing-references"

testAdminRemovesUserFromConv :: TestM ()
testAdminRemovesUserFromConv = do
  [alice, bob] <- createAndConnectUsers [Nothing, Nothing]
  (qcnv, events) <- runMLSTest $ do
    [alice1, bob1, bob2] <- traverse createMLSClient [alice, bob, bob]
    traverse_ uploadNewKeyPackage [bob1, bob2]
    (_, qcnv) <- setupMLSGroup alice1
    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommit
    events <- createRemoveCommit alice1 [bob1, bob2] >>= sendAndConsumeCommit
    pure (qcnv, events)

  liftIO $ assertOne events >>= assertLeaveEvent qcnv alice [bob]

  do
    convs <-
      responseJsonError =<< getConvs (qUnqualified bob) Nothing Nothing
        <!! const 200 === statusCode
    liftIO $
      assertBool
        "bob is not longer part of conversation after the commit"
        (qcnv `notElem` map cnvQualifiedId (convList convs))

testRemoveClientsIncomplete :: TestM ()
testRemoveClientsIncomplete = withSystemTempDirectory "mls" $ \tmp -> do
  MessagingSetup {..} <- aliceInvitesBobWithTmp tmp (2, LocalUser) def {createConv = CreateConv}
  let [bob] = users

  testSuccessfulCommit MessagingSetup {users = [bob], ..}

  -- remove only first client of bob
  (removalCommit, _mbWelcome) <- liftIO $ setupRemoveCommit tmp creator "group" "group" [NE.head (pClients bob)]

  err <-
    responseJsonError
      =<< postMessage (qUnqualified (pUserId creator)) removalCommit
        <!! statusCode === const 409

  liftIO $ Wai.label err @?= "mls-client-mismatch"

testRemoveDeletedClient :: Bool -> TestM ()
testRemoveDeletedClient deleteClientBefore = withSystemTempDirectory "mls" $ \tmp -> do
  (creator, [bob, dee]) <- withLastPrekeys $ setupParticipants tmp def [(2, LocalUser), (1, LocalUser)]

  -- create a group
  (groupId, conversation) <- setupGroup tmp CreateConv creator "group"

  -- add clients to it and get welcome message
  (addCommit, welcome) <-
    liftIO $
      setupCommit tmp creator "group" "group" $
        NonEmpty.tail (pClients creator) <> toList (pClients bob) <> toList (pClients dee)

  testSuccessfulCommit MessagingSetup {users = [bob, dee], commit = addCommit, ..}

  let (_bobClient1, bobClient2) = assertTwo (toList (pClients bob))

  when deleteClientBefore $ do
    cannon <- view tsCannon
    WS.bracketR
      cannon
      (qUnqualified . pUserId $ bob)
      $ \ws -> do
        deleteClient (qUnqualified (pUserId bob)) (snd bobClient2) (Just defPassword)
          !!! statusCode
            === const
              200
        -- check that the corresponding event is received

        liftIO $
          WS.assertMatch_ (5 # WS.Second) ws $
            wsAssertClientRemoved (snd bobClient2)

  void . liftIO $
    spawn
      ( cli
          (pClientQid bob)
          tmp
          [ "group",
            "from-welcome",
            "--group-out",
            tmp </> "group",
            tmp </> "welcome"
          ]
      )
      Nothing

  void . liftIO $
    spawn
      ( cli
          (pClientQid dee)
          tmp
          [ "group",
            "from-welcome",
            "--group-out",
            tmp </> "group",
            tmp </> "welcome"
          ]
      )
      Nothing

  (removalCommit, _mbWelcome) <- liftIO $ setupRemoveCommit tmp dee "group" "group" [bobClient2]

  -- dee (which is not an admin) commits removal of bob's deleted client
  let doCommitRemoval = postMessage (qUnqualified (pUserId dee)) removalCommit

  if deleteClientBefore
    then do
      events :: [Event] <-
        fmap mmssEvents . responseJsonError
          =<< doCommitRemoval
            <!! statusCode === const 201
      liftIO $ assertEqual "a non-admin received conversation events when removing a client" [] events
    else do
      err <-
        responseJsonError
          =<< doCommitRemoval
            <!! statusCode === const 409
      liftIO $ Wai.label err @?= "mls-client-mismatch"

testRemoveSubset :: TestM ()
testRemoveSubset = withSystemTempDirectory "mls" $ \tmp -> do
  MessagingSetup {..} <- aliceInvitesBobWithTmp tmp (2, LocalUser) def {createConv = CreateConv}
  let [bob] = users

  testSuccessfulCommit MessagingSetup {users = [bob], ..}

  -- attempt to remove only first client of bob
  (removalCommit, _mbWelcome) <- liftIO $ setupRemoveCommit tmp creator "group" "group" [NonEmpty.head (pClients bob)]

  err <-
    responseJsonError
      =<< postMessage (qUnqualified (pUserId creator)) removalCommit
        <!! statusCode === const 409

  liftIO $ Wai.label err @?= "mls-client-mismatch"

testRemoteAppMessage :: TestM ()
testRemoteAppMessage = do
  users@[alice, bob] <- createAndConnectUsers [Nothing, Just "bob.example.com"]

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient users

    (_, qcnv) <- setupMLSGroup alice1

    let mock req = case frRPC req of
          "on-conversation-updated" -> pure (Aeson.encode ())
          "on-new-remote-conversation" -> pure (Aeson.encode EmptyResponse)
          "on-mls-message-sent" -> pure (Aeson.encode EmptyResponse)
          "get-mls-clients" ->
            pure
              . Aeson.encode
              . Set.singleton
              $ ClientInfo (ciClient bob1) True
          ms -> assertFailure ("unmocked endpoint called: " <> cs ms)

    ((message, events), reqs) <- withTempMockFederator' mock $ do
      void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommit
      message <- createApplicationMessage alice1 "hello"
      events <- sendAndConsumeMessage message
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
testLocalToRemote = withSystemTempDirectory "mls" $ \tmp -> do
  let domain = Domain "faraway.example.com"
  -- step 2
  MessagingSetup {creator = alice, users = [bob], ..} <-
    aliceInvitesBobWithTmp
      tmp
      (1, LocalUser)
      def
        { creatorOrigin = RemoteUser domain
        }

  -- step 10
  liftIO $ mergeWelcome tmp (pClientQid bob) "group" "groupB.json" "welcome"
  -- step 11
  message <-
    liftIO $
      spawn
        ( cli
            (pClientQid bob)
            tmp
            ["message", "--group", tmp </> "groupB.json", "hi"]
        )
        Nothing

  fedGalleyClient <- view tsFedGalleyClient

  -- register remote conversation: step 4
  qcnv <- randomQualifiedId (qDomain (pUserId alice))
  let nrc =
        NewRemoteConversation (qUnqualified qcnv) $
          ProtocolMLS (ConversationMLSData groupId (Epoch 1) MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519)
  void $
    runFedClient
      @"on-new-remote-conversation"
      fedGalleyClient
      (qDomain (pUserId alice))
      nrc

  -- A notifies B about bob being in the conversation (Join event): step 5
  now <- liftIO getCurrentTime
  let cu =
        ConversationUpdate
          { cuTime = now,
            cuOrigUserId = pUserId alice,
            cuConvId = qUnqualified qcnv,
            cuAlreadyPresentUsers = [qUnqualified $ pUserId bob],
            cuAction =
              SomeConversationAction
                SConversationJoinTag
                ConversationJoin
                  { cjUsers = pure (pUserId bob),
                    cjRole = roleNameWireMember
                  }
          }
  void $
    runFedClient
      @"on-conversation-updated"
      fedGalleyClient
      (qDomain (pUserId alice))
      cu

  let mock req = case frRPC req of
        "send-mls-message" -> pure (Aeson.encode (MLSMessageResponseUpdates []))
        rpc -> assertFailure $ "unmocked RPC called: " <> T.unpack rpc

  (_, reqs) <- withTempMockFederator' mock $ do
    galley <- viewGalley

    -- bob sends a message: step 12
    post
      ( galley . paths ["mls", "messages"]
          . zUser (qUnqualified (pUserId bob))
          . zConn "conn"
          . content "message/mls"
          . bytes message
      )
      !!! const 201
      === statusCode

  -- check requests to mock federator: step 14
  liftIO $ do
    req <- assertOne reqs
    frRPC req @?= "send-mls-message"
    frTargetDomain req @?= qDomain qcnv
    bdy <- case Aeson.eitherDecode (frBody req) of
      Right b -> pure b
      Left e -> assertFailure $ "Could not parse send-mls-message request body: " <> e
    msrConvId bdy @?= qUnqualified qcnv
    msrSender bdy @?= qUnqualified (pUserId bob)
    msrRawMessage bdy @?= Base64ByteString message

testLocalToRemoteNonMember :: TestM ()
testLocalToRemoteNonMember = withSystemTempDirectory "mls" $ \tmp -> do
  let domain = Domain "faraway.example.com"
  -- step 2
  MessagingSetup {creator = alice, users = [bob], ..} <-
    aliceInvitesBobWithTmp
      tmp
      (1, LocalUser)
      def
        { creatorOrigin = RemoteUser domain
        }

  -- step 10
  liftIO $ mergeWelcome tmp (pClientQid bob) "group" "groupB.json" "welcome"
  -- step 11
  message <-
    liftIO $
      spawn
        ( cli
            (pClientQid bob)
            tmp
            ["message", "--group", tmp </> "groupB.json", "hi"]
        )
        Nothing

  fedGalleyClient <- view tsFedGalleyClient

  -- register remote conversation: step 4
  qcnv <- randomQualifiedId (qDomain (pUserId alice))
  let nrc =
        NewRemoteConversation (qUnqualified qcnv) $
          ProtocolMLS (ConversationMLSData groupId (Epoch 1) MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519)
  void $
    runFedClient
      @"on-new-remote-conversation"
      fedGalleyClient
      (qDomain (pUserId alice))
      nrc

  let mock req = case frRPC req of
        "send-mls-message" -> pure (Aeson.encode (MLSMessageResponseUpdates []))
        rpc -> assertFailure $ "unmocked RPC called: " <> T.unpack rpc

  void $
    withTempMockFederator' mock $ do
      galley <- viewGalley

      -- bob sends a message: step 12
      post
        ( galley . paths ["mls", "messages"]
            . zUser (qUnqualified (pUserId bob))
            . zConn "conn"
            . content "message/mls"
            . bytes message
        )
        !!! do
          const 404 === statusCode
          const (Just "no-conversation-member") === fmap Wai.label . responseJsonError

testAppMessage :: TestM ()
testAppMessage = do
  -- create users
  alice : otherUsers <- createAndConnectUsers (replicate 4 Nothing)

  runMLSTest $ do
    alice1 : otherClients <-
      let cus = concatMap (uncurry replicate) ([1 ..] `zip` otherUsers)
       in traverse createMLSClient (alice : cus)

    -- upload key packages
    traverse_ uploadNewKeyPackage otherClients

    -- create group with alice1 and otherClients
    qcnv <- snd <$> setupMLSGroup alice1
    createAddCommit alice1 otherUsers >>= void . sendAndConsumeCommit

    message <- createApplicationMessage alice1 "some text"
    events <- sendAndConsumeMessage message
    liftIO $ events @?= []

    mlsBracket (alice1 : otherClients) $ \wss ->
      liftIO $
        WS.assertMatchN_ (5 # WS.Second) wss $
          wsAssertMLSMessage qcnv alice (mpMessage message)

testAppMessage2 :: TestM ()
testAppMessage2 = do
  (MessagingSetup {..}, message) <- withSystemTempDirectory "mls" $ \tmp -> do
    (creator, users) <- withLastPrekeys $ setupParticipants tmp def ((,LocalUser) <$> [2, 1])
    (groupId, conversation) <- setupGroup tmp CreateConv creator "group"

    (commit, welcome) <-
      liftIO $
        setupCommit tmp creator "group" "group" $
          users >>= toList . pClients

    let setup = MessagingSetup {..}
    void $ postCommit setup

    let bob = head users
    liftIO $ mergeWelcome tmp (pClientQid bob) "group" "group" "welcome"
    message <-
      liftIO $
        createMessage tmp bob "group" "some text"
    pure (setup, message)

  let (bob, charlie) = assertTwo users
  galley <- viewGalley
  cannon <- view tsCannon

  let mkClients p = do
        c <- pClients p
        pure (qUnqualified (pUserId p), snd c)

  WS.bracketAsClientRN
    cannon
    ( toList (mkClients creator)
        <> NonEmpty.tail (mkClients bob)
        <> toList (mkClients charlie)
    )
    $ \wss -> do
      post
        ( galley . paths ["mls", "messages"]
            . zUser (qUnqualified (pUserId bob))
            . zConn "conn"
            . content "message/mls"
            . bytes message
        )
        !!! const 201
        === statusCode

      -- check that the corresponding event is received

      liftIO $
        WS.assertMatchN_ (5 # WS.Second) wss $
          wsAssertMLSMessage conversation (pUserId bob) message

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

  -- Simulate the whole MLS setup for both clients first. In reality,
  -- backend calls would need to happen in order for bob to get ahold of a
  -- welcome message, but that should not affect the correctness of the test.

  (MessagingSetup {..}, message) <- withSystemTempDirectory "mls" $ \tmp -> do
    setup <-
      aliceInvitesBobWithTmp
        tmp
        (1, RemoteUser bobDomain)
        def
          { createConv = CreateConv
          }
    bob <- assertOne (users setup)
    let mockedResponse fedReq =
          case frRPC fedReq of
            "mls-welcome" -> pure (Aeson.encode EmptyResponse)
            "on-new-remote-conversation" -> pure (Aeson.encode EmptyResponse)
            "on-conversation-updated" -> pure (Aeson.encode ())
            "get-mls-clients" ->
              pure
                . Aeson.encode
                . Set.fromList
                . map (flip ClientInfo True . snd)
                . toList
                . pClients
                $ bob
            ms -> assertFailure ("unmocked endpoint called: " <> cs ms)

    void . withTempMockFederator' mockedResponse $
      postCommit setup
    liftIO $ mergeWelcome tmp (pClientQid bob) "group" "groupB.json" "welcome"
    message <-
      liftIO $
        spawn
          ( cli
              (pClientQid bob)
              tmp
              ["message", "--group", tmp </> "groupB.json", "hello from another backend"]
          )
          Nothing
    pure (setup, message)

  let bob = head users
  let alice = creator

  fedGalleyClient <- view tsFedGalleyClient
  cannon <- view tsCannon

  -- actual test

  let msr =
        MessageSendRequest
          { msrConvId = qUnqualified conversation,
            msrSender = qUnqualified (pUserId bob),
            msrRawMessage = Base64ByteString message
          }

  WS.bracketR cannon (qUnqualified (pUserId alice)) $ \ws -> do
    resp <- runFedClient @"send-mls-message" fedGalleyClient bobDomain msr
    liftIO $ do
      resp @?= MLSMessageResponseUpdates []
      WS.assertMatch_ (5 # Second) ws $
        wsAssertMLSMessage conversation (pUserId bob) message

testRemoteToLocalWrongConversation :: TestM ()
testRemoteToLocalWrongConversation = do
  -- alice is local, bob is remote
  -- alice creates a local conversation and invites bob
  -- bob then sends a message to the conversation

  let bobDomain = Domain "faraway.example.com"

  -- Simulate the whole MLS setup for both clients first. In reality,
  -- backend calls would need to happen in order for bob to get ahold of a
  -- welcome message, but that should not affect the correctness of the test.

  (MessagingSetup {..}, message) <- withSystemTempDirectory "mls" $ \tmp -> do
    setup <-
      aliceInvitesBobWithTmp
        tmp
        (1, RemoteUser bobDomain)
        def
          { createConv = CreateConv
          }
    bob <- assertOne (users setup)
    let mockedResponse fedReq =
          case frRPC fedReq of
            "mls-welcome" -> pure (Aeson.encode EmptyResponse)
            "on-new-remote-conversation" -> pure (Aeson.encode EmptyResponse)
            "on-conversation-updated" -> pure (Aeson.encode ())
            "get-mls-clients" ->
              pure
                . Aeson.encode
                . Set.fromList
                . map (flip ClientInfo True . snd)
                . toList
                . pClients
                $ bob
            ms -> assertFailure ("unmocked endpoint called: " <> cs ms)

    void . withTempMockFederator' mockedResponse $
      postCommit setup
    liftIO $ mergeWelcome tmp (pClientQid bob) "group" "groupB.json" "welcome"
    message <-
      liftIO $
        spawn
          ( cli
              (pClientQid bob)
              tmp
              ["message", "--group", tmp </> "groupB.json", "hello from another backend"]
          )
          Nothing
    pure (setup, message)

  let bob = head users

  fedGalleyClient <- view tsFedGalleyClient

  -- actual test
  randomConfId <- randomId
  let msr =
        MessageSendRequest
          { msrConvId = randomConfId,
            msrSender = qUnqualified (pUserId bob),
            msrRawMessage = Base64ByteString message
          }

  resp <- runFedClient @"send-mls-message" fedGalleyClient bobDomain msr
  liftIO $ resp @?= MLSMessageResponseError MLSGroupConversationMismatch

testRemoteNonMemberToLocal :: TestM ()
testRemoteNonMemberToLocal = do
  -- alice is local, bob is remote
  -- alice creates a local conversation and invites bob
  -- bob then sends a message to the conversation

  let bobDomain = Domain "faraway.example.com"

  -- Simulate the whole MLS setup for both clients first. In reality,
  -- backend calls would need to happen in order for bob to get ahold of a
  -- welcome message, but that should not affect the correctness of the test.

  (MessagingSetup {..}, message) <- withSystemTempDirectory "mls" $ \tmp -> do
    setup <-
      aliceInvitesBobWithTmp
        tmp
        (1, RemoteUser bobDomain)
        def
          { createConv = CreateConv
          }
    bob <- assertOne (users setup)
    liftIO $ mergeWelcome tmp (pClientQid bob) "group" "groupB.json" "welcome"
    message <-
      liftIO $
        spawn
          ( cli
              (pClientQid bob)
              tmp
              ["message", "--group", tmp </> "groupB.json", "hello from another backend"]
          )
          Nothing
    pure (setup, message)

  let bob = head users
  fedGalleyClient <- view tsFedGalleyClient

  -- actual test

  let msr =
        MessageSendRequest
          { msrConvId = qUnqualified conversation,
            msrSender = qUnqualified (pUserId bob),
            msrRawMessage = Base64ByteString message
          }

  resp <- runFedClient @"send-mls-message" fedGalleyClient bobDomain msr
  liftIO $ do
    resp @?= MLSMessageResponseError ConvNotFound

-- | The group exists in mls-test-cli's store, but not in wire-server's database.
propNonExistingConv :: TestM ()
propNonExistingConv = withSystemTempDirectory "mls" $ \tmp -> do
  (creator, [bob]) <- withLastPrekeys $ setupParticipants tmp def [(1, LocalUser)]

  let groupId = toBase64Text "test_group"
  groupJSON <-
    liftIO $
      spawn (cli (pClientQid creator) tmp ["group", "create", T.unpack groupId]) Nothing
  liftIO $ BS.writeFile (tmp </> cs groupId) groupJSON

  prop <-
    liftIO $
      spawn
        ( cli
            (pClientQid creator)
            tmp
            [ "proposal",
              "--group-in",
              tmp </> cs groupId,
              "--in-place",
              "add",
              tmp </> pClientQid bob
            ]
        )
        Nothing
  postMessage (qUnqualified (pUserId creator)) prop !!! do
    const 404 === statusCode
    const (Just "no-conversation") === fmap Wai.label . responseJsonError

propExistingConv :: TestM ()
propExistingConv = withSystemTempDirectory "mls" $ \tmp -> do
  (creator, [bob]) <- withLastPrekeys $ setupParticipants tmp def [(1, LocalUser)]
  -- setupGroup :: HasCallStack => FilePath -> CreateConv -> Participant -> String -> TestM (Qualified ConvId)
  void $ setupGroup tmp CreateConv creator "group.json"

  prop <- liftIO $ bareAddProposal tmp creator bob "group.json" "group.json"

  events <-
    fmap mmssEvents . responseJsonError
      =<< postMessage (qUnqualified (pUserId creator)) prop
      <!! const 201 === statusCode
  liftIO $ events @?= ([] :: [Event])

propInvalidEpoch :: TestM ()
propInvalidEpoch = withSystemTempDirectory "mls" $ \tmp -> do
  (creator, users) <- withLastPrekeys $ setupParticipants tmp def [(1, LocalUser), (1, LocalUser), (1, LocalUser)]
  (groupId, conversation) <- setupGroup tmp CreateConv creator "group.0.json"

  let (bob, charlie, dee) = assertThree users

  -- Add bob -> epoch 1
  do
    (commit, welcome) <-
      liftIO $
        setupCommit tmp creator "group.0.json" "group.1.json" $
          toList (pClients bob)
    testSuccessfulCommit MessagingSetup {users = [bob], ..}

  -- try to request a proposal that with too old epoch (0)
  do
    prop <- liftIO $ bareAddProposal tmp creator charlie "group.0.json" "group.0.json"
    err <-
      responseJsonError
        =<< postMessage (qUnqualified (pUserId creator)) prop
        <!! const 409 === statusCode
    liftIO $ Wai.label err @?= "mls-stale-message"

  -- try to request a proposal that is too new epoch (2)
  do
    void $
      liftIO $
        setupCommit tmp creator "group.1.json" "group.2.json" $
          toList (pClients charlie)
    prop <- liftIO $ bareAddProposal tmp creator dee "group.2.json" "group.2.json"
    err <-
      responseJsonError
        =<< postMessage (qUnqualified (pUserId creator)) prop
        <!! const 404 === statusCode
    liftIO $ Wai.label err @?= "mls-key-package-ref-not-found"

  -- same proposal with correct epoch (1)
  do
    prop <- liftIO $ bareAddProposal tmp creator dee "group.1.json" "group.1.json"
    postMessage (qUnqualified (pUserId creator)) prop
      !!! const 201 === statusCode

-- scenario:
-- alice1 creates a group and adds bob1
-- bob2 joins with external proposal (alice1 commits it)
-- bob2 adds charlie1
testExternalAddProposal :: TestM ()
testExternalAddProposal = do
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
    void $
      createPendingProposalCommit alice1
        >>= sendAndConsumeCommit

    -- bob adds charlie
    putOtherMemberQualified
      (qUnqualified alice)
      bob
      (OtherMemberUpdate (Just roleNameWireAdmin))
      qcnv
      !!! const 200 === statusCode
    createAddCommit bob2 [charlie]
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
    postMessage (qUnqualified charlie) (mpMessage prop)
      !!! do
        const 422 === statusCode
        const (Just "mls-unsupported-proposal") === fmap Wai.label . responseJsonError

-- scenario:
-- alice adds bob
-- charlie attempts to join with an external add proposal
testExternalAddProposalWrongUser :: TestM ()
testExternalAddProposalWrongUser = do
  users@[_, bob, charlie] <- createAndConnectUsers (replicate 3 Nothing)

  runMLSTest $ do
    -- setup clients
    [alice1, bob1, charlie1] <- traverse createMLSClient users
    void $ uploadNewKeyPackage bob1

    void $ setupMLSGroup alice1
    void $
      createAddCommit alice1 [bob]
        >>= sendAndConsumeCommit

    prop <- createExternalAddProposal charlie1
    postMessage (qUnqualified charlie) (mpMessage prop)
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
propUnsupported = withSystemTempDirectory "mls" $ \tmp -> do
  MessagingSetup {..} <- aliceInvitesBobWithTmp tmp (1, LocalUser) def {createConv = CreateConv}
  aliceKP <- liftIO $ do
    d <- BS.readFile (tmp </> pClientQid creator)
    either (\e -> assertFailure ("could not parse key package: " <> T.unpack e)) pure $
      decodeMLS' d
  let alicePublicKey = bcSignatureKey $ kpCredential aliceKP

  -- "\0 " corresponds to 0020 in TLS encoding, which is the length of the
  -- following public key
  file <-
    liftIO . BS.readFile $
      tmp </> pClientQid creator <> ".db" </> cs (B64U.encode $ "\0 " <> alicePublicKey)
  let s =
        file ^.. key "signature_private_key" . key "value" . _Array . traverse . _Integer
          & fmap fromIntegral
          & BS.pack
  let (privKey, pubKey) = BS.splitAt 32 s
  liftIO $ alicePublicKey @?= pubKey
  let aliceRef =
        kpRef
          MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519
          . KeyPackageData
          . rmRaw
          . kpTBS
          $ aliceKP
  let Just appAckMsg =
        maybeCryptoError $
          mkAppAckProposalMessage
            groupId
            (Epoch 0)
            aliceRef
            []
            <$> C.secretKey privKey
            <*> C.publicKey pubKey
      msgSerialised =
        LBS.toStrict . runPut . serialiseMLS $ appAckMsg

  postMessage (qUnqualified . pUserId $ creator) msgSerialised
    !!! const 201 === statusCode

testBackendRemoveProposalLocalConvLocalUser :: TestM ()
testBackendRemoveProposalLocalConvLocalUser = withSystemTempDirectory "mls" $ \tmp -> do
  saveRemovalKey (tmp </> "removal.key")
  MessagingSetup {..} <- aliceInvitesBobWithTmp tmp (2, LocalUser) def {createConv = CreateConv}
  let [bobParticipant] = users
  let bob = pUserId bobParticipant
  let alice = pUserId creator
  testSuccessfulCommit MessagingSetup {users = [bobParticipant], ..}

  kprefs <- (fromJust . kpRef' . snd) <$$> liftIO (readKeyPackages tmp bobParticipant)

  c <- view tsCannon
  WS.bracketR c (qUnqualified alice) $ \wsA -> do
    deleteUser (qUnqualified bob) !!! const 200 === statusCode

    for_ kprefs $ \kp ->
      WS.assertMatch_ (5 # WS.Second) wsA $ \notification -> do
        msg <- wsAssertBackendRemoveProposal bob conversation kp notification
        void . liftIO $
          spawn
            ( cli
                (pClientQid creator)
                tmp
                $ [ "consume",
                    "--group",
                    tmp </> "group",
                    "--in-place",
                    "--signer-key",
                    tmp </> "removal.key",
                    "-"
                  ]
            )
            (Just msg)

  -- alice commits the external proposals
  (commit', _) <- liftIO $ pendingProposalsCommit tmp creator "group"
  events <-
    postCommit
      MessagingSetup
        { commit = commit',
          ..
        }
  liftIO $ events @?= []

testBackendRemoveProposalLocalConvRemoteUser :: TestM ()
testBackendRemoveProposalLocalConvRemoteUser = withSystemTempDirectory "mls" $ \tmp -> do
  let opts =
        def
          { createClients = DontCreateClients,
            createConv = CreateConv
          }
  (alice, [bob]) <-
    withLastPrekeys $
      setupParticipants tmp opts [(1, RemoteUser (Domain "faraway.example.com"))]
  (groupId, conversation) <- setupGroup tmp CreateConv alice "group"
  (commit, welcome) <- liftIO $ setupCommit tmp alice "group" "group" (pClients bob)

  let mock req = case frRPC req of
        "on-conversation-updated" -> pure (Aeson.encode ())
        "on-new-remote-conversation" -> pure (Aeson.encode EmptyResponse)
        "on-mls-message-sent" -> pure (Aeson.encode EmptyResponse)
        "get-mls-clients" ->
          pure
            . Aeson.encode
            . Set.fromList
            . map (flip ClientInfo True . snd)
            . toList
            . pClients
            $ bob
        ms -> assertFailure ("unmocked endpoint called: " <> cs ms)

  void $
    withTempMockFederator' mock $ do
      c <- view tsCannon
      WS.bracketR c (qUnqualified (pUserId alice)) $ \wsA -> do
        void $ postCommit MessagingSetup {creator = alice, users = [bob], ..}

        kprefs <- (fromJust . kpRef' . snd) <$$> liftIO (readKeyPackages tmp bob)

        fedGalleyClient <- view tsFedGalleyClient
        void $
          runFedClient
            @"on-user-deleted-conversations"
            fedGalleyClient
            (qDomain (pUserId bob))
            ( UserDeletedConversationsNotification
                { udcvUser = qUnqualified (pUserId bob),
                  udcvConversations = unsafeRange [qUnqualified conversation]
                }
            )

        for_ kprefs $ \kp ->
          WS.assertMatch_ (5 # WS.Second) wsA $ \notification ->
            void $
              wsAssertBackendRemoveProposal (pUserId bob) conversation kp notification
