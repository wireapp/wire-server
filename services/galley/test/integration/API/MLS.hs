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
{-# OPTIONS_GHC -Wwarn #-}

module API.MLS (tests) where

import API.MLS.Util
import API.Util
import Bilge hiding (head)
import Bilge.Assert
import Cassandra
import Control.Lens (view)
import qualified Control.Monad.State as State
import Crypto.Error
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.Aeson as Aeson
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Default
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
import qualified Network.Wai.Utilities.Error as Wai
import System.IO.Temp
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
import Wire.API.Federation.API.Common
import Wire.API.Federation.API.Galley
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Keys
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
          test s "anyone removes a non-existing client from a group" testRemoveDeletedClient
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
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)
  void $ postConvQualified (qUnqualified alice) defNewProteusConv
  groupId <-
    liftIO $ fmap (GroupId . BS.pack) (replicateM 32 (generate arbitrary))
  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    void $ uploadNewKeyPackage bob1
    createGroup alice1 groupId
    mp <- createAddCommit alice1 [bob]
    err <-
      responseJsonError
        =<< postMessage (ciUser alice1) (mpMessage mp) <!! const 404 === statusCode
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
    void $ rollBackClient alice1
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
        =<< postMessage (qUnqualified alice) (mpMessage commit)
          <!! statusCode === const 409
    liftIO $ Wai.label err @?= "mls-client-mismatch"

testRemoveDeletedClient :: TestM ()
testRemoveDeletedClient = do
  [alice, bob, charlie] <- createAndConnectUsers [Nothing, Nothing, Nothing]

  runMLSTest $ do
    clients@[alice1, _bob1, bob2, charlie1] <- traverse createMLSClient [alice, bob, bob, charlie]
    traverse_ uploadNewKeyPackage (tail clients)
    void $ setupMLSGroup alice1
    void $ createAddCommit alice1 [bob, charlie] >>= sendAndConsumeCommit

    liftTest $ do
      cannon <- view tsCannon
      WS.bracketR cannon (qUnqualified bob) $ \ws -> do
        deleteClient (qUnqualified bob) (ciClient bob2) (Just defPassword)
          !!! statusCode === const 200
        -- check that the corresponding event is received
        liftIO $
          WS.assertMatch_ (5 # WS.Second) ws $
            wsAssertClientRemoved (ciClient bob2)

    events <- createRemoveCommit charlie1 [bob2] >>= sendAndConsumeCommit
    liftIO $ assertEqual "a non-admin received conversation events when removing a client" [] events

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

    let mock req = case frRPC req of
          "send-mls-message" -> pure (Aeson.encode (MLSMessageResponseUpdates []))
          rpc -> assertFailure $ "unmocked RPC called: " <> T.unpack rpc

    (_, reqs) <-
      withTempMockFederator' mock $
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
      msrConvId bdy @?= qUnqualified qcnv
      msrSender bdy @?= qUnqualified bob
      msrRawMessage bdy @?= Base64ByteString (mpMessage message)
  where
    receiveOnConvUpdated conv origUser joiner = do
      client <- view tsFedGalleyClient
      now <- liftIO getCurrentTime
      let cu =
            ConversationUpdate
              { cuTime = now,
                cuOrigUserId = origUser,
                cuConvId = qUnqualified conv,
                cuAlreadyPresentUsers = [qUnqualified joiner],
                cuAction =
                  SomeConversationAction
                    SConversationJoinTag
                    ConversationJoin
                      { cjUsers = pure joiner,
                        cjRole = roleNameWireMember
                      }
              }
      void $
        runFedClient
          @"on-conversation-updated"
          client
          (qDomain conv)
          cu

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

    let mock req = case frRPC req of
          "send-mls-message" -> pure (Aeson.encode (MLSMessageResponseUpdates []))
          rpc -> assertFailure $ "unmocked RPC called: " <> T.unpack rpc

    void $
      withTempMockFederator' mock $ do
        galley <- viewGalley

        -- bob sends a message: step 12
        post
          ( galley . paths ["mls", "messages"]
              . zUser (qUnqualified bob)
              . zConn "conn"
              . content "message/mls"
              . bytes (mpMessage message)
          )
          !!! do
            const 404 === statusCode
            const (Just "no-conversation-member")
              === fmap Wai.label . responseJsonError

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
      events <- sendAndConsumeMessage message
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
      events <- sendAndConsumeMessage message
      liftIO $ events @?= []

      -- check that the corresponding event is received

      liftIO $
        WS.assertMatchN_ (5 # WS.Second) wss $
          wsAssertMLSMessage conversation bob (mpMessage message)

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
  [alice, bob] <- createAndConnectUsers [Nothing, Just (domainText bobDomain)]

  -- Simulate the whole MLS setup for both clients first. In reality,
  -- backend calls would need to happen in order for bob to get ahold of a
  -- welcome message, but that should not affect the correctness of the test.

  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]

    (_groupId, qcnv) <- setupMLSGroup alice1
    kpb <- claimKeyPackages alice1 bob
    mp <- createAddCommit alice1 [bob]

    let mockedResponse fedReq =
          case frRPC fedReq of
            "mls-welcome" -> pure (Aeson.encode EmptyResponse)
            "on-new-remote-conversation" -> pure (Aeson.encode EmptyResponse)
            "on-conversation-updated" -> pure (Aeson.encode ())
            "get-mls-clients" ->
              pure
                . Aeson.encode
                . Set.singleton
                $ ClientInfo (ciClient bob1) True
            "claim-key-packages" -> pure . Aeson.encode $ kpb
            ms -> assertFailure ("unmocked endpoint called: " <> cs ms)

    void . withTempMockFederator' mockedResponse $
      sendAndConsumeCommit mp

    traverse_ consumeWelcome (mpWelcome mp)
    message <- createApplicationMessage bob1 "hello from another backend"

    fedGalleyClient <- view tsFedGalleyClient
    cannon <- view tsCannon

    -- actual test

    let msr =
          MessageSendRequest
            { msrConvId = qUnqualified qcnv,
              msrSender = qUnqualified bob,
              msrRawMessage = Base64ByteString (mpMessage message)
            }

    WS.bracketR cannon (qUnqualified alice) $ \ws -> do
      resp <- runFedClient @"send-mls-message" fedGalleyClient bobDomain msr
      liftIO $ do
        resp @?= MLSMessageResponseUpdates []
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

    let mockedResponse fedReq =
          case frRPC fedReq of
            "mls-welcome" -> pure (Aeson.encode EmptyResponse)
            "on-new-remote-conversation" -> pure (Aeson.encode EmptyResponse)
            "on-conversation-updated" -> pure (Aeson.encode ())
            "get-mls-clients" ->
              pure
                . Aeson.encode
                . Set.singleton
                $ ClientInfo (ciClient bob1) True
            ms -> assertFailure ("unmocked endpoint called: " <> cs ms)

    void . withTempMockFederator' mockedResponse $ sendAndConsumeCommit mp
    traverse_ consumeWelcome (mpWelcome mp)
    message <- createApplicationMessage bob1 "hello from another backend"

    fedGalleyClient <- view tsFedGalleyClient

    -- actual test
    randomConfId <- randomId
    let msr =
          MessageSendRequest
            { msrConvId = randomConfId,
              msrSender = qUnqualified bob,
              msrRawMessage = Base64ByteString (mpMessage message)
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
          MessageSendRequest
            { msrConvId = qUnqualified qcnv,
              msrSender = qUnqualified bob,
              msrRawMessage = Base64ByteString (mpMessage message)
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
    postMessage (ciUser alice1) (mpMessage prop) !!! do
      const 404 === statusCode
      const (Just "no-conversation") === fmap Wai.label . responseJsonError

propExistingConv :: TestM ()
propExistingConv = do
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)
  runMLSTest $ do
    [alice1, bob1] <- traverse createMLSClient [alice, bob]
    void $ uploadNewKeyPackage bob1
    void $ setupMLSGroup alice1
    events <- createAddProposals alice1 [bob] >>= traverse sendAndConsumeMessage

    liftIO $ events @?= [[]]

propInvalidEpoch :: TestM ()
propInvalidEpoch = do
  users@[alice, bob, charlie, dee] <- createAndConnectUsers (replicate 4 Nothing)
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
          =<< postMessage (qUnqualified alice) (mpMessage prop)
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
          =<< postMessage (qUnqualified alice) (mpMessage prop)
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
    postMessage (ciUser alice1) msgData !!! const 201 === statusCode

testBackendRemoveProposalLocalConvLocalUser :: TestM ()
testBackendRemoveProposalLocalConvLocalUser = do
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)
  runMLSTest $ do
    [alice1, bob1, bob2] <- traverse createMLSClient [alice, bob, bob]
    traverse_ uploadNewKeyPackage [bob1, bob2]
    (_, qcnv) <- setupMLSGroup alice1

    void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommit

    bobClients <-
      fmap (filter (\(cid, _) -> cidQualifiedUser cid == bob)) $
        currentGroupFile alice1 >>= liftIO . readGroupState

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

        void $
          for_ kprefs $ \kp ->
            WS.assertMatch (5 # WS.Second) wsA $
              wsAssertBackendRemoveProposal (pUserId bob) conversation kp
