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
import Control.Arrow
import Control.Lens (view, (^..))
import Crypto.Error
import qualified Crypto.PubKey.Ed25519 as C
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64U
import Data.ByteString.Conversion
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
import Wire.API.MLS.Group (convToGroupId)
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Keys
import Wire.API.MLS.Message
import Wire.API.MLS.Serialisation
import Wire.API.Message
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
          test s "add new client of an already-present user to a conversation" testAddNewClient,
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
        "External Proposal"
        [ test s "member adds new client" testExternalAddProposal,
          test s "non-member adds new client" testExternalAddProposalWrongUser,
          test s "member adds unknown new client" testExternalAddProposalWrongClient
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
  withSystemTempDirectory "mls" $ \tmp -> do
    (alice, [bob]) <- withLastPrekeys $ setupParticipants tmp def [(1, LocalUser)]
    _ <- setupGroup tmp CreateConv alice "group"

    (_commit, _welcome) <-
      liftIO $
        setupCommit tmp alice "group" "group" $
          toList (pClients bob)
    liftIO $ mergeWelcome tmp (pClientQid bob) "group" "group" "welcome"
    message <- liftIO $ createMessage tmp bob "group" "some text"

    -- send the message as bob, who is not in the conversation
    err <-
      responseJsonError
        =<< postMessage (qUnqualified (pUserId bob)) message
        <!! const 404 === statusCode

    liftIO $ Wai.label err @?= "no-conversation"

testLocalWelcome :: TestM ()
testLocalWelcome = do
  MessagingSetup {..} <- aliceInvitesBob (1, LocalUser) def
  let bob = head users

  cannon <- view tsCannon

  WS.bracketR cannon (qUnqualified (pUserId bob)) $ \wsB -> do
    -- send welcome message
    postWelcome (qUnqualified $ pUserId creator) welcome
      !!! const 201 === statusCode

    -- check that the corresponding event is received
    void . liftIO $
      WS.assertMatch (5 # WS.Second) wsB $
        wsAssertMLSWelcome (pUserId bob) welcome

testWelcomeNoKey :: TestM ()
testWelcomeNoKey = do
  MessagingSetup {..} <- aliceInvitesBob (1, LocalUser) def {createClients = CreateWithoutKey}

  postWelcome (qUnqualified (pUserId creator)) welcome
    !!! const 404 === statusCode

testRemoteWelcome :: TestM ()
testRemoteWelcome = do
  -- 1. Create a conversation with Alice and Bob
  let bobDomain = Domain "b.far-away.example.com"
      opts = def {createConv = CreateConv, createClients = DontCreateClients}
  MessagingSetup {..} <- aliceInvitesBob (1, RemoteUser bobDomain) opts
  let alice = creator

  let okResp = EmptyResponse
  let mockedResponse fedReq =
        case frRPC fedReq of
          "mls-welcome" -> pure (Aeson.encode okResp)
          ms -> assertFailure ("unmocked endpoint called: " <> cs ms)

  (_resp, reqs) <-
    withTempMockFederator' mockedResponse $
      postWelcome (qUnqualified $ pUserId alice) welcome
        !!! const 201 === statusCode

  --  Assert the correct federated call is made.
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

testSuccessfulRemoveMemberFromConvCommit ::
  HasCallStack =>
  Participant ->
  [Participant] ->
  Qualified ConvId ->
  ByteString ->
  [Participant] ->
  TestM ()
testSuccessfulRemoveMemberFromConvCommit admin users conv commit participantsToRemove = do
  cannon <- view tsCannon

  WS.bracketRN cannon (map (qUnqualified . pUserId) users) $ \wss -> do
    events :: [Event] <-
      fmap mmssEvents . responseJsonError
        =<< postMessage (qUnqualified (pUserId admin)) commit
        <!! statusCode === const 201

    e <- assertOne events
    liftIO $ assertLeaveEvent conv (pUserId admin) (map pUserId participantsToRemove) e

    for_ wss $ \ws ->
      WS.assertMatch_ (5 # WS.Second) ws $
        wsAssertMembersLeave conv (pUserId admin) (map pUserId participantsToRemove)

    -- all users (including the removed ones) receive the commit
    for_ wss $ \ws -> do
      WS.assertMatch_ (5 # WS.Second) ws $
        wsAssertMLSMessage conv (pUserId admin) commit

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
  setup@MessagingSetup {..} <-
    aliceInvitesBob
      (1, LocalUser)
      def
        { createConv = CreateConv,
          numCreatorClients = 3
        }
  testSuccessfulCommit setup

  -- check that bob can now see the conversation
  let bob = head users
  convs <-
    responseJsonError =<< getConvs (qUnqualified (pUserId bob)) Nothing Nothing
      <!! const 200 === statusCode
  liftIO $
    assertBool
      "Users added to an MLS group should find it when listing conversations"
      (conversation `elem` map cnvQualifiedId (convList convs))

testAddUserNotConnected :: TestM ()
testAddUserNotConnected = do
  setup@MessagingSetup {..} <- aliceInvitesBob (1, LocalUser) def {createConv = CreateConv, makeConnections = False}
  let bob = head users

  -- try to add unconnected user
  err <- testFailedCommit setup 403
  liftIO $ Wai.label err @?= "not-connected"

  -- now connect and retry
  connectUsers (qUnqualified (pUserId creator)) (pure (qUnqualified (pUserId bob)))
  testSuccessfulCommit setup

testAddUserWithProteusClients :: TestM ()
testAddUserWithProteusClients = do
  setup <- withSystemTempDirectory "mls" $ \tmp -> do
    (alice, users@[bob]) <- withLastPrekeys $ do
      -- bob has 2 MLS clients
      participants@(_, [bob]) <- setupParticipants tmp def [(2, LocalUser)]

      -- and a non-MLS client
      void $ takeLastPrekey >>= lift . randomClient (qUnqualified (pUserId bob))

      pure participants

    -- alice creates a conversation and adds Bob's MLS clients
    (groupId, conversation) <- setupGroup tmp CreateConv alice "group"
    (commit, welcome) <- liftIO $ setupCommit tmp alice "group" "group" (pClients bob)

    pure MessagingSetup {creator = alice, ..}

  testSuccessfulCommit setup

testAddUserPartial :: TestM ()
testAddUserPartial = do
  (creator, commit) <- withSystemTempDirectory "mls" $ \_tmp -> do
    let tmp = "/tmp/mls"
    -- Bob has 3 clients, Charlie has 2
    (alice, [bob, charlie]) <- withLastPrekeys $ setupParticipants tmp def ((,LocalUser) <$> [3, 2])

    -- upload one more key package for each of bob's clients
    -- this makes sure the unused client has at least one key package, and
    -- therefore will be considered MLS-capable
    for_ (pClients bob) $ \(cid, c) -> do
      kp <-
        liftIO $
          decodeMLSError
            =<< spawn (cli cid tmp ["key-package", "create"]) Nothing
      addKeyPackage def {mapKeyPackage = False, setPublicKey = False} (pUserId bob) c kp

    void $ setupGroup tmp CreateConv alice "group"
    (commit, _) <-
      liftIO . setupCommit tmp alice "group" "group" $
        -- only 2 out of the 3 clients of Bob's are added to the conversation
        NonEmpty.take 2 (pClients bob) <> toList (pClients charlie)
    pure (alice, commit)

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
      <!! const 409 === statusCode
  liftIO $ Wai.label err @?= "mls-client-mismatch"

testAddClientPartial :: TestM ()
testAddClientPartial = withSystemTempDirectory "mls" $ \tmp -> do
  withLastPrekeys $ do
    (alice, [bob]) <- setupParticipants tmp def ((,LocalUser) <$> [1])
    (groupId, conversation) <- lift $ setupGroup tmp CreateConv alice "group"
    (commit, welcome) <- liftIO . setupCommit tmp alice "group" "group" $ pClients bob
    let setup =
          MessagingSetup
            { creator = alice,
              users = [bob],
              ..
            }
    lift $ testSuccessfulCommit setup

    -- create more clients for Bob, only take the first one
    newClient <- fmap head . replicateM 2 $ do
      setupUserClient tmp CreateWithKey True (pUserId bob)

    -- add new client
    (commit', welcome') <-
      liftIO $
        setupCommit
          tmp
          alice
          "group"
          "group"
          [(userClientQid (pUserId bob) newClient, newClient)]

    lift $ testSuccessfulCommitWithNewUsers setup {commit = commit', welcome = welcome'} []

testAddNewClient :: TestM ()
testAddNewClient = do
  withSystemTempDirectory "mls" $ \tmp -> withLastPrekeys $ do
    -- bob starts with a single client
    (creator, users@[bob]) <- setupParticipants tmp def [(1, LocalUser)]
    (groupId, conversation) <- lift $ setupGroup tmp CreateConv creator "group"

    -- creator sends first commit message
    do
      (commit, welcome) <- liftIO $ setupCommit tmp creator "group" "group" (pClients bob)
      lift $ testSuccessfulCommit MessagingSetup {..}

    do
      -- then bob adds a new client
      c <- setupUserClient tmp CreateWithKey True (pUserId bob)
      let bobC = (userClientQid (pUserId bob) c, c)
      -- which gets added to the group
      (commit, welcome) <- liftIO $ setupCommit tmp creator "group" "group" [bobC]
      -- and the corresponding commit is sent
      lift $ testSuccessfulCommitWithNewUsers MessagingSetup {..} []

testSendAnotherUsersCommit :: TestM ()
testSendAnotherUsersCommit = do
  withSystemTempDirectory "mls" $ \tmp -> withLastPrekeys $ do
    -- bob starts with a single client
    (creator, users@[bob]) <- setupParticipants tmp def [(1, LocalUser)]
    (groupId, conversation) <- lift $ setupGroup tmp CreateConv creator "group"

    -- creator sends first commit message
    do
      (commit, welcome) <- liftIO $ setupCommit tmp creator "group" "group" (pClients bob)
      lift $ testSuccessfulCommit MessagingSetup {..}

    do
      -- then bob adds a new client
      c <- setupUserClient tmp CreateWithKey True (pUserId bob)
      let bobC = (userClientQid (pUserId bob) c, c)
      -- which gets added to the group
      (commit, _welcome) <- liftIO $ setupCommit tmp creator "group" "group" [bobC]
      -- and the corresponding commit is sent from bob instead of the creator
      err <- lift (responseJsonError =<< postMessage (qUnqualified (pUserId bob)) commit <!! const 409 === statusCode)
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
testStaleCommit = withSystemTempDirectory "mls" $ \tmp -> do
  (creator, users) <- withLastPrekeys $ setupParticipants tmp def ((,LocalUser) <$> [2, 3])
  (groupId, conversation) <- setupGroup tmp CreateConv creator "group.0"
  let (users1, users2) = splitAt 1 users

  -- add the first batch of users to the conversation, but do not overwrite group
  do
    (commit, welcome) <-
      liftIO $
        setupCommit tmp creator "group.0" "group.1" $
          users1 >>= toList . pClients
    testSuccessfulCommit MessagingSetup {users = users1, ..}

  -- now add the rest of the users to the original group state
  do
    (commit, welcome) <-
      liftIO $
        setupCommit tmp creator "group.0" "group.2" $
          users2 >>= toList . pClients
    err <- testFailedCommit MessagingSetup {..} 409
    liftIO $ Wai.label err @?= "mls-stale-message"

testAddRemoteUser :: TestM ()
testAddRemoteUser = do
  setup <-
    aliceInvitesBob
      (1, RemoteUser (Domain "faraway.example.com"))
      def
        { createClients = DontCreateClients,
          createConv = CreateConv
        }
  bob <- assertOne (users setup)
  let mock req = case frRPC req of
        "on-conversation-updated" -> pure (Aeson.encode ())
        "on-new-remote-conversation" -> pure (Aeson.encode EmptyResponse)
        "get-mls-clients" ->
          let clients =
                Set.fromList
                  . map snd
                  . toList
                  . pClients
                  $ bob
           in pure (Aeson.encode (clients, clients))
        ms -> assertFailure ("unmocked endpoint called: " <> cs ms)
  (events, reqs) <- withTempMockFederator' mock $ do
    postCommit setup

  liftIO $ do
    req <- assertOne $ filter ((== "on-conversation-updated") . frRPC) reqs
    frTargetDomain req @?= qDomain (pUserId bob)
    bdy <- case Aeson.eitherDecode (frBody req) of
      Right b -> pure b
      Left e -> assertFailure $ "Could not parse on-conversation-updated request body: " <> e
    cuOrigUserId bdy @?= pUserId (creator setup)
    cuConvId bdy @?= qUnqualified (conversation setup)
    cuAlreadyPresentUsers bdy @?= [qUnqualified (pUserId bob)]
    cuAction bdy
      @?= SomeConversationAction
        SConversationJoinTag
        ConversationJoin
          { cjUsers = pure (pUserId bob),
            cjRole = roleNameWireMember
          }

  liftIO $ do
    event <- assertOne events
    assertJoinEvent
      (conversation setup)
      (pUserId (creator setup))
      [pUserId bob]
      roleNameWireMember
      event

testCommitLock :: TestM ()
testCommitLock = withSystemTempDirectory "mls" $ \tmp -> do
  -- create MLS conversation
  (creator, users) <- withLastPrekeys $ setupParticipants tmp def ((,LocalUser) <$> [2, 2, 2])
  (groupId, conversation) <- setupGroup tmp CreateConv creator "group"
  let (users1, usersX) = splitAt 1 users
  let (users2, users3) = splitAt 1 usersX
  void $ assertOne users1
  void $ assertOne users2
  void $ assertOne users3

  -- initial user can be added
  do
    (commit, welcome) <-
      liftIO $
        setupCommit tmp creator "group" "group" $
          users1 >>= toList . pClients
    testSuccessfulCommit MessagingSetup {users = users1, ..}

  -- can commit without blocking
  do
    (commit, welcome) <-
      liftIO $
        setupCommit tmp creator "group" "group" $
          users2 >>= toList . pClients
    testSuccessfulCommit MessagingSetup {users = users2, ..}

  -- block epoch
  casClient <- view tsCass
  runClient casClient $ insertLock (convToGroupId (qTagUnsafe conversation)) (Epoch 2)

  -- commit fails due to competing lock
  do
    (commit, welcome) <-
      liftIO $
        setupCommit tmp creator "group" "group" $
          users3 >>= toList . pClients
    -- assert HTTP 409 on next attempt to commit
    err <- testFailedCommit MessagingSetup {..} 409
    liftIO $ Wai.label err @?= "mls-stale-message"

  -- unblock epoch
  runClient casClient $ deleteLock (convToGroupId (qTagUnsafe conversation)) (Epoch 2)
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
    unlock :: PrepQuery W (GroupId, Epoch) ()
    unlock = "delete from mls_commit_locks where group_id = ? and epoch = ?"
    deleteLock groupId epoch =
      retry x5 $
        write
          unlock
          ( params
              LocalQuorum
              (groupId, epoch)
          )

testAddUserBareProposalCommit :: TestM ()
testAddUserBareProposalCommit = withSystemTempDirectory "mls" $ \tmp -> do
  (alice, [bob]) <- withLastPrekeys $ setupParticipants tmp def [(1, LocalUser)]

  (groupId, conversation) <- setupGroup tmp CreateConv alice "group"

  prop <- liftIO $ bareAddProposal tmp alice bob "group" "group"
  postMessage (qUnqualified (pUserId alice)) prop
    !!! const 201 === statusCode

  (commit, mbWelcome) <-
    liftIO $
      pendingProposalsCommit tmp alice "group"

  welcome <- assertJust mbWelcome

  testSuccessfulCommit MessagingSetup {creator = alice, users = [bob], ..}

  -- check that bob can now see the conversation
  convs <-
    responseJsonError =<< getConvs (qUnqualified (pUserId bob)) Nothing Nothing
      <!! const 200 === statusCode
  liftIO $
    assertBool
      "Users added to an MLS group should find it when listing conversations"
      (conversation `elem` map cnvQualifiedId (convList convs))

testUnknownProposalRefCommit :: TestM ()
testUnknownProposalRefCommit = withSystemTempDirectory "mls" $ \tmp -> do
  (alice, [bob]) <- withLastPrekeys $ setupParticipants tmp def [(1, LocalUser)]

  (groupId, conversation) <- setupGroup tmp CreateConv alice "group"

  -- create proposal, but don't send it to group
  void $ liftIO $ bareAddProposal tmp alice bob "group" "group"

  (commit, mbWelcome) <-
    liftIO $
      pendingProposalsCommit tmp alice "group"

  welcome <- assertJust mbWelcome

  err <- testFailedCommit (MessagingSetup {creator = alice, users = [bob], ..}) 404
  liftIO $ Wai.label err @?= "mls-proposal-not-found"

testCommitNotReferencingAllProposals :: TestM ()
testCommitNotReferencingAllProposals = withSystemTempDirectory "mls" $ \tmp -> do
  (alice, [bob, dee]) <- withLastPrekeys $ setupParticipants tmp def [(1, LocalUser), (1, LocalUser)]

  (groupId, conversation) <- setupGroup tmp CreateConv alice "group"

  propBob <- liftIO $ bareAddProposal tmp alice bob "group" "group"
  postMessage (qUnqualified (pUserId alice)) propBob
    !!! const 201 === statusCode

  propDee <- liftIO $ bareAddProposal tmp alice dee "group" "group2"
  postMessage (qUnqualified (pUserId alice)) propDee
    !!! const 201 === statusCode

  (commit, mbWelcome) <-
    liftIO $
      pendingProposalsCommit tmp alice "group"

  welcome <- assertJust mbWelcome

  err <- testFailedCommit (MessagingSetup {creator = alice, users = [bob, dee], ..}) 409
  liftIO $ Wai.label err @?= "mls-commit-missing-references"

testAdminRemovesUserFromConv :: TestM ()
testAdminRemovesUserFromConv = withSystemTempDirectory "mls" $ \tmp -> do
  MessagingSetup {..} <- aliceInvitesBobWithTmp tmp (2, LocalUser) def {createConv = CreateConv}
  let [bob] = users

  testSuccessfulCommit MessagingSetup {users = [bob], ..}

  (removalCommit, _mbWelcome) <- liftIO $ setupRemoveCommit tmp creator "group" "group" (pClients bob)

  do
    convs <-
      responseJsonError =<< getConvs (qUnqualified (pUserId bob)) Nothing Nothing
        <!! const 200 === statusCode
    liftIO $
      assertBool
        "bob is in conversation before he gets removed"
        (conversation `elem` map cnvQualifiedId (convList convs))

  testSuccessfulRemoveMemberFromConvCommit creator [bob] conversation removalCommit [bob]

  do
    convs <-
      responseJsonError =<< getConvs (qUnqualified (pUserId bob)) Nothing Nothing
        <!! const 200 === statusCode
    liftIO $
      assertBool
        "bob is not longer part of conversation after the commit"
        (conversation `notElem` map cnvQualifiedId (convList convs))

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

  when deleteClientBefore $
    deleteClient (qUnqualified (pUserId bob)) (snd bobClient2) (Just defPassword)
      !!! statusCode === const 200

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
testRemoteAppMessage = withSystemTempDirectory "mls" $ \tmp -> do
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
  message <-
    liftIO $
      spawn (cli (pClientQid alice) tmp ["message", "--group", tmp </> "group", "some text"]) Nothing

  let mock req = case frRPC req of
        "on-conversation-updated" -> pure (Aeson.encode ())
        "on-new-remote-conversation" -> pure (Aeson.encode EmptyResponse)
        "on-mls-message-sent" -> pure (Aeson.encode EmptyResponse)
        "get-mls-clients" ->
          let clients =
                Set.fromList
                  . map snd
                  . toList
                  . pClients
                  $ bob
           in pure (Aeson.encode (clients, clients))
        ms -> assertFailure ("unmocked endpoint called: " <> cs ms)
  (events :: [Event], reqs) <- fmap (first mmssEvents) . withTempMockFederator' mock $ do
    galley <- viewGalley
    void $ postCommit MessagingSetup {creator = alice, users = [bob], ..}
    let v2 = toByteString' (toLower <$> show V2)
    responseJsonError
      =<< post
        ( galley . paths [v2, "mls", "messages"]
            . zUser (qUnqualified (pUserId alice))
            . zConn "conn"
            . content "message/mls"
            . bytes message
        )
      <!! const 201
      === statusCode

  liftIO $ do
    req <- assertOne $ filter ((== "on-mls-message-sent") . frRPC) reqs
    frTargetDomain req @?= qDomain (pUserId bob)
    bdy <- case Aeson.eitherDecode (frBody req) of
      Right b -> pure b
      Left e -> assertFailure $ "Could not parse on-mls-message-sent request body: " <> e
    rmmSender bdy @?= pUserId alice
    rmmConversation bdy @?= qUnqualified conversation
    rmmRecipients bdy
      @?= [(qUnqualified (pUserId bob), c) | (_, c) <- toList (pClients bob)]
    rmmMessage bdy @?= Base64ByteString message

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
testAppMessage = withSystemTempDirectory "mls" $ \tmp -> do
  (creator, users) <- withLastPrekeys $ setupParticipants tmp def ((,LocalUser) <$> [1, 2, 3])
  (groupId, conversation) <- setupGroup tmp CreateConv creator "group"

  (commit, welcome) <-
    liftIO $
      setupCommit tmp creator "group" "group" $
        users >>= toList . pClients

  void $ postCommit MessagingSetup {..}
  message <- liftIO $ createMessage tmp creator "group" "some text"

  galley <- viewGalley
  cannon <- view tsCannon

  WS.bracketRN
    cannon
    (map (qUnqualified . pUserId) (creator : users))
    $ \wss -> do
      post
        ( galley . paths ["mls", "messages"]
            . zUser (qUnqualified (pUserId creator))
            . zConn "conn"
            . content "message/mls"
            . bytes message
        )
        !!! const 201
        === statusCode

      -- check that the corresponding event is received

      liftIO $
        WS.assertMatchN_ (5 # WS.Second) wss $
          wsAssertMLSMessage conversation (pUserId creator) message

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
              let clients =
                    Set.fromList
                      . map snd
                      . toList
                      . pClients
                      $ bob
               in pure (Aeson.encode (clients, clients))
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
                . map snd
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

testExternalAddProposal :: TestM ()
testExternalAddProposal = withSystemTempDirectory "mls" $ \tmp -> do
  (creator, [bob]) <- withLastPrekeys $ setupParticipants tmp def [(1, LocalUser)]
  (groupId, conversation) <- setupGroup tmp CreateConv creator "group"

  bobClient1 <- assertOne . toList $ pClients bob
  (commit, welcome) <-
    liftIO $
      setupCommit tmp creator "group" "group" $
        NonEmpty.tail (pClients creator) <> [bobClient1]
  testSuccessfulCommit MessagingSetup {users = [bob], ..}

  liftIO $ mergeWelcome tmp (fst bobClient1) "group" "group" "welcome"

  bobClient2Qid <-
    userClientQid (pUserId bob)
      <$> withLastPrekeys (setupUserClient tmp CreateWithKey True (pUserId bob))
  externalProposal <- liftIO $ createExternalProposal tmp bobClient2Qid "group" "group"
  postMessage (qUnqualified (pUserId bob)) externalProposal !!! const 201 === statusCode

testExternalAddProposalWrongUser :: TestM ()
testExternalAddProposalWrongUser = withSystemTempDirectory "mls" $ \tmp -> do
  (creator, [bob, charly]) <- withLastPrekeys $ setupParticipants tmp def [(1, LocalUser), (1, LocalUser)]
  (groupId, conversation) <- setupGroup tmp CreateConv creator "group"

  bobClient1 <- assertOne . toList $ pClients bob
  charlyClient1 <- assertOne . toList $ pClients charly
  (commit, welcome) <-
    liftIO $
      setupCommit tmp creator "group" "group" $
        NonEmpty.tail (pClients creator) <> [bobClient1, charlyClient1]
  testSuccessfulCommit MessagingSetup {users = [bob, charly], ..}

  liftIO $ mergeWelcome tmp (fst bobClient1) "group" "group" "welcome"

  bobClient2Qid <-
    userClientQid (pUserId bob)
      <$> withLastPrekeys (setupUserClient tmp CreateWithKey True (pUserId bob))
  externalProposal <- liftIO $ createExternalProposal tmp bobClient2Qid "group" "group"
  postMessage (qUnqualified (pUserId charly)) externalProposal !!! do
    const 422 === statusCode
    const (Just "mls-unsupported-proposal") === fmap Wai.label . responseJsonError

testExternalAddProposalWrongClient :: TestM ()
testExternalAddProposalWrongClient = withSystemTempDirectory "mls" $ \tmp -> do
  (creator, [bob, charly]) <- withLastPrekeys $ setupParticipants tmp def [(1, LocalUser), (1, LocalUser)]
  (groupId, conversation) <- setupGroup tmp CreateConv creator "group"

  bobClient1 <- assertOne . toList $ pClients bob
  charlyClient1 <- assertOne . toList $ pClients charly
  (commit, welcome) <-
    liftIO $
      setupCommit tmp creator "group" "group" $
        NonEmpty.tail (pClients creator) <> [bobClient1, charlyClient1]
  testSuccessfulCommit MessagingSetup {users = [bob, charly], ..}

  liftIO $ mergeWelcome tmp (fst bobClient1) "group" "group" "welcome"

  bobClient2Qid <-
    userClientQid (pUserId bob)
      <$> withLastPrekeys (setupUserClient tmp CreateWithoutKey True (pUserId bob))
  externalProposal <- liftIO $ createExternalProposal tmp bobClient2Qid "group" "group"
  postMessage (qUnqualified (pUserId charly)) externalProposal !!! do
    const 422 === statusCode
    const (Just "mls-unsupported-proposal") === fmap Wai.label . responseJsonError

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
