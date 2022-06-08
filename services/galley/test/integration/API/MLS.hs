{-# LANGUAGE RecordWildCards #-}

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
import Control.Lens (view)
import qualified Data.Aeson as Aeson
import Data.Default
import Data.Domain
import Data.Id
import Data.Json.Util hiding ((#))
import qualified Data.List.NonEmpty as NonEmpty
import Data.List1 hiding (head)
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import Data.String.Conversions
import qualified Data.Text as T
import Federator.MockServer
import Imports
import qualified Network.Wai.Utilities.Error as Wai
import System.FilePath
import System.IO.Temp
import Test.Tasty
import Test.Tasty.Cannon ((#))
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Wire.API.Conversation
import Wire.API.Conversation.Action
import Wire.API.Conversation.Role
import Wire.API.Event.Conversation
import Wire.API.Federation.API.Common
import Wire.API.Federation.API.Galley
import Wire.API.Message

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "MLS"
    [ testGroup
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
          test s "add user with some non-MLS clients" testAddUserWithProteusClients,
          test s "add new client of an already-present user to a conversation" testAddNewClient,
          test s "send a stale commit" testStaleCommit,
          test s "add remote user to a conversation" testAddRemoteUser
        ],
      testGroup
        "Application Message"
        [ test s "send application message" testAppMessage,
          test s "send remote application message" testRemoteAppMessage
        ],
      testGroup
        "Protocol mismatch"
        [ test s "send a commit to a proteus conversation" testAddUsersToProteus,
          test s "add users bypassing MLS" testAddUsersDirectly,
          test s "remove users bypassing MLS" testRemoveUsersDirectly,
          test s "send proteus message to an MLS conversation" testProteusMessage
        ]
    ]

postMLSConvFail :: TestM ()
postMLSConvFail = do
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice
  bob <- randomUser
  connectUsers alice (list1 bob [])
  postConvQualified alice defNewMLSConv {newConvQualifiedUsers = [Qualified bob (qDomain qalice)]}
    !!! do
      const 400 === statusCode
      const (Just "non-empty-member-list") === fmap Wai.label . responseJsonError

postMLSConvOk :: TestM ()
postMLSConvOk = do
  c <- view tsCannon
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice
  let nameMaxSize = T.replicate 256 "a"
  WS.bracketR c alice $ \wsA -> do
    rsp <- postConvQualified alice defNewMLSConv {newConvName = checked nameMaxSize}
    pure rsp !!! do
      const 201 === statusCode
      const Nothing === fmap Wai.label . responseJsonError
    cid <- assertConv rsp RegularConv alice qalice [] (Just nameMaxSize) Nothing
    checkConvCreateEvent cid wsA

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
    conversation <- setupGroup tmp CreateConv alice "group"
    (commit, welcome) <- liftIO $ setupCommit tmp "group" "group" (pClients bob)

    pure MessagingSetup {creator = alice, ..}

  testSuccessfulCommit setup

testAddUserPartial :: TestM ()
testAddUserPartial = do
  (creator, commit) <- withSystemTempDirectory "mls" $ \tmp -> do
    -- Bob has 3 clients, Charlie has 2
    (alice, [bob, charlie]) <- withLastPrekeys $ setupParticipants tmp def ((,LocalUser) <$> [3, 2])
    void $ setupGroup tmp CreateConv alice "group"
    (commit, _) <-
      liftIO . setupCommit tmp "group" "group" $
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

testAddNewClient :: TestM ()
testAddNewClient = do
  withSystemTempDirectory "mls" $ \tmp -> withLastPrekeys $ do
    -- bob starts with a single client
    (creator, users@[bob]) <- setupParticipants tmp def [(1, LocalUser)]
    conversation <- lift $ setupGroup tmp CreateConv creator "group"

    -- creator sends first commit message
    do
      (commit, welcome) <- liftIO $ setupCommit tmp "group" "group" (pClients bob)
      lift $ testSuccessfulCommit MessagingSetup {..}

    do
      -- then bob adds a new client
      (qcid, c) <- setupUserClient tmp CreateWithKey True (pUserId bob)
      let bobC = (qcid, c)
      -- which gets added to the group
      (commit, welcome) <- liftIO $ setupCommit tmp "group" "group" [bobC]
      -- and the corresponding commit is sent
      lift $ testSuccessfulCommitWithNewUsers MessagingSetup {..} []

testAddUsersToProteus :: TestM ()
testAddUsersToProteus = do
  setup <- aliceInvitesBob (1, LocalUser) def {createConv = CreateProteusConv}
  err <- testFailedCommit setup 404
  liftIO $ Wai.label err @?= "no-conversation"

testAddUsersDirectly :: TestM ()
testAddUsersDirectly = do
  setup@MessagingSetup {..} <- aliceInvitesBob (1, LocalUser) def {createConv = CreateConv}
  void $ postCommit setup
  charlie <- randomUser
  e <-
    responseJsonError
      =<< postMembers
        (qUnqualified (pUserId creator))
        (pure charlie)
        (qUnqualified conversation)
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
  conversation <- setupGroup tmp CreateConv creator "group.0"
  let (users1, users2) = splitAt 1 users

  -- add the first batch of users to the conversation, but do not overwrite group
  do
    (commit, welcome) <-
      liftIO $
        setupCommit tmp "group.0" "group.1" $
          users1 >>= toList . pClients
    testSuccessfulCommit MessagingSetup {users = users1, ..}

  -- now add the rest of the users to the original group state
  do
    (commit, welcome) <-
      liftIO $
        setupCommit tmp "group.0" "group.2" $
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
        "get-mls-clients" ->
          pure
            . Aeson.encode
            . Set.fromList
            . map snd
            . toList
            . pClients
            $ bob
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
  conversation <- setupGroup tmp CreateConv alice "group"
  (commit, welcome) <- liftIO $ setupCommit tmp "group" "group" (pClients bob)
  message <-
    liftIO $
      spawn (cli tmp ["message", "--group", tmp </> "group", "some text"]) Nothing

  let mock req = case frRPC req of
        "on-conversation-updated" -> pure (Aeson.encode ())
        "on-mls-message-sent" -> pure (Aeson.encode EmptyResponse)
        "get-mls-clients" ->
          pure
            . Aeson.encode
            . Set.fromList
            . map snd
            . toList
            . pClients
            $ bob
        ms -> assertFailure ("unmocked endpoint called: " <> cs ms)
  (events :: [Event], reqs) <- withTempMockFederator' mock $ do
    galley <- viewGalley
    void $ postCommit MessagingSetup {creator = alice, users = [bob], ..}
    responseJsonError
      =<< post
        ( galley . paths ["mls", "messages"]
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

testAppMessage :: TestM ()
testAppMessage = withSystemTempDirectory "mls" $ \tmp -> do
  (creator, users) <- withLastPrekeys $ setupParticipants tmp def ((,LocalUser) <$> [1, 2, 3])
  conversation <- setupGroup tmp CreateConv creator "group"

  (commit, welcome) <-
    liftIO $
      setupCommit tmp "group" "group" $
        users >>= toList . pClients

  void $ postCommit MessagingSetup {..}

  message <-
    liftIO $
      spawn (cli tmp ["message", "--group", tmp </> "group", "some text"]) Nothing

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
