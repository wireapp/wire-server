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

import API.Util
import Bilge
import Bilge.Assert
import Control.Lens (preview, to, view)
import qualified Control.Monad.State as State
import qualified Data.ByteString as BS
import Data.ByteString.Conversion
import Data.Default
import Data.Domain
import Data.Id
import Data.Json.Util hiding ((#))
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List1
import qualified Data.Map as Map
import Data.Qualified
import Data.String.Conversions
import qualified Data.Text as T
import Imports
import qualified Network.Wai.Utilities.Error as Wai
import System.FilePath
import System.IO.Temp
import System.Process
import Test.Tasty
import Test.Tasty.Cannon ((#))
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Wire.API.Conversation
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Serialisation
import Wire.API.User.Client
import Wire.API.User.Client.Prekey

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "MLS"
    [ testGroup
        "Welcome"
        [ test s "local welcome" testLocalWelcome,
          test s "local welcome (client with no public key)" testWelcomeNoKey,
          test s "local welcome (client with no public key)" testWelcomeUnknownClient
        ],
      testGroup
        "Commit"
        [ test s "add user to a conversation" testAddUser,
          test s "add user (not connected)" testAddUserNotConnected,
          test s "add user (partial client list)" testAddUserPartial,
          test s "add new client of an already-present user to a conversation" testAddNewClient,
          test s "send a commit to a proteus conversation" testAddUsersToProteus,
          test s "send a stale commit" testStaleCommit
        ]
    ]

testLocalWelcome :: TestM ()
testLocalWelcome = do
  MessagingSetup {..} <- aliceInvitesBob 1 def
  let bob = users !! 0

  galley <- viewGalley
  cannon <- view tsCannon

  WS.bracketR cannon (qUnqualified (pUserId bob)) $ \wsB -> do
    -- send welcome message
    post
      ( galley
          . paths ["mls", "welcome"]
          . zUser (qUnqualified (pUserId creator))
          . zConn "conn"
          . content "message/mls"
          . bytes welcome
      )
      !!! const 201 === statusCode

    -- check that the corresponding event is received
    void . liftIO $
      WS.assertMatch (5 # WS.Second) wsB $
        wsAssertMLSWelcome (pUserId bob) welcome

testWelcomeNoKey :: TestM ()
testWelcomeNoKey = do
  MessagingSetup {..} <- aliceInvitesBob 1 def {createClients = CreateWithoutKey}

  galley <- viewGalley
  post
    ( galley
        . paths ["mls", "welcome"]
        . zUser (qUnqualified (pUserId creator))
        . content "message/mls"
        . bytes welcome
    )
    !!! const 400 === statusCode

testWelcomeUnknownClient :: TestM ()
testWelcomeUnknownClient = do
  MessagingSetup {..} <- aliceInvitesBob 1 def {createClients = DontCreateClients}

  galley <- viewGalley
  post
    ( galley
        . paths ["mls", "welcome"]
        . zUser (qUnqualified (pUserId creator))
        . content "message/mls"
        . bytes welcome
    )
    !!! const 400 === statusCode

-- | Send a commit message, and assert that all participants see an event with
-- the given list of new members.
testSuccessfulCommitWithNewUsers :: HasCallStack => MessagingSetup -> [Qualified UserId] -> TestM ()
testSuccessfulCommitWithNewUsers MessagingSetup {..} newUsers = do
  cannon <- view tsCannon

  WS.bracketRN cannon (map (qUnqualified . pUserId) users) $ \wss -> do
    -- send commit message
    galley <- viewGalley
    events <-
      responseJsonError
        =<< post
          ( galley . paths ["mls", "messages"]
              . zUser (qUnqualified (pUserId creator))
              . zConn "conn"
              . content "message/mls"
              . bytes commit
          )
        <!! const 201 === statusCode

    liftIO $
      if null newUsers
        then do
          -- check that alice receives no events
          assertBool ("expected no events, received " <> show events) (null events)

          -- check that no users receive join events
          WS.assertNoEvent (1 # WS.Second) wss
        else do
          -- check that alice receives a join event
          case (events, newUsers) of
            ([], []) -> pure () -- no users added, no event received
            (es, []) -> assertFailure $ "expected no events, received " <> show es
            ([e], _) -> assertJoinEvent conversation (pUserId creator) newUsers roleNameWireMember e
            ([], _) -> assertFailure "expected join event to be returned to alice"
            (es, _) -> assertFailure $ "expected one event, found: " <> show es

          -- check that all users receive a join event
          for_ wss $ \ws -> do
            WS.assertMatch (5 # WS.Second) ws $
              wsAssertMemberJoinWithRole conversation (pUserId creator) newUsers roleNameWireMember

  -- FUTUREWORK: check that messages sent to the conversation are propagated to bob
  pure ()

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
  setup <- aliceInvitesBob 1 def {createConv = CreateConv}
  testSuccessfulCommit setup

testAddUserNotConnected :: TestM ()
testAddUserNotConnected = do
  setup@MessagingSetup {..} <- aliceInvitesBob 1 def {createConv = CreateConv, makeConnections = False}
  let bob = users !! 0

  -- try to add unconnected user
  err <- testFailedCommit setup 403
  liftIO $ Wai.label err @?= "not-connected"

  -- now connect and retry
  connectUsers (qUnqualified (pUserId creator)) (pure (qUnqualified (pUserId bob)))
  testSuccessfulCommit setup

testAddUserPartial :: TestM ()
testAddUserPartial = do
  (creator, commit) <- withSystemTempDirectory "mls" $ \tmp -> do
    -- Bob has 3 clients, Charlie has 2
    (alice, [bob, charlie]) <- withLastPrekeys $ setupParticipants tmp def [3, 2]
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
    (creator, users@[bob]) <- setupParticipants tmp def [1]
    conversation <- lift $ setupGroup tmp CreateConv creator "group"

    -- creator sends first commit message
    do
      (commit, welcome) <- liftIO $ setupCommit tmp "group" "group" (pClients bob)
      lift $ testSuccessfulCommit MessagingSetup {..}

    do
      -- then bob adds a new client
      bobC <- setupUserClient tmp CreateWithKey (pUserId bob)
      -- which gets added to the group
      (commit, welcome) <- liftIO $ setupCommit tmp "group" "group" [bobC]
      -- and the corresponding commit is sent
      lift $ testSuccessfulCommitWithNewUsers MessagingSetup {..} []

testAddUsersToProteus :: TestM ()
testAddUsersToProteus = do
  setup <- aliceInvitesBob 1 def {createConv = CreateProteusConv}
  err <- testFailedCommit setup 404
  liftIO $ Wai.label err @?= "no-conversation"

testStaleCommit :: TestM ()
testStaleCommit = withSystemTempDirectory "mls" $ \tmp -> do
  (creator, users) <- withLastPrekeys $ setupParticipants tmp def [2, 3]
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

--------------------------------------------------------------------------------
-- Messaging setup

data CreateClients = CreateWithoutKey | CreateWithKey | DontCreateClients
  deriving (Eq)

data CreateConv = CreateConv | CreateProteusConv | DontCreateConv
  deriving (Eq)

createNewConv :: CreateConv -> Maybe NewConv
createNewConv CreateConv = Just defNewMLSConv
createNewConv CreateProteusConv = Just defNewProteusConv
createNewConv DontCreateConv = Nothing

data SetupOptions = SetupOptions
  { createClients :: CreateClients,
    createConv :: CreateConv,
    makeConnections :: Bool
  }

instance Default SetupOptions where
  def =
    SetupOptions
      { createClients = CreateWithKey,
        createConv = DontCreateConv,
        makeConnections = True
      }

data MessagingSetup = MessagingSetup
  { creator :: Participant,
    users :: [Participant],
    conversation :: Qualified ConvId,
    welcome :: ByteString,
    commit :: ByteString
  }

data Participant = Participant
  { pUserId :: Qualified UserId,
    pClients :: NonEmpty (String, ClientId)
  }
  deriving (Show)

cli :: FilePath -> [String] -> CreateProcess
cli tmp args =
  proc "crypto-cli" $
    ["--store", tmp </> "store.db", "--enc-key", "test"] <> args

pClientQid :: Participant -> String
pClientQid = fst . NonEmpty.head . pClients

setupUserClient ::
  HasCallStack =>
  FilePath ->
  CreateClients ->
  Qualified UserId ->
  State.StateT [LastPrekey] TestM (String, ClientId)
setupUserClient tmp doCreateClients usr = do
  lpk <- takeLastPrekey
  lift $ do
    -- create client if requested
    c <- case doCreateClients of
      DontCreateClients -> randomClient (qUnqualified usr) lpk
      _ -> addClient usr lpk

    let qcid =
          show (qUnqualified usr)
            <> ":"
            <> T.unpack (client c)
            <> "@"
            <> T.unpack (domainText (qDomain usr))

    -- generate key package
    kp <-
      liftIO $
        decodeMLSError
          =<< spawn (cli tmp ["key-package", qcid]) Nothing
    liftIO $ BS.writeFile (tmp </> qcid) (rmRaw kp)

    -- set bob's private key and upload key package if required
    case doCreateClients of
      CreateWithKey -> addKeyPackage usr c kp
      _ -> pure ()

    pure (qcid, c)

setupParticipant ::
  HasCallStack =>
  FilePath ->
  CreateClients ->
  Int ->
  Qualified UserId ->
  State.StateT [LastPrekey] TestM Participant
setupParticipant tmp doCreateClients numClients usr =
  Participant usr . NonEmpty.fromList
    <$> replicateM numClients (setupUserClient tmp doCreateClients usr)

setupParticipants ::
  HasCallStack =>
  FilePath ->
  SetupOptions ->
  [Int] ->
  State.StateT [LastPrekey] TestM (Participant, [Participant])
setupParticipants tmp SetupOptions {..} ns = do
  creator <- lift randomQualifiedUser >>= setupParticipant tmp DontCreateClients 1
  others <- for ns $ \n ->
    lift randomQualifiedUser >>= setupParticipant tmp createClients n
  lift . when makeConnections $
    traverse_
      ( connectUsers (qUnqualified (pUserId creator))
          . List1
          . fmap (qUnqualified . pUserId)
      )
      (nonEmpty others)
  pure (creator, others)

withLastPrekeys :: Monad m => State.StateT [LastPrekey] m a -> m a
withLastPrekeys m = State.evalStateT m someLastPrekeys

setupGroup :: HasCallStack => FilePath -> CreateConv -> Participant -> String -> TestM (Qualified ConvId)
setupGroup tmp createConv creator name = do
  (mGroupId, conversation) <- case createNewConv createConv of
    Nothing -> pure (Nothing, error "No conversation created")
    Just nc -> do
      conv <-
        responseJsonError =<< postConvQualified (qUnqualified (pUserId creator)) nc
          <!! const 201 === statusCode

      pure (preview (to cnvProtocol . _ProtocolMLS . to cnvmlsGroupId) conv, cnvQualifiedId conv)

  let groupId = toBase64Text (maybe "test_group" unGroupId mGroupId)
  groupJSON <-
    liftIO $
      spawn (cli tmp ["group", pClientQid creator, T.unpack groupId]) Nothing
  liftIO $ BS.writeFile (tmp </> name) groupJSON

  pure conversation

setupCommit ::
  (HasCallStack, Foldable f) =>
  String ->
  String ->
  String ->
  f (String, ClientId) ->
  IO (ByteString, ByteString)
setupCommit tmp groupName newGroupName clients =
  (,)
    <$> spawn
      ( cli
          tmp
          $ [ "member",
              "add",
              "--group",
              tmp </> groupName,
              "--welcome-out",
              tmp </> "welcome",
              "--group-out",
              tmp </> newGroupName
            ]
            <> map ((tmp </>) . fst) (toList clients)
      )
      Nothing
      <*> BS.readFile (tmp </> "welcome")

takeLastPrekey :: MonadFail m => State.StateT [LastPrekey] m LastPrekey
takeLastPrekey = do
  (lpk : lpks) <- State.get
  State.put lpks
  pure lpk

-- | Setup: Alice creates a group and invites bob. Return welcome and commit message.
aliceInvitesBob :: HasCallStack => Int -> SetupOptions -> TestM MessagingSetup
aliceInvitesBob numBobClients opts@SetupOptions {..} = withSystemTempDirectory "mls" $ \tmp -> do
  (alice, [bob]) <- withLastPrekeys $ setupParticipants tmp opts [numBobClients]

  -- create a group
  conversation <- setupGroup tmp createConv alice "group"

  -- add bob to it and get welcome message
  (commit, welcome) <- liftIO $ setupCommit tmp "group" "group" (pClients bob)

  pure $
    MessagingSetup
      { creator = alice,
        users = [bob],
        ..
      }

addClient :: HasCallStack => Qualified UserId -> LastPrekey -> TestM ClientId
addClient u lpk = do
  let new = newClient PermanentClientType lpk

  brig <- view tsBrig
  c <-
    responseJsonError
      =<< post
        ( brig
            . paths ["i", "clients", toByteString' (qUnqualified u)]
            . zConn "conn"
            . queryItem "skip_reauth" "true"
            . json new
        )
      <!! const 201 === statusCode

  pure (clientId c)

addKeyPackage :: HasCallStack => Qualified UserId -> ClientId -> RawMLS KeyPackage -> TestM ()
addKeyPackage u c kp = do
  let update = defUpdateClient {updateClientMLSPublicKeys = Map.singleton Ed25519 (bcSignatureKey (kpCredential (rmValue kp)))}
  -- set public key
  brig <- view tsBrig
  put
    ( brig
        . paths ["clients", toByteString' c]
        . zUser (qUnqualified u)
        . json update
    )
    !!! const 200 === statusCode

  -- upload key package
  post
    ( brig
        . paths ["mls", "key-packages", "self", toByteString' c]
        . zUser (qUnqualified u)
        . json (KeyPackageUpload [kp])
    )
    !!! const 201 === statusCode

  -- claim key package (technically, some other user should claim them, but it doesn't really make a difference)
  bundle <-
    responseJsonError
      =<< post
        ( brig
            . paths ["mls", "key-packages", "claim", toByteString' (qDomain u), toByteString' (qUnqualified u)]
            . zUser (qUnqualified u)
        )
      <!! const 200 === statusCode
  liftIO $ map (Just . kpbeRef) (toList (kpbEntries bundle)) @?= [kpRef' kp]
