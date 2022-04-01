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
import Control.Lens (view)
import Data.Bifunctor
import qualified Data.ByteString as BS
import Data.ByteString.Conversion
import Data.Default
import Data.Domain
import Data.Id
import Data.Json.Util hiding ((#))
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
    [ test s "local welcome" testLocalWelcome,
      test s "local welcome (client with no public key)" testWelcomeNoKey,
      test s "local welcome (client with no public key)" testWelcomeUnknownClient,
      test s "add user to a conversation" testAddUser,
      test s "add user (not connected)" testAddUserNotConnected
    ]

testLocalWelcome :: TestM ()
testLocalWelcome = do
  MessagingSetup {..} <- aliceInvitesBob def
  let (bob, _) = users !! 0

  galley <- viewGalley
  cannon <- view tsCannon

  WS.bracketR cannon (qUnqualified bob) $ \wsB -> do
    -- send welcome message
    post
      ( galley
          . paths ["mls", "welcome"]
          . zUser (qUnqualified (fst creator))
          . zConn "conn"
          . content "message/mls"
          . bytes welcome
      )
      !!! const 201 === statusCode

    -- check that the corresponding event is received
    void . liftIO $
      WS.assertMatch (5 # WS.Second) wsB $
        wsAssertMLSWelcome bob welcome

testWelcomeNoKey :: TestM ()
testWelcomeNoKey = do
  MessagingSetup {..} <- aliceInvitesBob def {createClients = CreateWithoutKey}

  galley <- viewGalley
  post
    ( galley
        . paths ["mls", "welcome"]
        . zUser (qUnqualified (fst creator))
        . content "message/mls"
        . bytes welcome
    )
    !!! const 400 === statusCode

testWelcomeUnknownClient :: TestM ()
testWelcomeUnknownClient = do
  MessagingSetup {..} <- aliceInvitesBob def {createClients = DontCreate}

  galley <- viewGalley
  post
    ( galley
        . paths ["mls", "welcome"]
        . zUser (qUnqualified (fst creator))
        . content "message/mls"
        . bytes welcome
    )
    !!! const 400 === statusCode

testSuccessfulCommit :: MessagingSetup -> TestM ()
testSuccessfulCommit MessagingSetup {..} = do
  let (bob, _) = users !! 0
  cannon <- view tsCannon

  WS.bracketR cannon (qUnqualified bob) $ \wsB -> do
    -- send commit message
    galley <- viewGalley
    events <-
      responseJsonError
        =<< post
          ( galley . paths ["mls", "message"]
              . zUser (qUnqualified (fst creator))
              . zConn "conn"
              . content "message/mls"
              . bytes commit
          )
        <!! const 201 === statusCode

    void . liftIO $ do
      -- check that alice receives join event
      case events of
        [e] -> assertJoinEvent conversation (fst creator) [bob] roleNameWireMember e
        [] -> assertFailure "expected join event to be returned to alice"
        es -> assertFailure $ "expected one event, found: " <> show es

      -- check that bob receives join event
      WS.assertMatch (5 # WS.Second) wsB $
        wsAssertMemberJoinWithRole conversation (fst creator) [bob] roleNameWireMember

  -- FUTUREWORK: check that messages sent to the conversation are not propagated to bob
  pure ()

testAddUser :: TestM ()
testAddUser = do
  setup <- aliceInvitesBob def {createConv = True}
  testSuccessfulCommit setup

testAddUserNotConnected :: TestM ()
testAddUserNotConnected = do
  setup@MessagingSetup {..} <- aliceInvitesBob def {createConv = True, makeConnections = False}
  let (bob, _) = users !! 0

  galley <- viewGalley
  cannon <- view tsCannon

  -- try to add unconnected user
  WS.bracketR cannon (qUnqualified bob) $ \wsB -> do
    err <-
      responseJsonError
        =<< post
          ( galley . paths ["mls", "message"]
              . zUser (qUnqualified (fst creator))
              . zConn "conn"
              . content "message/mls"
              . bytes commit
          )
        <!! do const 403 === statusCode
    liftIO $ Wai.label err @?= "not-connected"

    -- check that bob does not receive any events
    void . liftIO $ WS.assertNoEvent (1 # WS.Second) [wsB]

  -- now connect and retry
  connectUsers (qUnqualified (fst creator)) (pure (qUnqualified bob))
  testSuccessfulCommit setup

--------------------------------------------------------------------------------
-- Messaging setup

data CreateClients = CreateWithoutKey | CreateWithKey | DontCreate

data SetupOptions = SetupOptions
  { createClients :: CreateClients,
    createConv :: Bool,
    makeConnections :: Bool
  }

instance Default SetupOptions where
  def = SetupOptions {createClients = CreateWithKey, createConv = False, makeConnections = True}

data MessagingSetup = MessagingSetup
  { creator :: (Qualified UserId, ClientId),
    users :: [(Qualified UserId, ClientId)],
    conversation :: Qualified ConvId,
    welcome :: ByteString,
    commit :: ByteString
  }

cli :: FilePath -> [String] -> CreateProcess
cli tmp args =
  proc "crypto-cli" $
    ["--store", tmp </> "store.db", "--enc-key", "test"] <> args

setupUserClient :: FilePath -> CreateClients -> Qualified UserId -> LastPrekey -> TestM (String, ClientId)
setupUserClient tmp doCreateClients usr lpk = do
  -- create client if requested
  c <- case doCreateClients of
    DontCreate -> randomClient (qUnqualified usr) lpk
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

-- | Setup: Alice creates a group and invites bob. Return welcome and commit message.
aliceInvitesBob :: SetupOptions -> TestM MessagingSetup
aliceInvitesBob SetupOptions {..} = withSystemTempDirectory "mls" $ \tmp -> do
  alice <- randomQualifiedUser
  let aliceLPK = someLastPrekeys !! 0
  bob <- randomQualifiedUser
  let bobLPK = someLastPrekeys !! 1

  when makeConnections $
    connectUsers (qUnqualified alice) (pure (qUnqualified bob))

  (aliceClientId, aliceClient) <- setupUserClient tmp DontCreate alice aliceLPK
  (bobClientId, bobClient) <- setupUserClient tmp createClients bob bobLPK

  -- create a group

  (groupId, conversation) <-
    first (toBase64Text . unGroupId)
      <$> if createConv
        then do
          conv <-
            responseJsonError =<< postConvQualified (qUnqualified alice) defNewMLSConv
              <!! const 201 === statusCode
          liftIO $ case cnvProtocol conv of
            ProtocolMLS mlsData -> pure (cnvmlsGroupId mlsData, cnvQualifiedId conv)
            p -> assertFailure $ "Expected MLS conversation, got protocol: " <> show (protocolTag p)
        else pure ("test_group", error "No conversation created")

  groupJSON <-
    liftIO $
      spawn (cli tmp ["group", aliceClientId, T.unpack groupId]) Nothing
  liftIO $ BS.writeFile (tmp </> "group") groupJSON

  -- add bob to it and get welcome message
  commit <-
    liftIO $
      spawn
        (cli tmp ["member", "add", "--group", tmp </> "group", "--welcome-out", tmp </> "welcome", tmp </> bobClientId])
        Nothing
  welcome <- liftIO $ BS.readFile (tmp </> "welcome")

  pure $ MessagingSetup {creator = (alice, aliceClient), users = [(bob, bobClient)], ..}

addClient :: Qualified UserId -> LastPrekey -> TestM ClientId
addClient u lpk = do
  let new = newClient PermanentClientType lpk

  brig <- view tsBrig
  c <-
    responseJsonError
      =<< post
        ( brig
            . paths ["clients"]
            . zUser (qUnqualified u)
            . zConn "conn"
            . json new
        )
      <!! const 201 === statusCode

  pure (clientId c)

addKeyPackage :: Qualified UserId -> ClientId -> RawMLS KeyPackage -> TestM ()
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
