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
import qualified Data.ByteString as BS
import Data.ByteString.Conversion
import Data.Default
import Data.Domain
import Data.Id
import qualified Data.Map as Map
import Data.Qualified
import Data.String.Conversions
import qualified Data.Text as T
import Imports
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
      test s "add user to a conversation" testAddUser
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

testAddUser :: TestM ()
testAddUser = do
  MessagingSetup {..} <- aliceInvitesBob def {createConv = True}

  galley <- viewGalley
  post
    ( galley . paths ["mls", "message"]
        . zUser (qUnqualified (fst creator))
        . zConn "conn"
        . content "message/mls"
        . bytes commit
    )
    !!! const 201 === statusCode

--------------------------------------------------------------------------------
-- Messaging setup

data CreateClients = CreateWithoutKey | CreateWithKey | DontCreate

data SetupOptions = SetupOptions
  { createClients :: CreateClients,
    createConv :: Bool
  }

instance Default SetupOptions where
  def = SetupOptions {createClients = CreateWithKey, createConv = False}

data MessagingSetup = MessagingSetup
  { creator :: (Qualified UserId, ClientId),
    users :: [(Qualified UserId, ClientId)],
    welcome :: ByteString,
    commit :: ByteString
  }

-- | Setup: Alice creates a group and invites bob. Return welcome and commit message.
aliceInvitesBob :: SetupOptions -> TestM MessagingSetup
aliceInvitesBob SetupOptions {..} = withSystemTempDirectory "mls" $ \tmp -> do
  alice <- randomQualifiedUser
  let aliceLPK = someLastPrekeys !! 0
  bob <- randomQualifiedUser
  let bobLPK = someLastPrekeys !! 1

  aliceClient <- randomClient (qUnqualified alice) aliceLPK
  bobClient <- case createClients of
    DontCreate -> randomClient (qUnqualified bob) bobLPK
    _ -> addClient bob bobLPK

  let store = tmp </> "store.db"
      cli args = proc "crypto-cli" $ ["--store", store, "--enc-key", "test"] <> args
      aliceClientId =
        show (qUnqualified alice)
          <> ":"
          <> T.unpack (client aliceClient)
          <> "@"
          <> T.unpack (domainText (qDomain alice))
      bobClientId =
        show (qUnqualified bob)
          <> ":"
          <> T.unpack (client bobClient)
          <> "@"
          <> T.unpack (domainText (qDomain bob))

  -- generate key package for alice
  void . liftIO $ spawn (cli ["key-package", aliceClientId]) Nothing

  -- generate key package for bob
  bobKeyPackage <-
    liftIO $
      decodeMLSError
        =<< spawn (cli ["key-package", bobClientId]) Nothing

  -- set bob's private key and upload key package if required
  case createClients of
    CreateWithKey -> addKeyPackage bob bobClient bobKeyPackage
    _ -> pure ()

  -- create a group

  _groupId <-
    if createConv
      then do
        conv <-
          responseJsonError =<< postConvQualified (qUnqualified alice) defNewMLSConv
            <!! const 201 === statusCode
        liftIO $ case cnvProtocol conv of
          ProtocolMLS mlsData -> pure (cnvmlsGroupId mlsData)
          p -> assertFailure $ "Expected MLS conversation, got protocol: " <> show (protocolTag p)
      else pure "test_group"

  groupJSON <-
    liftIO $
      spawn (cli ["group", aliceClientId, "dGVzdF9ncm91cA=="]) Nothing
  liftIO $ BS.writeFile (tmp </> "group") groupJSON

  -- add bob to it and get welcome message
  commit <-
    liftIO $
      spawn
        (cli ["member", "add", "--group", tmp </> "group", "--welcome-out", tmp </> "welcome", "-"])
        (Just (rmRaw bobKeyPackage))
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
