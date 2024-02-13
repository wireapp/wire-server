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

module Test.Wire.API.MLS where

import Control.Concurrent.Async
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.ByteArray hiding (length)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Domain
import Data.Id
import Data.Json.Util (toBase64Text)
import Data.Qualified
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.UUID.V4 qualified as UUID
import Imports
import System.Exit
import System.FilePath ((</>))
import System.Process
import System.Random
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO (withSystemTempDirectory)
import Wire.API.MLS.AuthenticatedContent
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Commit
import Wire.API.MLS.Credential
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.HPKEPublicKey
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Message
import Wire.API.MLS.Proposal
import Wire.API.MLS.ProtocolVersion
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Welcome

tests :: TestTree
tests =
  testGroup "MLS" $
    [ testCase "parse key package" testParseKeyPackage,
      testCase "parse capabilities in key package" testParseKeyPackageWithCapabilities,
      testCase "parse commit message" testParseCommit,
      testCase "parse application message" testParseApplication,
      testCase "parse welcome and groupinfo message" testParseWelcomeAndGroupInfo,
      testCase "key package ref" testKeyPackageRef,
      testCase "create signed remove proposal" testRemoveProposalMessageSignature,
      testCase "parse client identity" testParseClientIdentity
    ]

testParseClientIdentity :: IO ()
testParseClientIdentity = do
  let cid = "wireapp://qHiDLsbkT2-p9uSJsmrZ_A%217f39900830740008@wire.com"
  let actual = sanIdentity cid
  show <$> actual @?= Right "a878832e-c6e4-4f6f-a9f6-e489b26ad9fc:7f39900830740008@wire.com"

testParseKeyPackage :: IO ()
testParseKeyPackage = do
  alice <- randomIdentity
  let qcid = B8.unpack (encodeMLS' alice)
  kpData <- withSystemTempDirectory "mls" $ \tmp -> do
    void $ spawn (cli qcid tmp ["init", qcid]) Nothing
    spawn (cli qcid tmp ["key-package", "create"]) Nothing

  kp <- case decodeMLS' @KeyPackage kpData of
    Left err -> assertFailure (T.unpack err)
    Right x -> pure x

  pvTag (kp.protocolVersion) @?= Just ProtocolMLS10
  kp.cipherSuite @?= CipherSuite 1
  BS.length (unHPKEPublicKey kp.initKey) @?= 32

  case keyPackageIdentity kp of
    Left err -> assertFailure $ "Failed to parse identity: " <> T.unpack err
    Right identity -> identity @?= alice

testParseKeyPackageWithCapabilities :: IO ()
testParseKeyPackageWithCapabilities = do
  kpData <- BS.readFile "test/resources/key_package1.mls"
  case decodeMLS' @KeyPackage kpData of
    Left err -> assertFailure (T.unpack err)
    Right _ -> pure ()

testParseCommit :: IO ()
testParseCommit = do
  qcid <- B8.unpack . encodeMLS' <$> randomIdentity
  commitData <- withSystemTempDirectory "mls" $ \tmp -> do
    void $ spawn (cli qcid tmp ["init", qcid]) Nothing
    groupJSON <- spawn (cli qcid tmp ["group", "create", "Zm9v"]) Nothing
    spawn (cli qcid tmp ["commit", "--group", "-"]) (Just groupJSON)

  msg <- case decodeMLS' @Message commitData of
    Left err -> assertFailure (T.unpack err)
    Right x -> pure x

  pvTag (msg.protocolVersion) @?= Just ProtocolMLS10

  pmsg <- case msg.content of
    MessagePublic x -> pure x
    _ -> assertFailure "expected public message"

  pmsg.content.value.sender @?= SenderMember 0

  commit <- case pmsg.content.value.content of
    FramedContentCommit c -> pure c
    _ -> assertFailure "expected commit"

  commit.value.proposals @?= []

testParseApplication :: IO ()
testParseApplication = do
  qcid <- B8.unpack . encodeMLS' <$> randomIdentity
  msgData <- withSystemTempDirectory "mls" $ \tmp -> do
    void $ spawn (cli qcid tmp ["init", qcid]) Nothing
    groupJSON <- spawn (cli qcid tmp ["group", "create", "Zm9v"]) Nothing
    spawn (cli qcid tmp ["message", "--group-in", "-", "hello"]) (Just groupJSON)

  msg <- case decodeMLS' @Message msgData of
    Left err -> assertFailure (T.unpack err)
    Right x -> pure x

  pvTag (msg.protocolVersion) @?= Just ProtocolMLS10

  pmsg <- case msg.content of
    MessagePrivate x -> pure x.value
    _ -> assertFailure "expected private message"

  pmsg.groupId @?= GroupId "foo"
  pmsg.epoch @?= Epoch 0

testParseWelcomeAndGroupInfo :: IO ()
testParseWelcomeAndGroupInfo = do
  qcid <- B8.unpack . encodeMLS' <$> randomIdentity
  qcid2 <- B8.unpack . encodeMLS' <$> randomIdentity
  (welData, giData) <- withSystemTempDirectory "mls" $ \tmp -> do
    void $ spawn (cli qcid tmp ["init", qcid]) Nothing
    void $ spawn (cli qcid2 tmp ["init", qcid2]) Nothing
    groupJSON <- spawn (cli qcid tmp ["group", "create", "Zm9v"]) Nothing
    kp <- spawn (cli qcid2 tmp ["key-package", "create"]) Nothing
    BS.writeFile (tmp </> "kp") kp
    void $
      spawn
        ( cli
            qcid
            tmp
            [ "member",
              "add",
              "--group",
              "-",
              tmp </> "kp",
              "--welcome-out",
              tmp </> "welcome",
              "--group-info-out",
              tmp </> "gi"
            ]
        )
        (Just groupJSON)
    (,)
      <$> BS.readFile (tmp </> "welcome")
      <*> BS.readFile (tmp </> "gi")

  do
    welcomeMsg <- case decodeMLS' @Message welData of
      Left err -> assertFailure (T.unpack err)
      Right x -> pure x

    pvTag (welcomeMsg.protocolVersion) @?= Just ProtocolMLS10

    wel <- case welcomeMsg.content of
      MessageWelcome x -> pure x.value
      _ -> assertFailure "expected welcome message"

    length (wel.welSecrets) @?= 1

  do
    gi <- case decodeMLS' @GroupInfo giData of
      Left err -> assertFailure (T.unpack err)
      Right x -> pure x

    gi.groupContext.groupId @?= GroupId "foo"
    gi.groupContext.epoch @?= Epoch 1

testKeyPackageRef :: IO ()
testKeyPackageRef = do
  let qcid = "b455a431-9db6-4404-86e7-6a3ebe73fcaf:3ae58155@mls.example.com"
  (kpData, ref) <- withSystemTempDirectory "mls" $ \tmp -> do
    void $ spawn (cli qcid tmp ["init", qcid]) Nothing
    kpData <- spawn (cli qcid tmp ["key-package", "create"]) Nothing
    ref <- spawn (cli qcid tmp ["key-package", "ref", "-"]) (Just kpData)
    pure (kpData, KeyPackageRef ref)

  kpRef MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519 (KeyPackageData kpData) @?= ref

testRemoveProposalMessageSignature :: IO ()
testRemoveProposalMessageSignature = withSystemTempDirectory "mls" $ \tmp -> do
  qcid <- do
    let c = ClientId 0x3ae58155
    usr <- flip Qualified (Domain "example.com") <$> (Id <$> UUID.nextRandom)
    pure (userClientQid usr c)
  void $ spawn (cli qcid tmp ["init", qcid]) Nothing

  qcid2 <- do
    let c = ClientId 0x4ae58157
    usr <- flip Qualified (Domain "example.com") <$> (Id <$> UUID.nextRandom)
    pure (userClientQid usr c)
  void $ spawn (cli qcid2 tmp ["init", qcid2]) Nothing
  kp :: RawMLS KeyPackage <-
    decodeMLSError <$> spawn (cli qcid2 tmp ["key-package", "create"]) Nothing
  BS.writeFile (tmp </> qcid2) (raw kp)

  secretKey <- Ed25519.generateSecretKey
  let groupFilename = "group"
      gid = GroupId "abcd"
      signerKeyFilename = "signer-key.bin"
      publicKey = Ed25519.toPublic secretKey
  BS.writeFile (tmp </> signerKeyFilename) (convert publicKey)
  createGroup tmp qcid groupFilename signerKeyFilename gid

  void $ spawn (cli qcid tmp ["member", "add", "--group", tmp </> groupFilename, "--in-place", tmp </> qcid2]) Nothing

  let proposal = mkRawMLS (RemoveProposal 1)
      pmessage =
        mkSignedPublicMessage
          secretKey
          publicKey
          gid
          (Epoch 1)
          (TaggedSenderExternal 0)
          (FramedContentProposal proposal)
      message = mkMessage $ MessagePublic pmessage
      messageFilename = "signed-message.mls"

  BS.writeFile (tmp </> messageFilename) (raw (mkRawMLS message))

  void $
    spawn
      ( cli
          qcid
          tmp
          [ "consume",
            "--group",
            tmp </> groupFilename,
            tmp </> messageFilename
          ]
      )
      Nothing

createGroup :: FilePath -> String -> String -> String -> GroupId -> IO ()
createGroup tmp store groupName removalKey gid = do
  groupJSON <-
    liftIO $
      spawn
        ( cli
            store
            tmp
            [ "group",
              "create",
              "--removal-key",
              tmp </> removalKey,
              T.unpack (toBase64Text (unGroupId gid))
            ]
        )
        Nothing
  liftIO $ BS.writeFile (tmp </> groupName) groupJSON

decodeMLSError :: ParseMLS a => ByteString -> a
decodeMLSError s = case decodeMLS' s of
  Left e -> error ("Could not parse MLS object: " <> Text.unpack e)
  Right x -> x

userClientQid :: Qualified UserId -> ClientId -> String
userClientQid usr c =
  show (qUnqualified usr)
    <> ":"
    <> T.unpack (clientToText c)
    <> "@"
    <> T.unpack (domainText (qDomain usr))

spawn :: HasCallStack => CreateProcess -> Maybe ByteString -> IO ByteString
spawn cp minput = do
  (mout, ex) <- withCreateProcess
    cp
      { std_out = CreatePipe,
        std_in = CreatePipe
      }
    $ \minh mouth _ ph ->
      let writeInput = for_ minh $ \inh -> do
            forM_ minput $ BS.hPutStr inh
            hClose inh
          readOutput = (,) <$> traverse BS.hGetContents mouth <*> waitForProcess ph
       in snd <$> concurrently writeInput readOutput
  case (mout, ex) of
    (Just out, ExitSuccess) -> pure out
    _ -> assertFailure "Failed spawning process"

cli :: String -> FilePath -> [String] -> CreateProcess
cli store tmp args =
  proc "mls-test-cli" $
    ["--store", tmp </> (store <> ".db")] <> args

randomIdentity :: IO ClientIdentity
randomIdentity = do
  uid <- Id <$> UUID.nextRandom
  c <- ClientId <$> randomIO
  pure $ ClientIdentity (Domain "mls.example.com") uid c
