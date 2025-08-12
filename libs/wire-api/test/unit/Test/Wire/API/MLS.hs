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
import Data.Aeson qualified as A
import Data.ByteArray hiding (all, length)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LBS
import Data.Domain
import Data.Id
import Data.Json.Util (toBase64Text)
import Data.Qualified
import Data.Set qualified as Set
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
      testCase "parse client identity" testParseClientIdentity,
      testCase "inspect key packages" testInspectKeyPackage
    ]

testInspectKeyPackage :: IO ()
testInspectKeyPackage = do
  let keyPackageUpload = A.decode keyPackageUploadJson
      actual = Set.fromList . map (fromJust . cipherSuiteTag . (.cipherSuite) . value) . keyPackages $ fromJust keyPackageUpload
      expected = Set.singleton . fromJust $ cipherSuiteTag (CipherSuite 2)
  liftIO $ actual @=? expected
  where
    keyPackageUploadJson :: LBS.ByteString
    keyPackageUploadJson =
      "{ \"key_packages\": [\
      \    \"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\", \
      \    \"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\", \
      \    \"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\", \
      \    \"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\", \
      \    \"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\" \
      \  ] \
      \}"

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
  pmessage <-
    mkSignedPublicMessage
      @Ed25519
      (secretKey, publicKey)
      gid
      (Epoch 1)
      (TaggedSenderExternal 0)
      (FramedContentProposal proposal)
  let message = mkMessage $ MessagePublic pmessage
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

decodeMLSError :: (ParseMLS a) => ByteString -> a
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

spawn :: (HasCallStack) => CreateProcess -> Maybe ByteString -> IO ByteString
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
    _ -> assertFailure $ "Failed spawning process\n" <> show mout <> "\n" <> show ex

cli :: String -> FilePath -> [String] -> CreateProcess
cli store tmp args =
  proc "mls-test-cli" $
    ["--store", tmp </> (store <> ".db")] <> args

randomIdentity :: IO ClientIdentity
randomIdentity = do
  uid <- Id <$> UUID.nextRandom
  c <- ClientId <$> randomIO
  pure $ ClientIdentity (Domain "mls.example.com") uid c
