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
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.ByteArray
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Domain
import Data.Either.Combinators
import Data.Hex
import Data.Id
import Data.Json.Util (toBase64Text)
import Data.Qualified
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Imports
import System.Exit
import System.FilePath ((</>))
import System.Process
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO (withSystemTempDirectory)
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Commit
import Wire.API.MLS.Credential
import Wire.API.MLS.Epoch
import Wire.API.MLS.Extension
import Wire.API.MLS.Group
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Message
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Welcome

tests :: TestTree
tests =
  testGroup "MLS" $
    [ testCase "parse key package" testParseKeyPackage,
      testCase "parse commit message" testParseCommit,
      testCase "parse application message" testParseApplication,
      testCase "parse welcome message" testParseWelcome,
      testCase "key package ref" testKeyPackageRef,
      testCase "validate message signature" testVerifyMLSPlainTextWithKey,
      testCase "create signed remove proposal" testRemoveProposalMessageSignature
    ]

testParseKeyPackage :: IO ()
testParseKeyPackage = do
  kpData <- BS.readFile "test/resources/key_package1.mls"
  kp <- case decodeMLS' @KeyPackage kpData of
    Left err -> assertFailure (T.unpack err)
    Right x -> pure x

  pvTag (kpProtocolVersion kp) @?= Just ProtocolMLS10
  kpCipherSuite kp @?= CipherSuite 1
  BS.length (kpInitKey kp) @?= 32

  case decodeMLS' @ClientIdentity (bcIdentity (kpCredential kp)) of
    Left err -> assertFailure $ "Failed to parse identity: " <> T.unpack err
    Right identity ->
      identity
        @?= ClientIdentity
          { ciDomain = Domain "mls.example.com",
            ciUser = Id (fromJust (UUID.fromString "b455a431-9db6-4404-86e7-6a3ebe73fcaf")),
            ciClient = newClientId 0x3ae58155
          }

  -- check raw TBS package
  let rawTBS = rmRaw (kpTBS kp)
  rawTBS @?= BS.take 196 kpData

testParseCommit :: IO ()
testParseCommit = do
  msgData <- LBS.readFile "test/resources/commit1.mls"
  msg :: Message 'MLSPlainText <- case decodeMLS @SomeMessage msgData of
    Left err -> assertFailure (T.unpack err)
    Right (SomeMessage SMLSCipherText _) ->
      assertFailure "Expected plain text message, found encrypted"
    Right (SomeMessage SMLSPlainText msg) ->
      pure msg

  msgGroupId msg @?= "test_group"
  msgEpoch msg @?= Epoch 0

  case msgSender msg of
    MemberSender kp -> kp @?= KeyPackageRef (fromRight' (unhex "24e4b0a802a2b81f00a9af7df5e91da8"))
    _ -> assertFailure "Unexpected sender type"

  let payload = msgPayload msg
  commit <- case payload of
    CommitMessage c -> pure c
    _ -> assertFailure "Unexpected message type"

  case cProposals commit of
    [Inline (AddProposal _)] -> pure ()
    _ -> assertFailure "Unexpected proposals"

testParseApplication :: IO ()
testParseApplication = do
  msgData <- LBS.readFile "test/resources/app_message1.mls"
  msg :: Message 'MLSCipherText <- case decodeMLS @SomeMessage msgData of
    Left err -> assertFailure (T.unpack err)
    Right (SomeMessage SMLSCipherText msg) -> pure msg
    Right (SomeMessage SMLSPlainText _) ->
      assertFailure "Expected encrypted message, found plain text"

  msgGroupId msg @?= "test_group"
  msgEpoch msg @?= Epoch 0
  msgContentType (msgPayload msg) @?= fromMLSEnum ApplicationMessageTag

testParseWelcome :: IO ()
testParseWelcome = do
  welData <- LBS.readFile "test/resources/welcome1.mls"
  wel <- case decodeMLS welData of
    Left err -> assertFailure (T.unpack err)
    Right x -> pure x

  welCipherSuite wel @?= CipherSuite 1
  map gsNewMember (welSecrets wel) @?= [KeyPackageRef (fromRight' (unhex "ab4692703ca6d50ffdeaae3096f885c2"))]

testKeyPackageRef :: IO ()
testKeyPackageRef = do
  kpData <- BS.readFile "test/resources/key_package1.mls"
  ref <- KeyPackageRef <$> BS.readFile "test/resources/key_package_ref1"
  kpRef MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519 (KeyPackageData kpData) @?= ref

testVerifyMLSPlainTextWithKey :: IO ()
testVerifyMLSPlainTextWithKey = do
  -- this file was created with openmls from the client that is in the add proposal
  msgData <- BS.readFile "test/resources/external_proposal.mls"

  msg :: Message 'MLSPlainText <- case decodeMLS' @SomeMessage msgData of
    Left err -> assertFailure (T.unpack err)
    Right (SomeMessage SMLSCipherText _) ->
      assertFailure "Expected SomeMessage SMLSCipherText"
    Right (SomeMessage SMLSPlainText msg) ->
      pure msg

  kp <- case msgPayload msg of
    ProposalMessage prop ->
      case rmValue prop of
        AddProposal kp -> pure kp
        _ -> error "Expected AddProposal"
    _ -> error "Expected ProposalMessage"

  let pubkey = bcSignatureKey . kpCredential . rmValue $ kp
  liftIO $
    assertBool
      "message signature verification failed"
      $ verifyMessageSignature MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519 msg pubkey

testRemoveProposalMessageSignature :: IO ()
testRemoveProposalMessageSignature = withSystemTempDirectory "mls" $ \tmp -> do
  qcid <- do
    let c = newClientId 0x3ae58155
    usr <- flip Qualified (Domain "example.com") <$> (Id <$> UUID.nextRandom)
    pure (userClientQid usr c)
  void . liftIO $ spawn (cli qcid tmp ["init", qcid]) Nothing

  qcid2 <- do
    let c = newClientId 0x4ae58157
    usr <- flip Qualified (Domain "example.com") <$> (Id <$> UUID.nextRandom)
    pure (userClientQid usr c)
  void . liftIO $ spawn (cli qcid2 tmp ["init", qcid2]) Nothing
  kp <- liftIO $ decodeMLSError <$> spawn (cli qcid2 tmp ["key-package", "create"]) Nothing
  liftIO $ BS.writeFile (tmp </> qcid2) (rmRaw kp)

  let groupFilename = "group"
  let gid = GroupId "abcd"
  createGroup tmp qcid groupFilename gid

  void $ liftIO $ spawn (cli qcid tmp ["member", "add", "--group", tmp </> groupFilename, "--in-place", tmp </> qcid2]) Nothing

  secretKey <- Ed25519.generateSecretKey
  let publicKey = Ed25519.toPublic secretKey
  let message = mkSignedMessage secretKey publicKey gid (Epoch 1) (ProposalMessage (mkRemoveProposal (fromJust (kpRef' kp))))

  let messageFilename = "signed-message.mls"
  BS.writeFile (tmp </> messageFilename) (rmRaw (mkRawMLS message))
  let signerKeyFilename = "signer-key.bin"
  BS.writeFile (tmp </> signerKeyFilename) (convert publicKey)

  void . liftIO $ spawn (cli qcid tmp ["check-signature", "--group", tmp </> groupFilename, "--message", tmp </> messageFilename, "--signer-key", tmp </> signerKeyFilename]) Nothing

createGroup :: FilePath -> String -> String -> GroupId -> IO ()
createGroup tmp store groupName gid = do
  groupJSON <-
    liftIO $
      spawn
        ( cli
            store
            tmp
            ["group", "create", T.unpack (toBase64Text (unGroupId gid))]
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
    <> T.unpack (client c)
    <> "@"
    <> T.unpack (domainText (qDomain usr))

spawn :: HasCallStack => CreateProcess -> Maybe ByteString -> IO ByteString
spawn cp minput = do
  (mout, ex) <- withCreateProcess
    cp
      { std_out = CreatePipe,
        std_in = if isJust minput then CreatePipe else Inherit
      }
    $ \minh mouth _ ph ->
      let writeInput = for_ ((,) <$> minput <*> minh) $ \(input, inh) ->
            BS.hPutStr inh input >> hClose inh
          readOutput = (,) <$> traverse BS.hGetContents mouth <*> waitForProcess ph
       in snd <$> concurrently writeInput readOutput
  case (mout, ex) of
    (Just out, ExitSuccess) -> pure out
    _ -> assertFailure "Failed spawning process"

cli :: String -> FilePath -> [String] -> CreateProcess
cli store tmp args =
  proc "mls-test-cli" $
    ["--store", tmp </> (store <> ".db")] <> args
