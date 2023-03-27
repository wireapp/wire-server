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
import Data.Domain
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
import Wire.API.MLS.Credential
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.API.MLS.HPKEPublicKey
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Message
import Wire.API.MLS.Proposal
import Wire.API.MLS.ProtocolVersion
import Wire.API.MLS.PublicGroupState
import Wire.API.MLS.Serialisation

tests :: TestTree
tests =
  testGroup "MLS" $
    [ testCase "parse key package" testParseKeyPackage,
      testCase "parse commit message" testParseCommit,
      testCase "parse application message" testParseApplication,
      testCase "parse welcome message" testParseWelcome,
      testCase "key package ref" testKeyPackageRef,
      testCase "validate message signature" testVerifyMLSPlainTextWithKey,
      testCase "create signed remove proposal" testRemoveProposalMessageSignature,
      testCase "parse GroupInfoBundle" testParseGroupInfoBundle -- TODO: remove this also
    ]

testParseKeyPackage :: IO ()
testParseKeyPackage = do
  kpData <- BS.readFile "test/resources/key_package1.mls"
  kp <- case decodeMLS' @KeyPackage kpData of
    Left err -> assertFailure (T.unpack err)
    Right x -> pure x

  pvTag (kp.protocolVersion) @?= Just ProtocolMLS10
  kp.cipherSuite @?= CipherSuite 1
  BS.length (unHPKEPublicKey kp.initKey) @?= 32

  case keyPackageIdentity kp of
    Left err -> assertFailure $ "Failed to parse identity: " <> T.unpack err
    Right identity ->
      identity
        @?= ClientIdentity
          { ciDomain = Domain "mls.example.com",
            ciUser = Id (fromJust (UUID.fromString "b455a431-9db6-4404-86e7-6a3ebe73fcaf")),
            ciClient = newClientId 0x3ae58155
          }

  -- check raw TBS package
  let rawTBS = kp.tbs.rmRaw
  rawTBS @?= BS.take 196 kpData

-- TODO
testParseCommit :: IO ()
testParseCommit = pure ()

-- TODO
testParseApplication :: IO ()
testParseApplication = pure ()

-- TODO
testParseWelcome :: IO ()
testParseWelcome = pure ()

testKeyPackageRef :: IO ()
testKeyPackageRef = do
  kpData <- BS.readFile "test/resources/key_package1.mls"
  ref <- KeyPackageRef <$> BS.readFile "test/resources/key_package_ref1"
  kpRef MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519 (KeyPackageData kpData) @?= ref

-- TODO
testVerifyMLSPlainTextWithKey :: IO ()
testVerifyMLSPlainTextWithKey = pure ()

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
  kp :: RawMLS KeyPackage <-
    liftIO $
      decodeMLSError <$> spawn (cli qcid2 tmp ["key-package", "create"]) Nothing
  liftIO $ BS.writeFile (tmp </> qcid2) (rmRaw kp)

  let groupFilename = "group"
  let gid = GroupId "abcd"
  createGroup tmp qcid groupFilename gid

  void $ liftIO $ spawn (cli qcid tmp ["member", "add", "--group", tmp </> groupFilename, "--in-place", tmp </> qcid2]) Nothing

  secretKey <- Ed25519.generateSecretKey
  let publicKey = Ed25519.toPublic secretKey
  let proposal = mkRawMLS (RemoveProposal (error "TODO: remove proposal"))
  let message =
        mkSignedMessage
          secretKey
          publicKey
          gid
          (Epoch 1)
          (FramedContentProposal proposal)

  let messageFilename = "signed-message.mls"
  BS.writeFile (tmp </> messageFilename) (rmRaw (mkRawMLS message))
  let signerKeyFilename = "signer-key.bin"
  BS.writeFile (tmp </> signerKeyFilename) (convert publicKey)

  void . liftIO $
    spawn
      ( cli
          qcid
          tmp
          [ "consume",
            "--group",
            tmp </> groupFilename,
            "--signer-key",
            tmp </> signerKeyFilename,
            tmp </> messageFilename
          ]
      )
      Nothing

testParseGroupInfoBundle :: IO ()
testParseGroupInfoBundle = withSystemTempDirectory "mls" $ \tmp -> do
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
  kp :: RawMLS KeyPackage <- liftIO $ decodeMLSError <$> spawn (cli qcid2 tmp ["key-package", "create"]) Nothing
  liftIO $ BS.writeFile (tmp </> qcid2) (rmRaw kp)

  let groupFilename = "group"
  let gid = GroupId "abcd"
  createGroup tmp qcid groupFilename gid

  void $
    liftIO $
      spawn
        ( cli
            qcid
            tmp
            [ "member",
              "add",
              "--group",
              tmp </> groupFilename,
              "--in-place",
              tmp </> qcid2,
              "--group-state-out",
              tmp </> "group-info-bundle"
            ]
        )
        Nothing

  bundleBS <- BS.readFile (tmp </> "group-info-bundle")
  case decodeMLS' @PublicGroupState bundleBS of
    Left err -> assertFailure ("Failed parsing PublicGroupState: " <> T.unpack err)
    Right _ -> pure ()

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
