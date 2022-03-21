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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Domain
import Data.Either.Combinators
import Data.Hex
import Data.Id
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Commit
import Wire.API.MLS.Credential
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
      testCase "key package ref" testKeyPackageRef
    ]

testParseKeyPackage :: IO ()
testParseKeyPackage = do
  kpData <- LBS.readFile "test/resources/key_package1.mls"
  case decodeMLS @KeyPackage kpData of
    Left err -> assertFailure (T.unpack err)
    Right (kpTBS -> kp) -> do
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
  msgEpoch msg @?= 0

  case msgSender msg of
    MemberSender kp -> kp @?= KeyPackageRef (fromRight' (unhex "24e4b0a802a2b81f00a9af7df5e91da8"))
    _ -> assertFailure "Unexpected sender type"

  let payload = msgPayload msg
  commit <- case msgTBS payload of
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
  msgEpoch msg @?= 0
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
  kpData <- LBS.readFile "test/resources/key_package1.mls"
  ref <- KeyPackageRef <$> BS.readFile "test/resources/key_package_ref1"
  kpRef MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519 (KeyPackageData kpData) @?= ref
