-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.MLS.KeyRotation (tests) where

import Crypto.ECC (Curve_P256R1, KeyPair (..), curveGenerateKeyPair)
import Crypto.PubKey.ECDSA qualified as ECDSA
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.ByteArray (convert)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Domain
import Data.Id
import Data.Proxy
import Data.UUID qualified as UUID
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Test.Wire.API.MLS (cli, decodeMLSError, randomIdentity, spawn)
import UnliftIO (withSystemTempDirectory)
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.KeyRotation
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Validation.Error

tests :: TestTree
tests =
  testGroup
    "KeyRotation"
    [ testCase "serialise the signed content" testSerialiseTBS,
      testCase "accept a valid Ed25519 signature" $
        testValidSignature MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519 =<< ed25519Signer,
      testCase "accept a valid P256 signature" $
        testValidSignature MLS_128_DHKEMP256_AES128GCM_SHA256_P256 =<< p256Signer,
      testCase "reject a signature by another key" testSignatureByOtherKey,
      testCase "reject a signature over another rotation" testSignatureOverOtherRotation,
      testCase "reject a rotation to the same key" testSameKey,
      testCase "accept an x509 key package of the client" testX509KeyPackage,
      testCase "reject a basic key package" testBasicKeyPackage,
      testCase "reject an x509 key package of another client" testOtherClientKeyPackage
    ]

-- | A signature key pair, as raw public key and signing function.
data Signer = Signer
  { publicKey :: ByteString,
    signTBS :: MLSKeyRotationTBS -> IO ByteString
  }

ed25519Signer :: IO Signer
ed25519Signer = do
  sk <- Ed25519.generateSecretKey
  let pk = Ed25519.toPublic sk
  pure
    Signer
      { publicKey = convert pk,
        signTBS = signWithLabel @Ed25519 keyRotationLabel (sk, pk) . mkRawMLS
      }

p256Signer :: IO Signer
p256Signer = do
  kp <- curveGenerateKeyPair (Proxy @Curve_P256R1)
  let pk = keypairGetPublic kp
  pure
    Signer
      { publicKey = ECDSA.encodePublic (Proxy @Curve_P256R1) pk,
        signTBS = signWithLabel @Ecdsa_secp256r1_sha256 keyRotationLabel (keypairGetPrivate kp, pk) . mkRawMLS
      }

fixedIdentity :: ClientIdentity
fixedIdentity =
  ClientIdentity
    { ciDomain = Domain "example.com",
      ciUser = Id (fromMaybe (error "invalid UUID") (UUID.fromString "7c9f0f1e-6a0b-4d2c-9a5b-2f4c3a1b0d9e")),
      ciClient = ClientId 0xabcd
    }

-- | The format is part of the API: clients have to produce exactly these bytes.
testSerialiseTBS :: IO ()
testSerialiseTBS = do
  let identity = "7c9f0f1e-6a0b-4d2c-9a5b-2f4c3a1b0d9e:abcd@example.com"
      tbs = MLSKeyRotationTBS {clientIdentity = fixedIdentity, oldKey = "old", newKey = "new!"}
  encodeMLS' tbs
    @?= BS.concat
      [ BS.singleton (fromIntegral (B8.length identity)),
        identity,
        "\x03old",
        "\x04new!"
      ]

rotationSignature :: Signer -> ClientIdentity -> ByteString -> IO ByteString
rotationSignature signer cid new =
  signer.signTBS MLSKeyRotationTBS {clientIdentity = cid, oldKey = signer.publicKey, newKey = new}

testValidSignature :: CipherSuiteTag -> Signer -> IO ()
testValidSignature cs old = do
  new <- (.publicKey) <$> ed25519Signer
  sig <- rotationSignature old fixedIdentity new
  verifyKeyRotationSignature cs fixedIdentity old.publicKey new sig @?= Right ()

testSignatureByOtherKey :: IO ()
testSignatureByOtherKey = do
  old <- ed25519Signer
  attacker <- ed25519Signer
  new <- (.publicKey) <$> ed25519Signer
  sig <-
    attacker.signTBS
      MLSKeyRotationTBS {clientIdentity = fixedIdentity, oldKey = old.publicKey, newKey = new}
  verifyKeyRotationSignature MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519 fixedIdentity old.publicKey new sig
    @?= Left KeyRotationInvalidSignature

-- | A signature can only be used for the client and the new key it was made
-- for.
testSignatureOverOtherRotation :: IO ()
testSignatureOverOtherRotation = do
  let cs = MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519
  old <- ed25519Signer
  new <- (.publicKey) <$> ed25519Signer
  other <- (.publicKey) <$> ed25519Signer
  otherClient <- randomIdentity
  sig <- rotationSignature old fixedIdentity new
  verifyKeyRotationSignature cs fixedIdentity old.publicKey other sig
    @?= Left KeyRotationInvalidSignature
  verifyKeyRotationSignature cs otherClient old.publicKey new sig
    @?= Left KeyRotationInvalidSignature

testSameKey :: IO ()
testSameKey = do
  old <- ed25519Signer
  sig <- rotationSignature old fixedIdentity old.publicKey
  verifyKeyRotationSignature MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519 fixedIdentity old.publicKey old.publicKey sig
    @?= Left KeyRotationSameKey

-- | Create a key package with mls-test-cli, and return it together with the
-- public signature key of the store.
cliKeyPackage :: FilePath -> ClientIdentity -> String -> IO (RawMLS KeyPackage, ByteString)
cliKeyPackage tmp cid credType = do
  let store = B8.unpack (encodeMLS' cid)
  void $ spawn (cli store tmp ["init", "-t", credType, store]) Nothing
  kp <- decodeMLSError <$> spawn (cli store tmp ["key-package", "create"]) Nothing
  pk <- spawn (cli store tmp ["public-key"]) Nothing
  pure (kp, pk)

rotationWith :: RawMLS KeyPackage -> MLSKeyRotation
rotationWith kp = MLSKeyRotation {keyPackage = kp, signature = ""}

testX509KeyPackage :: IO ()
testX509KeyPackage = withSystemTempDirectory "mls" $ \tmp -> do
  cid <- randomIdentity
  (kp, pk) <- cliKeyPackage tmp cid "x509"
  validateKeyRotationKeyPackage cid (rotationWith kp)
    @?= Right (MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519, pk)

testBasicKeyPackage :: IO ()
testBasicKeyPackage = withSystemTempDirectory "mls" $ \tmp -> do
  cid <- randomIdentity
  (kp, _) <- cliKeyPackage tmp cid "basic"
  validateKeyRotationKeyPackage cid (rotationWith kp) @?= Left KeyRotationNotX509

testOtherClientKeyPackage :: IO ()
testOtherClientKeyPackage = withSystemTempDirectory "mls" $ \tmp -> do
  cid <- randomIdentity
  other <- randomIdentity
  (kp, _) <- cliKeyPackage tmp other "x509"
  validateKeyRotationKeyPackage cid (rotationWith kp)
    @?= Left (KeyRotationInvalidKeyPackage IdentityMismatch)
