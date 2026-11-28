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

-- | Tests for the post-quantum cipher suites.
--
-- The key packages in @test/resources/pq@ were made by core-crypto, one per
-- cipher suite, together with the key package ref core-crypto computed for each.
module Test.Wire.API.MLS.PostQuantum (tests) where

import Crypto.Error (throwCryptoErrorIO)
import Crypto.PubKey.MLDSA (MLDSA)
import Crypto.PubKey.MLDSA qualified as Crypton
import Data.Bits (complement)
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.Proxy
import Imports
import Numeric (showHex)
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.MLDSA qualified as MLDSA
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Validation

pqSuites :: [CipherSuiteTag]
pqSuites =
  [ MLS_128_MLKEM768X25519_AES128GCM_SHA256_Ed25519,
    MLS_128_MLKEM768X25519_AES256GCM_SHA384_Ed25519,
    MLS_128_MLKEM768P256_AES128GCM_SHA256_P256,
    MLS_128_MLKEM768P256_AES256GCM_SHA384_P256,
    MLS_192_MLKEM1024P384_AES256GCM_SHA384_P384,
    MLS_128_MLKEM768_AES256GCM_SHA384_Ed25519,
    MLS_128_MLKEM768_AES256GCM_SHA384_P256,
    MLS_192_MLKEM1024_AES256GCM_SHA384_P384,
    MLS_128_MLKEM768X25519_CHACHA20POLY1305_SHA384_MLDSA44,
    MLS_192_MLKEM768_AES256GCM_SHA384_MLDSA65,
    MLS_256_MLKEM1024_AES256GCM_SHA384_MLDSA87
  ]

tests :: TestTree
tests =
  testGroup
    "MLS post-quantum cipher suites"
    [ testCase "codepoints match core-crypto" testCodepoints,
      testCase "signature schemes match core-crypto" testSignatureSchemes,
      testGroup "core-crypto key packages" $ map keyPackageTests pqSuites,
      testGroup
        "ML-DSA"
        [ testCase "verifies a signature with the empty context" testMLDSARoundtrip,
          testCase "rejects a signature with a non-empty context" testMLDSAContext,
          testCase "rejects keys and signatures of the wrong length" testMLDSAMalformed,
          testCase "rejects a key of another parameter set" testMLDSAParamSetMismatch
        ]
    ]

testCodepoints :: Assertion
testCodepoints = do
  map (cipherSuiteNumber . tagCipherSuite) pqSuites @?= [0xF001 .. 0xF00B]
  for_ [minBound .. maxBound] $ \t ->
    cipherSuiteTag (tagCipherSuite t) @?= Just t

testSignatureSchemes :: Assertion
testSignatureSchemes =
  map csSignatureScheme pqSuites
    @?= [ Ed25519,
          Ed25519,
          Ecdsa_secp256r1_sha256,
          Ecdsa_secp256r1_sha256,
          Ecdsa_secp384r1_sha384,
          Ed25519,
          Ecdsa_secp256r1_sha256,
          Ecdsa_secp384r1_sha384,
          Mldsa44,
          Mldsa65,
          Mldsa87
        ]

fixturePath :: CipherSuiteTag -> String -> FilePath
fixturePath cs ext =
  "test/resources/pq/key_package_0x"
    <> showHex (cipherSuiteNumber (tagCipherSuite cs)) ""
    <> ext

keyPackageTests :: CipherSuiteTag -> TestTree
keyPackageTests cs =
  testGroup
    (show cs)
    [ testCase "validates" $ do
        (kp, _) <- loadKeyPackage
        fmap fst (validateKeyPackage Nothing kp) @?= Right cs,
      testCase "has the same ref as in core-crypto" $ do
        (_, kpData) <- loadKeyPackage
        ref <- BS.readFile (fixturePath cs ".ref")
        kpRef cs (KeyPackageData kpData) @?= KeyPackageRef ref,
      testCase "rejects a tampered signature" $ do
        (kp, _) <- loadKeyPackage
        let sig = kp.signature_
            i = BS.length sig `div` 2
            sig' = BS.take i sig <> BS.singleton (complement (BS.index sig i)) <> BS.drop (i + 1) sig
        fmap fst (validateKeyPackage Nothing kp {signature_ = sig'})
          @?= Left InvalidKeyPackageSignature
    ]
  where
    loadKeyPackage = do
      kpData <- BS.readFile (fixturePath cs ".mls")
      case decodeMLS' @KeyPackage kpData of
        Left err -> assertFailure ("cannot parse key package: " <> show err)
        Right kp -> pure (kp, kpData)

message :: RawMLS ()
message = RawMLS "MLS 1.0 test message" ()

genKeyPair :: forall p. (MLDSA p) => Proxy p -> IO (ByteString, Crypton.SigningKey p)
genKeyPair p = do
  (vk, sk) <- Crypton.generateKeyPair p
  pure (BA.convert vk, sk)

signWith :: forall p. (MLDSA p) => Crypton.SigningKey p -> Crypton.Context -> IO ByteString
signWith sk ctx = BA.convert <$> Crypton.sign sk ctx message.raw

testMLDSARoundtrip :: Assertion
testMLDSARoundtrip = do
  check (Proxy @MLDSA.MLDSA44)
  check (Proxy @MLDSA.MLDSA65)
  check (Proxy @MLDSA.MLDSA87)
  where
    check :: forall p. (MLDSA p) => Proxy p -> Assertion
    check p = do
      (pk, sk) <- genKeyPair p
      sig <- signWith sk Crypton.emptyContext
      assertBool "valid signature" $ MLDSA.verifySignature p pk message sig
      assertBool "other message" . not $
        MLDSA.verifySignature p pk (RawMLS "MLS 1.0 other message" ()) sig

testMLDSAContext :: Assertion
testMLDSAContext = do
  (pk, sk) <- genKeyPair (Proxy @MLDSA.MLDSA65)
  ctx <- throwCryptoErrorIO (Crypton.context ("wire" :: ByteString))
  sig <- signWith sk ctx
  assertBool "signature with a context" . not $
    MLDSA.verifySignature (Proxy @MLDSA.MLDSA65) pk message sig

testMLDSAMalformed :: Assertion
testMLDSAMalformed = do
  let p = Proxy @MLDSA.MLDSA65
  (pk, sk) <- genKeyPair p
  sig <- signWith sk Crypton.emptyContext
  let rejects what pk' sig' =
        assertBool what . not $ MLDSA.verifySignature p pk' message sig'
  rejects "empty key" "" sig
  rejects "empty signature" pk ""
  rejects "truncated key" (BS.init pk) sig
  rejects "truncated signature" pk (BS.init sig)
  rejects "extended key" (pk <> "\0") sig
  rejects "extended signature" pk (sig <> "\0")

testMLDSAParamSetMismatch :: Assertion
testMLDSAParamSetMismatch = do
  (pk, sk) <- genKeyPair (Proxy @MLDSA.MLDSA65)
  sig <- signWith sk Crypton.emptyContext
  assertBool "ML-DSA-44" . not $ MLDSA.verifySignature (Proxy @MLDSA.MLDSA44) pk message sig
  assertBool "ML-DSA-87" . not $ MLDSA.verifySignature (Proxy @MLDSA.MLDSA87) pk message sig
  BS.length pk @?= 1952
