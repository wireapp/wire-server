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

-- | Handling of MLS private keys used for signing external proposals.
module Galley.Keys
  ( MLSPrivateKeyPaths,
    loadAllMLSKeys,
  )
where

import Control.Error.Util
import Control.Exception
import Crypto.ECC hiding (KeyPair)
import Crypto.Error
import Crypto.PubKey.ECDSA qualified as ECDSA
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.ASN1.BinaryEncoding
import Data.ASN1.BitArray
import Data.ASN1.Encoding
import Data.ASN1.Types
import Data.Bifunctor
import Data.ByteString.Lazy qualified as LBS
import Data.PEM
import Data.Proxy
import Data.X509
import Imports
import Network.Wai.Utilities.Exception
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Keys

type MLSPrivateKeyPaths = MLSKeysByPurpose (MLSKeys FilePath)

data MLSPrivateKeyException = MLSPrivateKeyException
  { mpkePath :: FilePath,
    mpkeMsg :: String
  }
  deriving (Eq, Show, Typeable)

instance Exception MLSPrivateKeyException where
  displayException e = mpkePath e <> ": " <> mpkeMsg e

loadAllMLSKeys :: MLSPrivateKeyPaths -> IO (MLSKeysByPurpose MLSPrivateKeys)
loadAllMLSKeys = traverse loadMLSKeys

loadMLSKeys :: MLSKeys FilePath -> IO MLSPrivateKeys
loadMLSKeys paths =
  MLSPrivateKeys
    <$> loadKeyPair @Ed25519 paths.ed25519
    <*> loadKeyPair @Ecdsa_secp256r1_sha256 paths.ecdsa_secp256r1_sha256
    <*> loadKeyPair @Ecdsa_secp384r1_sha384 paths.ecdsa_secp384r1_sha384
    <*> loadKeyPair @Ecdsa_secp521r1_sha512 paths.ecdsa_secp521r1_sha512

class LoadKeyPair (ss :: SignatureSchemeTag) where
  loadKeyPair :: FilePath -> IO (KeyPair ss)

instance LoadKeyPair Ed25519 where
  loadKeyPair = loadEd25519KeyPair

instance LoadKeyPair Ecdsa_secp256r1_sha256 where
  loadKeyPair = loadECDSAKeyPair @Curve_P256R1

instance LoadKeyPair Ecdsa_secp384r1_sha384 where
  loadKeyPair = loadECDSAKeyPair @Curve_P384R1

instance LoadKeyPair Ecdsa_secp521r1_sha512 where
  loadKeyPair = loadECDSAKeyPair @Curve_P521R1

class CurveOID c where
  curveOID :: [Integer]

instance CurveOID Curve_P256R1 where
  curveOID = [1, 2, 840, 10045, 3, 1, 7]

instance CurveOID Curve_P384R1 where
  curveOID = [1, 3, 132, 0, 34]

instance CurveOID Curve_P521R1 where
  curveOID = [1, 3, 132, 0, 35]

loadECDSAKeyPair ::
  forall c.
  (ECDSA.EllipticCurveECDSA c, CurveOID c) =>
  FilePath ->
  IO (ECDSA.PrivateKey c, ECDSA.PublicKey c)
loadECDSAKeyPair path = do
  bytes <- LBS.readFile path
  either (throwIO . MLSPrivateKeyException path) pure $
    decodeEcdsaKeyPair @c bytes

loadEd25519KeyPair :: FilePath -> IO (Ed25519.SecretKey, Ed25519.PublicKey)
loadEd25519KeyPair path = do
  bytes <- LBS.readFile path
  priv <-
    either (throwIO . MLSPrivateKeyException path) pure $
      decodeEd25519PrivateKey bytes
  pure (priv, Ed25519.toPublic priv)

decodeEcdsaKeyPair ::
  forall c.
  (ECDSA.EllipticCurveECDSA c, CurveOID c) =>
  LByteString ->
  Either String (ECDSA.PrivateKey c, ECDSA.PublicKey c)
decodeEcdsaKeyPair bytes = do
  let curve = Proxy @c
  pems <- pemParseLBS bytes
  pem <- expectOne "private key" pems
  let content = pemContent pem
  -- parse outer pkcs8 container as BER
  asn1 <- first displayExceptionNoBacktrace (decodeASN1' BER content)
  (oid, key) <- case asn1 of
    [ Start Sequence,
      IntVal _version,
      Start Sequence,
      OID [1, 2, 840, 10045, 2, 1], -- ecdsa
      OID oid,
      End Sequence,
      OctetString key,
      End Sequence
      ] -> pure (oid, key)
    _ -> Left "invalid ECDSA key format: expected pkcs8"
  note
    ( "private key curve mismatch, expected "
        <> show (curveOID @c)
        <> ", found "
        <> show oid
    )
    $ guard (oid == curveOID @c)
  -- parse key bytestring as BER again, this should be in the format of rfc5915
  asn1' <- first displayExceptionNoBacktrace (decodeASN1' BER key)
  (privBS, pubBS) <- case asn1' of
    [ Start Sequence,
      IntVal _version,
      OctetString priv,
      Start (Container Context _),
      BitString (BitArray _ pub),
      End (Container Context _),
      End Sequence
      ] -> pure (priv, pub)
    _ -> Left "invalid ECDSA key format: expected rfc5915 private key format"
  priv <-
    first displayExceptionNoBacktrace . eitherCryptoError $
      ECDSA.decodePrivate curve privBS
  pub <-
    first displayExceptionNoBacktrace . eitherCryptoError $
      ECDSA.decodePublic curve pubBS
  pure (priv, pub)

decodeEd25519PrivateKey ::
  LByteString ->
  Either String Ed25519.SecretKey
decodeEd25519PrivateKey bytes = do
  pems <- pemParseLBS bytes
  pem <- expectOne "private key" pems
  let content = pemContent pem
  asn1 <- first displayExceptionNoBacktrace (decodeASN1' BER content)
  (priv, remainder) <- fromASN1 asn1
  expectEmpty remainder
  case priv of
    PrivKeyEd25519 sec -> pure sec
    _ -> Left $ "invalid signature scheme (expected ed25519)"
  where
    expectEmpty :: [a] -> Either String ()
    expectEmpty [] = pure ()
    expectEmpty _ = Left "extraneous ASN.1 data"

expectOne :: String -> [a] -> Either String a
expectOne label [] = Left $ "no " <> label <> " found"
expectOne _ [x] = pure x
expectOne label _ = Left $ "found multiple " <> label <> "s"
