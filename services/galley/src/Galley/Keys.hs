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
import Crypto.PubKey.ECDSA qualified as ECDSA
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.ASN1.BinaryEncoding
import Data.ASN1.Encoding
import Data.ASN1.Types
import Data.Bifunctor
import Data.ByteString.Lazy qualified as LBS
import Data.Map qualified as Map
import Data.PEM
import Data.X509
import Debug.Trace
import Imports
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.Keys

type MLSPrivateKeyPaths = Map SignaturePurpose (Map SignatureSchemeTag FilePath)

data MLSPrivateKeyException = MLSPrivateKeyException
  { mpkePath :: FilePath,
    mpkeMsg :: String
  }
  deriving (Eq, Show, Typeable)

instance Exception MLSPrivateKeyException where
  displayException e = mpkePath e <> ": " <> mpkeMsg e

mapToFunction :: (Ord k, Monoid m) => Map k m -> k -> m
mapToFunction m x = Map.findWithDefault mempty x m

loadAllMLSKeys :: MLSPrivateKeyPaths -> IO (SignaturePurpose -> MLSKeys)
loadAllMLSKeys = fmap mapToFunction . traverse loadMLSKeys

loadMLSKeys :: Map SignatureSchemeTag FilePath -> IO MLSKeys
loadMLSKeys m =
  mkMLSKeys
    <$> traverse (loadKeyPair @Ed25519) (Map.lookup Ed25519 m)
    <*> traverse (loadKeyPair @Ecdsa_secp256r1_sha256) (Map.lookup Ecdsa_secp256r1_sha256 m)
    <*> traverse (loadKeyPair @Ecdsa_secp384r1_sha384) (Map.lookup Ecdsa_secp384r1_sha384 m)
    <*> traverse (loadKeyPair @Ecdsa_secp521r1_sha512) (Map.lookup Ecdsa_secp521r1_sha512 m)

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

loadECDSAKeyPair :: forall c. FilePath -> IO (ECDSA.PrivateKey c, ECDSA.PublicKey c)
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

decodeEcdsaKeyPair :: LByteString -> Either String (ECDSA.PrivateKey c, ECDSA.PublicKey c)
decodeEcdsaKeyPair bytes = do
  pems <- pemParseLBS bytes
  pem <-
    note "invalid PEM file" $
      find (\p -> pemName p == "EC PRIVATE KEY") pems
  let content = pemContent pem
  asn1 <- first displayException (decodeASN1' BER content)
  trace (show asn1) (error "todo")

decodeEd25519PrivateKey ::
  LByteString ->
  Either String Ed25519.SecretKey
decodeEd25519PrivateKey bytes = do
  pems <- pemParseLBS bytes
  pem <- expectOne "private key" pems
  let content = pemContent pem
  asn1 <- first displayException (decodeASN1' BER content)
  (priv, remainder) <- fromASN1 asn1
  expectEmpty remainder
  case priv of
    PrivKeyEd25519 sec -> pure sec
    _ -> Left $ "invalid signature scheme (expected ed25519)"
  where
    expectOne :: String -> [a] -> Either String a
    expectOne label [] = Left $ "no " <> label <> " found"
    expectOne _ [x] = pure x
    expectOne label _ = Left $ "found multiple " <> label <> "s"

    expectEmpty :: [a] -> Either String ()
    expectEmpty [] = pure ()
    expectEmpty _ = Left "extraneous ASN.1 data"
