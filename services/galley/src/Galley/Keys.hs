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
  ( mlsKeyPair_ed25519,
    loadMLSKeys,
  )
where

import Control.Exception
import Crypto.PubKey.Ed25519
import Data.ASN1.BinaryEncoding
import Data.ASN1.Encoding
import Data.ASN1.Types
import Data.Bifunctor
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.PEM
import Data.X509
import Imports
import Wire.API.MLS.Credential
import Wire.API.MLS.Keys

data MLSPrivateKeyException = MLSPrivateKeyException
  { mpkePath :: FilePath,
    mpkeMsg :: String
  }
  deriving (Eq, Show, Typeable)

instance Exception MLSPrivateKeyException where
  displayException e = mpkePath e <> ": " <> mpkeMsg e

loadMLSKeys :: Map SignatureSchemeTag FilePath -> IO MLSKeys
loadMLSKeys m =
  MLSKeys
    <$> traverse loadEd25519KeyPair (Map.lookup Ed25519 m)

loadEd25519KeyPair :: FilePath -> IO (SecretKey, PublicKey)
loadEd25519KeyPair path = do
  bytes <- LBS.readFile path
  priv <-
    either (throwIO . MLSPrivateKeyException path) pure $
      decodeEd25519PrivateKey bytes
  pure (priv, toPublic priv)

decodeEd25519PrivateKey ::
  LByteString ->
  Either String SecretKey
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
