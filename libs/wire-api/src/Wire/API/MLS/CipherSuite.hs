{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

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

module Wire.API.MLS.CipherSuite where

import Crypto.Hash.Algorithms
import qualified Crypto.KDF.HKDF as HKDF
import Data.Constraint
import Data.Singletons
import Data.Singletons.TH
import Data.Word
import Imports
import Wire.API.MLS.Serialisation

newtype CipherSuite = CipherSuite {cipherSuiteNumber :: Word16}
  deriving stock (Eq, Show)
  deriving newtype (ParseMLS)

-- Key derivation function.
data KDFTag = HKDF256 | HKDF384 | HKDF512

$(genSingletons [''KDFTag])

-- | See https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol.html#table-5
-- and https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-hpke-12#section-7.2
cipherSuiteKDF :: CipherSuite -> Maybe KDFTag
cipherSuiteKDF (CipherSuite n)
  | n >= 0 && n <= 3 = pure HKDF256
  | n >= 4 && n <= 6 = pure HKDF512
  | n == 7 = pure HKDF384
  | otherwise = Nothing

type family KDFHashAlgo (kdf :: KDFTag) where
  KDFHashAlgo 'HKDF256 = SHA256
  KDFHashAlgo 'HKDF384 = SHA384
  KDFHashAlgo 'HKDF512 = SHA512

kdfHashAlgo :: forall (kdf :: KDFTag). Sing kdf -> Dict (HashAlgorithm (KDFHashAlgo kdf))
kdfHashAlgo skdf = $(sCases ''KDFTag [|skdf|] [|Dict|])

kdfHash :: KDFTag -> ByteString -> ByteString -> ByteString
kdfHash t ctx value = case toSing t of
  SomeSing st -> go st
  where
    go :: forall (t :: KDFTag). Sing t -> ByteString
    go st = case kdfHashAlgo st of
      Dict -> HKDF.expand (HKDF.extract @(KDFHashAlgo t) (mempty :: ByteString) value) ctx 16
