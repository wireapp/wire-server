-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.MLS.ECDSA where

import Crypto.Error
import Crypto.Hash hiding (hash)
import Crypto.PubKey.ECDSA
import Data.ASN1.BinaryEncoding
import Data.ASN1.Encoding
import Data.ASN1.Prim
import Data.Proxy
import Imports
import Wire.API.MLS.Serialisation

-- | Decode an ECDSA signature.
decodeSignature ::
  forall curve.
  (EllipticCurveECDSA curve) =>
  Proxy curve ->
  ByteString ->
  Maybe (Signature curve)
decodeSignature curve bs = do
  ints <- case decodeASN1' DER bs of
    Right ([Start Sequence, IntVal r, IntVal s, End Sequence]) -> pure (r, s)
    _ -> Nothing
  maybeCryptoError $ signatureFromIntegers curve ints

-- Encode an ECDSA signature.
encodeSignature ::
  forall curve.
  (EllipticCurveECDSA curve) =>
  Proxy curve ->
  Signature curve ->
  ByteString
encodeSignature curve sig = case signatureToIntegers curve sig of
  (r, s) ->
    encodeASN1'
      DER
      [ Start Sequence,
        IntVal r,
        IntVal s,
        End Sequence
      ]

verifySignature ::
  forall curve a hash.
  ( EllipticCurveECDSA curve,
    HashAlgorithm hash
  ) =>
  Proxy curve ->
  hash ->
  ByteString ->
  RawMLS a ->
  ByteString ->
  Bool
verifySignature curve hash pub x sig =
  fromMaybe False $ do
    sig' <- decodeSignature curve sig
    pub' <- maybeCryptoError $ decodePublic curve pub
    let valid = verify curve hash pub' sig' x.raw
    pure valid
