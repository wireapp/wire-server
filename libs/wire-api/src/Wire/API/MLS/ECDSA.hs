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

import Crypto.ECC
import Crypto.Error
import Crypto.Hash
import Crypto.PubKey.ECDSA
import Data.Binary.Get
import Data.ByteString.Lazy qualified as BL
import Data.Proxy
import Imports
import Wire.API.MLS.Serialisation

-- | Decode an ECDSA signature.
--
-- See https://www.rfc-editor.org/rfc/rfc8032.html#section-3.3
decodeSignature ::
  forall curve.
  EllipticCurveECDSA curve =>
  Proxy curve ->
  ByteString ->
  Maybe (Signature curve)
decodeSignature p bs = do
  let b = curveOrderBits p
  let parser = (,) <$> decodeInteger b <*> decodeInteger b
  ints <- case runGetOrFail parser (BL.fromStrict bs) of
    Right (remainder, _, x) | BL.null remainder -> pure x
    _ -> Nothing
  maybeCryptoError $ signatureFromIntegers p ints

-- | Parser for an integer with the given number of bits. The number of bits
-- must be a multiple of 8.
--
-- See https://www.rfc-editor.org/rfc/rfc8032.html#section-5.1.2
decodeInteger :: Int -> Get Integer
decodeInteger bits =
  foldr (\d n -> d + n * 256) 0
    <$> replicateM (bits `div` 8) (fromIntegral <$> getWord8)

verifySignature ::
  forall curve a.
  EllipticCurveECDSA curve =>
  Proxy curve ->
  ByteString ->
  RawMLS a ->
  ByteString ->
  Bool
verifySignature p pub x sig =
  fromMaybe False $ do
    sig' <- decodeSignature p sig
    pub' <- maybeCryptoError $ decodePublic p pub
    pure $ verify p SHA256 pub' sig' x.raw
