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

-- | ML-DSA (FIPS 204) signature verification for MLS.
--
-- MLS clients sign with pure ML-DSA and an empty context string, and encode
-- public keys and signatures as FIPS 204 @pkEncode@ / @sigEncode@ byte strings.
module Wire.API.MLS.MLDSA
  ( verifySignature,
    MLDSA44,
    MLDSA65,
    MLDSA87,
  )
where

import Crypto.Error (maybeCryptoError)
import Crypto.PubKey.MLDSA (MLDSA, MLDSA44, MLDSA65, MLDSA87)
import Crypto.PubKey.MLDSA qualified as MLDSA
import Data.Proxy
import Imports
import Wire.API.MLS.Serialisation

-- | Verify a pure ML-DSA signature with the empty context string.
--
-- Returns 'False' for a public key or signature whose length is wrong for the
-- parameter set.
verifySignature ::
  forall p x.
  (MLDSA p) =>
  Proxy p ->
  ByteString ->
  RawMLS x ->
  ByteString ->
  Bool
verifySignature _ pub x sig = fromMaybe False $ do
  pk :: MLDSA.VerificationKey p <- maybeCryptoError (MLDSA.verificationKey pub)
  s :: MLDSA.Signature p <- maybeCryptoError (MLDSA.signature sig)
  pure $ MLDSA.verify pk MLDSA.emptyContext x.raw s
