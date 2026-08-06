-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Testlib.Prekeys
  ( getPrekey,
    getPrekeys,
    getLastPrekey,
  )
where

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Crypto.Random as Crypto
import Data.Aeson
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString.Base64 as Base64
import Data.String
import Data.String.Conversions (cs)
import Data.Word
import Prelude

getPrekey :: (Crypto.MonadRandom m) => m Value
getPrekey = mkPrekey 1

getPrekeys :: (Crypto.MonadRandom m) => Word16 -> m [Value]
getPrekeys n = mapM mkPrekey [0 .. n]

getLastPrekey :: (Crypto.MonadRandom m) => m Value
getLastPrekey = mkPrekey maxBound

mkPrekey :: (Crypto.MonadRandom m) => Word16 -> m Value
mkPrekey prekeyId = do
  pk <- newPrekey prekeyId
  pure $ object [fromString "id" .= prekeyId, fromString "key" .= pk]

-- | https://github.com/wireapp/proteus/blob/bb759d762bfde376fa5a8a08b1d1153a345ab28a/src/internal/keys.rs#L305
newPrekey :: (Crypto.MonadRandom m) => Word16 -> m String
newPrekey prekeyId = do
  secretKey <- Ed25519.generateSecretKey
  let publicKey = Ed25519.toPublic secretKey
  identitySecretKey <- Ed25519.generateSecretKey
  let identityPublicKey = Ed25519.toPublic identitySecretKey
      encodePublicKey k = CBOR.encodeMapLen 1 <> CBOR.encodeWord8 0 <> CBOR.encodeBytes (ByteArray.convert k)
      encodedIdentityKey = CBOR.encodeMapLen 1 <> CBOR.encodeWord8 0 <> encodePublicKey identityPublicKey
      cbor =
        CBOR.toStrictByteString $
          CBOR.encodeMapLen 5
            <> (CBOR.encodeWord8 0 <> CBOR.encodeWord8 1)
            <> (CBOR.encodeWord8 1 <> CBOR.encodeWord16 prekeyId)
            <> (CBOR.encodeWord8 2 <> encodePublicKey publicKey)
            <> (CBOR.encodeWord8 3 <> encodedIdentityKey)
            <> (CBOR.encodeWord8 4 <> CBOR.encodeNull)
  pure . cs $ Base64.encode cbor
