-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.API.One2One
  ( convId,
    createRemoteOne2OneConversation,
  )
where

import Control.Error (atMay)
import Control.Monad.Catch
import qualified Crypto.Hash as Crypto
import Data.Bits
import Data.ByteArray (convert)
import qualified Data.ByteString as B
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as L
import Data.Id
import Data.Qualified
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Galley.API.Util
import Galley.App
import Galley.Types.UserList
import Imports
import Wire.API.Federation.Error (federationNotImplemented)
import Wire.API.Routes.Public.Galley (ConversationResponse)

-- | The hash function used to obtain the 1-1 conversation ID for a pair of users.
--
-- /Note/: the hash function must always return byte strings of length > 16.
hash :: ByteString -> ByteString
hash = convert . Crypto.hash @ByteString @Crypto.SHA256

-- | A randomly-generated UUID to use as a namespace for the UUIDv5 of 1-1
-- conversation IDs
namespace :: UUID
namespace = UUID.fromWords 0x9a51edb8 0x060c0d9a 0x0c2950a8 0x5d152982

compareDomains :: Ord a => Qualified a -> Qualified a -> Ordering
compareDomains (Qualified a1 dom1) (Qualified a2 dom2) =
  compare (dom1, a1) (dom2, a2)

quidToByteString :: Qualified UserId -> ByteString
quidToByteString (Qualified uid domain) = toByteString' uid <> toByteString' domain

setUUIDv5 :: UUID -> UUID
setUUIDv5 x = case UUID.toWords x of
  (w0, w1, w2, w3) ->
    UUID.fromWords
      w0
      (w1 .&. 0xffff0fff .|. 0x5000)
      (w2 .&. 0x3fffffff .|. 0x80000000)
      w3

-- | This function returns the 1-1 conversation for a given pair of users.
--
-- Let A, B denote the (not necessarily distinct) backends of the two users,
-- with the domain of A less or equal than the domain of B in the lexicographic
-- ordering of their ascii encodings. Given users a@A and b@B, the UUID and
-- owning domain of the unique 1-1 conversation between a and b shall be a
-- deterministic function of the input data, plus some fixed parameters, as
-- described below.
--
-- __Parameters__
--
--   * A (collision-resistant) hash function h with N bits of output, where N
--     s a multiple of 8 strictly larger than 128; this is set to SHA256.
--   * A "namespace" UUID n.
--
-- __Algorithm__
--
-- First, in the special case where A and B are the same backend, assume that
-- the UUID of a is lower than that of b. If that is not the case, swap a
-- and b in the following. This is necessary to ensure that the function we
-- describe below is symmetric in its arguments.
-- Let c be the bytestring obtained as the concatenation of the following 5
-- components:
--
--   * the 16 bytes of the namespace n
--   * the 16 bytes of the UUID of a
--   * the ascii encoding of the domain of A
--   * the 16 bytes of the UUID of b
--   * the ascii encoding of the domain of B,
--
-- and let x = h(c) be its hashed value. The UUID of the 1-1 conversation
-- between a and b is obtained by converting the first 128 bits of x to a UUID
-- V5. Note that our use of V5 here is not strictly compliant with RFC 4122,
-- since we are using a custom hash and not necessarily SHA1.
--
-- The owning domain for the conversation is set to be A if bit 128 of x (i.e.
-- the most significant bit of the octet at index 16) is 0, and B otherwise.
-- This is well-defined, because we assumed the number of bits of x to be
-- strictly larger than 128.
convId :: Qualified UserId -> Qualified UserId -> Qualified ConvId
convId a b = case compareDomains a b of
  GT -> convId b a
  _ ->
    let c =
          mconcat
            [ L.toStrict (UUID.toByteString namespace),
              quidToByteString a,
              quidToByteString b
            ]
        x = hash c
        result =
          setUUIDv5
            . fromMaybe UUID.nil
            . UUID.fromByteString
            . L.fromStrict
            . B.take 16
            $ x
        domain
          | (fromMaybe 0 (atMay (B.unpack x) 16)) .&. 0x80 == 0 = qDomain a
          | otherwise = qDomain b
     in Qualified (Id result) domain

createRemoteOne2OneConversation ::
  Local UserId -> ConnId -> Remote UserId -> Galley ConversationResponse
createRemoteOne2OneConversation self _con rother = do
  ensureConnected self (UserList [] [rother])
  throwM federationNotImplemented
