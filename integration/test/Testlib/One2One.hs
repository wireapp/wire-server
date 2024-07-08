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

-- This is a duplicate of `Galley.Types.Conversations.One2One`
-- and is needed because we do not have access to galley code in the integration tests
module Testlib.One2One (generateRemoteAndConvIdWithDomain) where

import Control.Error (atMay)
import qualified Crypto.Hash as Crypto
import Data.Bits
import Data.ByteArray (convert)
import Data.ByteString
import qualified Data.ByteString as B
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as L
import Data.UUID as UUID
import SetupHelpers (randomUser)
import Testlib.Prelude

generateRemoteAndConvIdWithDomain :: (MakesValue domain, MakesValue a) => domain -> Bool -> a -> App (Value, Value)
generateRemoteAndConvIdWithDomain remoteDomain shouldBeLocal lUserId = do
  (localDomain, localUser) <- objQid lUserId
  otherUsr <- randomUser remoteDomain def >>= objId
  otherDomain <- asString remoteDomain
  let (cId, cDomain) =
        one2OneConvId
          (fromMaybe (error "invalid UUID") (UUID.fromString localUser), localDomain)
          (fromMaybe (error "invalid UUID") (UUID.fromString otherUsr), otherDomain)
      isLocal = localDomain == cDomain
  if shouldBeLocal == isLocal
    then
      pure
        $ ( object ["id" .= (otherUsr), "domain" .= otherDomain],
            object ["id" .= (UUID.toString cId), "domain" .= cDomain]
          )
    else generateRemoteAndConvIdWithDomain remoteDomain shouldBeLocal lUserId

one2OneConvId :: (UUID, String) -> (UUID, String) -> (UUID, String)
one2OneConvId a@(a1, dom1) b@(a2, dom2) = case compare (dom1, a1) (dom2, a2) of
  GT -> one2OneConvId b a
  _ ->
    let c =
          mconcat
            [ L.toStrict (UUID.toByteString namespace),
              quidToByteString a,
              quidToByteString b
            ]
        x = hash c
        result =
          toUuidV5
            . mkV5
            . fromMaybe nil
            . UUID.fromByteString
            . L.fromStrict
            . B.take 16
            $ x
        domain
          | fromMaybe 0 (atMay (B.unpack x) 16) .&. 0x80 == 0 = dom1
          | otherwise = dom2
     in (result, domain)
  where
    hash :: ByteString -> ByteString
    hash = convert . Crypto.hash @ByteString @Crypto.SHA256

    namespace :: UUID
    namespace = fromWords 0x9a51edb8 0x060c0d9a 0x0c2950a8 0x5d152982

    quidToByteString :: (UUID, String) -> ByteString
    quidToByteString (uid, domain) = toASCIIBytes uid <> toByteString' domain

newtype UuidV5 = UuidV5 {toUuidV5 :: UUID}
  deriving (Eq, Ord, Show)

mkV5 :: UUID -> UuidV5
mkV5 u = UuidV5
  $ case toWords u of
    (x0, x1, x2, x3) ->
      fromWords
        x0
        (retainVersion 5 x1)
        (retainVariant 2 x2)
        x3
  where
    retainVersion :: Word32 -> Word32 -> Word32
    retainVersion v x = (x .&. 0xFFFF0FFF) .|. (v `shiftL` 12)

    retainVariant :: Word32 -> Word32 -> Word32
    retainVariant v x = (x .&. 0x3FFFFFFF) .|. (v `shiftL` 30)
