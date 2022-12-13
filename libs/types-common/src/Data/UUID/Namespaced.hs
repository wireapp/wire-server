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

module Data.UUID.Namespaced where

import Data.Bits
import Data.UUID
import Data.UUID.V4
import Data.Word
import Imports

class IsNamespace n where
  namespaceToByte :: n -> Word8
  namespaceFromByte :: Word8 -> Maybe n

instance IsNamespace Word8 where
  namespaceToByte = id
  namespaceFromByte = Just

data NamespacedUUID n = NamespacedUUID
  { nuNamespace :: n,
    nuUUID :: UUID
  }

randomNamespacedUUID :: IsNamespace n => n -> IO (NamespacedUUID n)
randomNamespacedUUID n = do
  uuid <- nextRandom
  pure . NamespacedUUID n . uncurry fromWords64 $ case toWords64 uuid of
    (w1, w2) -> (setNamespace w1, w2)
  where
    setNamespace :: Word64 -> Word64
    setNamespace w =
      (w .&. 0xffffffffffff0000)
        .|. 0x3000 -- set version to 3
        -- replace the 12 bits after version with the namespace
        .|. fromIntegral (namespaceToByte n)

uuidToNamespacedUUID :: IsNamespace n => UUID -> Maybe (NamespacedUUID n)
uuidToNamespacedUUID uuid = do
  let (w1, _) = toWords64 uuid
  guard $ (w1 .&. 0x000000000000f000) == 0x3000
  n <- namespaceFromByte (fromIntegral (w1 .&. 0x00000000000000ff))
  pure (NamespacedUUID n uuid)
