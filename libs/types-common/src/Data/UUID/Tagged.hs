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

module Data.UUID.Tagged
  ( UUID,
    toUUID,
    V4,
    V5,
    Version (..),
    version,
    addv4,
    unpack,
    mk,
  )
where

import Data.Bits
import Data.UUID qualified as D
import Imports

-- | Versioned UUID.
newtype UUID v = UUID {toUUID :: D.UUID}
  deriving (Eq, Ord, Show)

instance NFData (UUID v) where rnf (UUID a) = seq a ()

class Version v where
  -- | Try to turn a plain UUID into a versioned UUID.
  fromUUID :: D.UUID -> Maybe (UUID v)
  fromUUID u = guard (version u == versionValue @v) $> UUID u

  versionValue :: Word32

data V4

instance Version V4 where
  versionValue = 4

data V5

instance Version V5 where
  versionValue = 5

mk :: forall v. (Version v) => D.UUID -> UUID v
mk u = UUID $
  case D.toWords u of
    (x0, x1, x2, x3) ->
      D.fromWords
        x0
        (retainVersion (versionValue @v) x1)
        (retainVariant 2 x2)
        x3

-- | Extract the 'D.UUID' from a versioned UUID.
unpack :: UUID v -> D.UUID
unpack (UUID x) = x

-- | Add two @UUID V4@ values. This retains variant and version information and
-- adds all other bits.
--
-- Note: This should've been XOR as the operation is bit-local, which makes
-- us not have to think about wrap-around and whatnot.  I'm not 100% sure that
-- this thing does not have subtle bugs that steer the number distribtution, which could
-- introduce an increase of collisions
addv4 :: UUID V4 -> UUID V4 -> UUID V4
addv4 (UUID a) (UUID b) =
  let (x0, x1, x2, x3) = D.toWords a
      (y0, y1, y2, y3) = D.toWords b
   in UUID $
        D.fromWords
          (x0 + y0)
          (retainVersion 4 (x1 + y1))
          (retainVariant 2 (x2 + y2))
          (x3 + y3)

-- | Tell the version number of a 'D.UUID' value.
version :: D.UUID -> Word32
version u =
  let (_, x, _, _) = D.toWords u
   in (x .&. 0x0000F000) `shiftR` 12

-- Internal:

retainVersion :: Word32 -> Word32 -> Word32
retainVersion v x = (x .&. 0xFFFF0FFF) .|. (v `shiftL` 12)

retainVariant :: Word32 -> Word32 -> Word32
retainVariant v x = (x .&. 0x3FFFFFFF) .|. (v `shiftL` 30)
