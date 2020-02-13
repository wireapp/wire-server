module Data.UUID.Tagged
  ( UUID,
    V4,
    Version (..),
    version,
    variant,
    addv4,
    subv4,
    unpack,
  )
where

import Data.Bits
import qualified Data.UUID as D
import qualified Data.UUID.V4 as D4
import Imports

-- | Versioned UUID.
newtype UUID v = UUID D.UUID deriving (Eq, Ord, Show)

instance NFData (UUID v) where rnf (UUID a) = seq a ()

class Version v where
  -- | Create a fresh versioned UUID.
  create :: IO (UUID v)

  -- | Try to turn a plain UUID into a versioned UUID.
  fromUUID :: D.UUID -> Maybe (UUID v)

data V4

instance Version V4 where
  create = UUID <$> D4.nextRandom
  fromUUID u = case version u of
    4 -> Just (UUID u)
    _ -> Nothing

-- | Extract the 'D.UUID' from a versioned UUID.
unpack :: UUID v -> D.UUID
unpack (UUID x) = x

-- | Add two @UUID V4@ values. This retains
-- variant and version information and adds
-- all other bits.
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

-- | Subtract two @UUID V4@ values (the second argument from the first).
-- This retains variant and version information and subtracts
-- all other bits.
subv4 :: UUID V4 -> UUID V4 -> UUID V4
subv4 (UUID a) (UUID b) =
  let (x0, x1, x2, x3) = D.toWords a
      (y0, y1, y2, y3) = D.toWords b
   in UUID $
        D.fromWords
          (x0 - y0)
          (retainVersion 4 (x1 - y1))
          (retainVariant 2 (x2 - y2))
          (x3 - y3)

-- | Tell the version number of a 'D.UUID' value.
version :: D.UUID -> Word32
version u =
  let (_, x, _, _) = D.toWords u
   in (x .&. 0x0000F000) `shiftR` 12

-- | Tell the variant of a 'D.UUID' value.
variant :: D.UUID -> Word32
variant u =
  let (_, _, x, _) = D.toWords u
   in (x .&. 0xC0000000) `shiftR` 30

-- Internal:

retainVersion :: Word32 -> Word32 -> Word32
retainVersion v x = (x .&. 0xFFFF0FFF) .|. (v `shiftL` 12)

retainVariant :: Word32 -> Word32 -> Word32
retainVariant v x = (x .&. 0x3FFFFFFF) .|. (v `shiftL` 30)
