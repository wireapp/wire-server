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

module Data.GenericEnum
  ( GenericEnum (..),
    GEnum (..),
  )
where

import Data.Kind (Type)
import GHC.Generics (Generic (..), K1 (..), M1 (..), U1 (..), V1, type (:*:) (..), type (:+:) (..))
import Imports

--------------------------------------------------------------------------------
-- Generic Bounded / Enum

-- | 'Bounded' and 'Enum' read off a type's 'Generic' representation, for use
-- with @DerivingVia@:
--
-- > data OAuthScope = FeatureConfigs OAuthTier | {- ... -} | Meetings OAuthTier
-- >   deriving (Bounded, Enum) via (GenericEnum OAuthScope)
--
-- @deriving Enum@ only covers types whose constructors are all
-- nullary, which @OAuthScope@ is not.  `GenericEnum` covers products
-- of 'Bounded' + 'Enum' fields as well, numbering sums in constructor
-- order and products lexicographically.  This is the same order
-- @deriving Ord@ uses, so @[minBound .. maxBound]@ comes out sorted.
newtype GenericEnum a = GenericEnum a

class GEnum (f :: Type -> Type) where
  -- | How many values @f@ has.
  gCard :: Int

  gToEnum :: Int -> f a
  gFromEnum :: f a -> Int

instance GEnum V1 where
  gCard = 0
  gToEnum i = error $ "GenericEnum: uninhabited type, toEnum " <> show i
  gFromEnum v = case v of {}

instance GEnum U1 where
  gCard = 1
  gToEnum _ = U1
  gFromEnum _ = 0

instance (GEnum f) => GEnum (M1 i c f) where
  gCard = gCard @f
  gToEnum = M1 . gToEnum
  gFromEnum = gFromEnum . unM1

instance (Bounded a, Enum a) => GEnum (K1 i a) where
  gCard = fromEnum (maxBound @a) - fromEnum (minBound @a) + 1
  gToEnum i = K1 (toEnum (i + fromEnum (minBound @a)))
  gFromEnum (K1 x) = fromEnum x - fromEnum (minBound @a)

instance (GEnum f, GEnum g) => GEnum (f :+: g) where
  gCard = gCard @f + gCard @g
  gToEnum i
    | i < gCard @f = L1 (gToEnum i)
    | otherwise = R1 (gToEnum (i - gCard @f))
  gFromEnum = \case
    L1 x -> gFromEnum x
    R1 y -> gCard @f + gFromEnum y

instance (GEnum f, GEnum g) => GEnum (f :*: g) where
  gCard = gCard @f * gCard @g
  gToEnum i = case i `divMod` gCard @g of
    (q, r) -> gToEnum q :*: gToEnum r
  gFromEnum (x :*: y) = gFromEnum x * gCard @g + gFromEnum y

instance (Generic a, GEnum (Rep a)) => Bounded (GenericEnum a) where
  minBound = GenericEnum . to $ gToEnum 0
  maxBound = GenericEnum . to $ gToEnum (gCard @(Rep a) - 1)

instance (Generic a, GEnum (Rep a)) => Enum (GenericEnum a) where
  fromEnum (GenericEnum x) = gFromEnum (from x)

  toEnum i
    | 0 <= i && i < gCard @(Rep a) = GenericEnum . to $ gToEnum i
    | otherwise = error $ "GenericEnum: toEnum out of range: " <> show i

  -- the class defaults for these two run off past 'maxBound'
  enumFrom x = enumFromTo x maxBound

  enumFromThen x y =
    enumFromThenTo x y $
      if fromEnum y >= fromEnum x then maxBound else minBound
