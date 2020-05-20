{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Arbitrary
  ( Arbitrary (..),
    GenericUniform (..),
    listOf',
    list1Of',
    setOf',
    mapOf',
  )
where

import qualified Codec.MIME.Type as MIME
import qualified Data.Aeson as Aeson
import qualified Data.Currency as Currency
import qualified Data.HashMap.Strict as HashMap
import Data.ISO3166_CountryCodes (CountryCode)
import Data.LanguageCodes (ISO639_1 (..))
import Data.List1 (List1, list1)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Generic.Random as Generic
import Generic.Random (listOf')
import Imports
import Test.QuickCheck
import Test.QuickCheck.Instances ()

-- | This type can be used with @DerivingVia@ to generically derive an instance
-- for the 'Arbitrary' typeclass.
--
-- Each constructor will appear equally often (@Uniform@ distribution).
-- For each field, values will be generated using 'arbitrary', unless it's a
-- list, in which case a smarter generater will be used to keep the size of
-- generated examples more manageable (see 'Generic.list1'').
--
-- Other list- or map-like fields don't get this special treatment and might
-- make it preferrable to decrease runtime of property tests by writing a manual
-- instance, e.g. using 'list1Of'' or 'mapOf''.
newtype GenericUniform a = GenericUniform {getGenericUniform :: a}

instance
  (Generic.GArbitrary Generic.SizedOptsDef a, Generic.GUniformWeight a) =>
  Arbitrary (GenericUniform a)
  where
  arbitrary = GenericUniform <$> Generic.genericArbitraryRec @a Generic.uniform

list1Of' :: Gen a -> Gen (List1 a)
list1Of' g = list1 <$> g <*> Generic.listOf' g

setOf' :: Ord a => Gen a -> Gen (Set a)
setOf' g = Set.fromList <$> Generic.listOf' g

mapOf' :: Ord k => Gen k -> Gen v -> Gen (Map k v)
mapOf' genK genV = Map.fromList <$> Generic.listOf' (liftA2 (,) genK genV)

--------------------------------------------------------------------------------
-- orphan instances for types from Hackage

-- just a stub, but better than no instance
instance Arbitrary MIME.Type where
  arbitrary = pure (MIME.Type (MIME.Image "png") [])

instance Arbitrary Currency.Alpha where
  arbitrary = genEnumBounded

instance Arbitrary ISO639_1 where
  arbitrary = genEnumBounded

-- | <https://github.com/HugoDaniel/iso639/pull/4>
deriving stock instance Bounded ISO639_1

instance Arbitrary CountryCode where
  arbitrary = genEnumBounded

instance Arbitrary Aeson.Value where
  arbitrary = oneof [genBaseCase, genObject, genArray]
    where
      genObject =
        Aeson.Object . HashMap.fromList
          <$> listOf' (liftA2 (,) arbitrary genBaseCase)
      genArray =
        Aeson.Array . foldMap pure
          <$> listOf' genBaseCase
      genBaseCase =
        oneof
          [ pure Aeson.Null,
            Aeson.String <$> arbitrary,
            Aeson.Number <$> arbitrary,
            Aeson.Bool <$> arbitrary
          ]

genEnumBounded :: (Enum a, Bounded a) => Gen a
genEnumBounded = elements [minBound ..]
