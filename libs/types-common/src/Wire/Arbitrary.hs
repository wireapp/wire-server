{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

module Wire.Arbitrary
  ( Arbitrary (..),
    GenericUniform (..),
    listOf',
    setOf',
    mapOf',
    generateExample,
  )
where

import Codec.MIME.Type qualified as MIME
import Data.Coerce (coerce)
import Data.Currency qualified as Currency
import Data.ISO3166_CountryCodes (CountryCode)
import Data.LanguageCodes (ISO639_1 (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC.Generics (Rep)
import Generic.Random (listOf', (:+) ((:+)))
import Generic.Random qualified as Generic
import Imports
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import Test.QuickCheck.Arbitrary qualified as QC
import Test.QuickCheck.Gen (Gen (MkGen))
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Random

-- | This type can be used with @DerivingVia@ to generically derive an instance
-- for the 'Arbitrary' typeclass.
--
-- Each constructor will appear equally often (a @Uniform@ distribution).
-- For each field, values will be generated using 'arbitrary', unless it's a
-- list '[]' or 'List1', in which case a smarter generator will be used to keep the
-- size of generated examples more manageable (see 'Generic.list'').
--
-- Other set- or map-like types don't get this special treatment and might
-- make it preferrable to decrease runtime of property tests by writing a manual
-- instance, e.g. using 'setOf'' or 'mapOf''.
newtype GenericUniform a = GenericUniform {getGenericUniform :: a}

instance
  ( Generic.GArbitrary CustomSizedOpts a,
    Generic.GUniformWeight a,
    QC.RecursivelyShrink (Rep a),
    QC.GSubterms (Rep a) a
  ) =>
  Arbitrary (GenericUniform a)
  where
  arbitrary = GenericUniform <$> Generic.genericArbitraryWith @CustomSizedOpts @a customSizedOpts Generic.uniform
  shrink = coerce (QC.genericShrink @a)

-- | We want plug in custom generators for all occurences of '[]' and 'List1'.
type CustomSizedOpts =
  Generic.Options
    'Generic.INCOHERENT
    'Generic.Sized
    (Generic.Gen1 [] :+ Generic.Gen1 NonEmpty :+ ())

customSizedOpts :: CustomSizedOpts
customSizedOpts =
  Generic.setGenerators
    (Generic.Gen1 listOf' :+ Generic.Gen1 nonEmptyListOf' :+ ())
    Generic.sizedOpts

nonEmptyListOf' :: Gen a -> Gen (NonEmpty a)
nonEmptyListOf' g = (:|) <$> g <*> listOf' g

setOf' :: (Ord a) => Gen a -> Gen (Set a)
setOf' g = Set.fromList <$> Generic.listOf' g

mapOf' :: (Ord k) => Gen k -> Gen v -> Gen (Map k v)
mapOf' genK genV = Map.fromList <$> Generic.listOf' (liftA2 (,) genK genV)

--------------------------------------------------------------------------------
-- orphan instances for types from Hackage

-- just a stub, but better than no instance
instance Arbitrary MIME.Type where
  arbitrary = pure (MIME.Type (MIME.Image "png") [])

deriving via (GenericUniform Currency.Alpha) instance Arbitrary Currency.Alpha

deriving instance Generic ISO639_1

deriving via (GenericUniform ISO639_1) instance Arbitrary ISO639_1

-- | <https://github.com/HugoDaniel/iso639/pull/4>
deriving stock instance Bounded ISO639_1

deriving via (GenericUniform CountryCode) instance Arbitrary CountryCode

-- | Use Arbitrary instance to generate an example to be used in swagger where
-- we cannot rely on swagger-ui to generate nice examples. So far, this is only
-- required for maps as swagger2 doesn't have a good way to specify the type of
-- keys.
generateExample :: (Arbitrary a) => a
generateExample =
  let (MkGen f) = arbitrary
   in f (mkQCGen 42) 42
