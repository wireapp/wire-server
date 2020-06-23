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
import Data.Coerce (coerce)
import qualified Data.Currency as Currency
import qualified Data.HashMap.Strict as HashMap
import Data.ISO3166_CountryCodes (CountryCode)
import Data.LanguageCodes (ISO639_1 (..))
import Data.List1 (List1, list1)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics (Rep)
import Generic.Random (listOf', (:+) ((:+)))
import qualified Generic.Random as Generic
import Imports
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import qualified Test.QuickCheck.Arbitrary as QC
import Test.QuickCheck.Gen (Gen, oneof)
import Test.QuickCheck.Instances ()

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
    'Generic.Sized
    (Generic.Gen1 [] :+ Generic.Gen1 List1 :+ ())

customSizedOpts :: CustomSizedOpts
customSizedOpts =
  Generic.setGenerators
    (Generic.Gen1 listOf' :+ Generic.Gen1 list1Of' :+ ())
    Generic.sizedOpts

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

deriving via (GenericUniform Currency.Alpha) instance Arbitrary Currency.Alpha

deriving instance Generic ISO639_1

deriving via (GenericUniform ISO639_1) instance Arbitrary ISO639_1

-- | <https://github.com/HugoDaniel/iso639/pull/4>
deriving stock instance Bounded ISO639_1

deriving via (GenericUniform CountryCode) instance Arbitrary CountryCode

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
