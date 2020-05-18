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
  )
where

import qualified Codec.MIME.Type as MIME
import qualified Data.Aeson as Aeson
import qualified Data.Currency as Currency
import qualified Data.HashMap.Strict as HashMap
import Data.ISO3166_CountryCodes (CountryCode)
import Data.LanguageCodes (ISO639_1 (..))
import qualified Generic.Random as Generic
import Imports
import Test.QuickCheck
import Test.QuickCheck.Instances ()

newtype GenericUniform a = GenericUniform {getGenericUniform :: a}

-- FUTUREWORK: have a working generic implementation that keeps nested lists
-- from blowing up the test duration.
-- Most likely by scaling the items' size depending on length of the list,
-- like in 'Generic.genericArbitraryRec'.
instance
  (Generic.GArbitrary Generic.UnsizedOpts a, Generic.GUniformWeight a) =>
  Arbitrary (GenericUniform a)
  where
  arbitrary = GenericUniform <$> Generic.genericArbitraryU

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

-- TODO: where does this belong?
instance Arbitrary Aeson.Value where
  arbitrary = oneof [genBaseCase, genObject, genArray]
    where
      genObject =
        Aeson.Object . HashMap.fromList
          <$> listOf (liftA2 (,) arbitrary genBaseCase)
      genArray =
        Aeson.Array . foldMap pure
          <$> listOf genBaseCase
      genBaseCase =
        oneof
          [ pure Aeson.Null,
            Aeson.String <$> arbitrary,
            Aeson.Number <$> arbitrary,
            Aeson.Bool <$> arbitrary
          ]

genEnumBounded :: (Enum a, Bounded a) => Gen a
genEnumBounded = elements [minBound ..]
