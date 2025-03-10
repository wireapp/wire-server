{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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

module Arbitraries where

import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder qualified as LT
import Data.Text.Lazy.Builder.Int qualified as LT
import Data.UUID hiding (fromString)
import Data.ZAuth.Token
import Imports
import Sodium.Crypto.Sign
import Test.Tasty.QuickCheck

instance Arbitrary (Token (Access ActualUser)) where
  arbitrary = Token <$> arbitrary <*> ((\h -> h {typ = A}) <$> arbitrary) <*> arbitrary

instance Arbitrary (Token (User ActualUser)) where
  arbitrary = Token <$> arbitrary <*> ((\h -> h {typ = U}) <$> arbitrary) <*> arbitrary

instance Arbitrary (Token Bot) where
  arbitrary = Token <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Token Provider) where
  arbitrary = Token <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Token (Access LHUser)) where
  arbitrary = Token <$> arbitrary <*> ((\h -> h {typ = LA}) <$> arbitrary) <*> arbitrary

instance Arbitrary (Token (User LHUser)) where
  arbitrary = Token <$> arbitrary <*> ((\h -> h {typ = LU}) <$> arbitrary) <*> arbitrary

instance Arbitrary Header where
  arbitrary =
    Header
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

arbitraryClientId :: Gen (Maybe Text)
arbitraryClientId =
  liftArbitrary $ fmap toClientId arbitrary
  where
    toClientId :: Word64 -> Text
    toClientId = LT.toStrict . LT.toLazyText . LT.hexadecimal

instance Arbitrary (Access t) where
  arbitrary = Access <$> arbitrary <*> arbitraryClientId <*> arbitrary

instance Arbitrary (User t) where
  arbitrary = User <$> arbitrary <*> arbitraryClientId <*> arbitrary

instance Arbitrary Bot where
  arbitrary = Bot <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Provider where
  arbitrary = Provider <$> arbitrary

instance Arbitrary ByteString where
  arbitrary = fromString <$> arbitrary `suchThat` notElem '.'

instance Arbitrary Signature where
  arbitrary = Signature <$> arbitrary

instance Arbitrary Type where
  arbitrary = elements [A, U, LA, LU]

instance Arbitrary Tag where
  arbitrary = pure S

instance Bounded UUID where
  minBound = nil
  maxBound = fromJust $ fromASCIIBytes "ffffffff-ffff-4fff-ffff-ffffffffffff"

instance Arbitrary UUID where
  arbitrary = arbitraryBoundedRandom
