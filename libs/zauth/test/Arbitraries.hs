{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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

module Arbitraries where

import Control.Lens ((.~))
import Data.UUID hiding (fromString)
import Data.ZAuth.Token
import Imports
import Sodium.Crypto.Sign
import Test.Tasty.QuickCheck

instance Arbitrary (Token Access) where
  arbitrary = mkToken <$> arbitrary <*> ((typ .~ A) <$> arbitrary) <*> arbitrary

instance Arbitrary (Token User) where
  arbitrary = mkToken <$> arbitrary <*> ((typ .~ U) <$> arbitrary) <*> arbitrary

instance Arbitrary (Token Bot) where
  arbitrary = mkToken <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Token Provider) where
  arbitrary = mkToken <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Token LegalHoldAccess) where
  arbitrary = mkToken <$> arbitrary <*> ((typ .~ LA) <$> arbitrary) <*> arbitrary

instance Arbitrary (Token LegalHoldUser) where
  arbitrary = mkToken <$> arbitrary <*> ((typ .~ LU) <$> arbitrary) <*> arbitrary

instance Arbitrary Header where
  arbitrary =
    mkHeader
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary Access where
  arbitrary = mkAccess <$> arbitrary <*> arbitrary

instance Arbitrary User where
  arbitrary = mkUser <$> arbitrary <*> arbitrary

instance Arbitrary Bot where
  arbitrary = mkBot <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Provider where
  arbitrary = mkProvider <$> arbitrary

instance Arbitrary LegalHoldAccess where
  arbitrary = mkLegalHoldAccess <$> arbitrary <*> arbitrary

instance Arbitrary LegalHoldUser where
  arbitrary = mkLegalHoldUser <$> arbitrary <*> arbitrary

instance Arbitrary ByteString where
  arbitrary = fromString <$> arbitrary `suchThat` (not . any (== '.'))

instance Arbitrary Signature where
  arbitrary = Signature <$> arbitrary

instance Arbitrary Type where
  arbitrary = elements [A, U, LA, LU]

instance Arbitrary Tag where
  arbitrary = return S

instance Bounded UUID where
  minBound = nil
  maxBound = fromJust $ fromASCIIBytes "ffffffff-ffff-4fff-ffff-ffffffffffff"

instance Arbitrary UUID where
  arbitrary = arbitraryBoundedRandom
