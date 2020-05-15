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

module Brig.Types.Test.Arbitrary
  ( module Wire.API.Arbitrary,
  )
where

import Brig.Types.Team.LegalHold
import Brig.Types.User
import Data.String.Conversions (cs)
import Imports
import Test.QuickCheck
import Wire.API.Arbitrary

instance Arbitrary ExcludedPrefix where
  arbitrary = ExcludedPrefix <$> arbitrary <*> arbitrary

instance Arbitrary PhonePrefix where
  arbitrary = do
    digits <- take 8 <$> listOf1 (elements ['0' .. '9'])
    pure . PhonePrefix . cs $ "+" <> digits

instance Arbitrary LegalHoldClientRequest where
  arbitrary =
    LegalHoldClientRequest
      <$> arbitrary
      <*> arbitrary

instance Arbitrary LegalHoldService where
  arbitrary = LegalHoldService <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
