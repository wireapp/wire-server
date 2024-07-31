{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-uni-patterns #-}

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

module Test.Galley.Types where

import Galley.Types.Teams
import Imports
import Test.Galley.Roundtrip (testRoundTrip)
import Test.QuickCheck qualified as QC
import Test.Tasty
import Test.Tasty.QuickCheck
import Wire.API.Team.Feature as Public

tests :: TestTree
tests = testGroup "Tests" [testRoundTrip @FeatureFlags]

instance Arbitrary FeatureFlags where
  arbitrary =
    FeatureFlags
      <$> QC.elements [minBound ..]
      <*> QC.elements [minBound ..]
      <*> QC.elements [minBound ..]
      -- the default lock status is implicitly added on deserialization and ignored on serialization, therefore we need to fix it to the default here
      -- we will be able to remove this once the lock status is explicitly included in the config
      <*> fmap (fmap unlocked) arbitrary
      <*> fmap unlocked arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> fmap (fmap unlocked) arbitrary
      <*> arbitrary
      <*> fmap (fmap unlocked) arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> fmap (fmap unlocked) arbitrary
    where
      unlocked :: ImplicitLockStatus a -> ImplicitLockStatus a
      unlocked = ImplicitLockStatus . Public.setLockStatus Public.LockStatusUnlocked . _unImplicitLockStatus
