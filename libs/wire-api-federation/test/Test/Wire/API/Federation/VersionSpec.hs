-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Federation.VersionSpec where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Set qualified as Set
import Imports
import Test.Hspec
import Wire.API.Federation.Version

spec :: Spec
spec = do
  describe "mostRecentTuple" $ do
    let mostRecent = mostRecentTuple Just
    -- FUTUREWORK: once we have more Version values, we may want to add some tests here.
    it "[..] + [] = null" $ do
      mostRecent (pure allVersions) (Set.fromList []) `shouldBe` Nothing
    it "[0] + [1] = null" $ do
      mostRecent (pure $ VersionRange V0 (Just V1)) (Set.fromList []) `shouldBe` Nothing
    it "[1] + [0, 1] = 1" $ do
      fmap snd (mostRecent (pure $ VersionRange V1 Nothing) (Set.fromList [0, 1])) `shouldBe` Just V1
    it "[0] + [0, 1] = 0" $ do
      fmap snd (mostRecent (pure $ VersionRange V0 (Just V1)) (Set.fromList [0, 1])) `shouldBe` Just V0
    it "[..] + [1] = 1" $ do
      fmap snd (mostRecent (VersionRange V0 (Just V1) :| [VersionRange V1 Nothing]) (Set.fromList [1])) `shouldBe` Just V1
