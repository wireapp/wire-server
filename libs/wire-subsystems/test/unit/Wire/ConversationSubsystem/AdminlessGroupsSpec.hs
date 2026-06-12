{-# LANGUAGE OverloadedStrings #-}

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
-- You should have received a copy of the GNU Affero General Public License
-- along with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.ConversationSubsystem.AdminlessGroupsSpec where

import Data.Domain (Domain (..))
import Data.Id
import Data.List.NonEmpty qualified as NE
import Data.Qualified
import Data.UUID qualified as UUID
import Imports
import Test.Hspec
import Wire.API.Team.Feature (PreventAdminlessGroupsPromotionStrategy (..))
import Wire.API.User.Profile (Name (..))
import Wire.ConversationSubsystem.AdminlessGroups (selectAutopromotionCandidate)

spec :: Spec
spec = describe "AdminlessGroups" do
  let candidates = [charlie, bob1, bob2, alice]

  it "selects the alphabetically first eligible member" do
    NE.toList (selectAutopromotionCandidate 0 PromotionStrategyAlphabetical (NE.fromList candidates))
      `shouldBe` [fst alice]

  it "selects a deterministic member for the random strategy" do
    -- The seed is injected by the caller, so the helper remains pure and the
    -- test can pin down the expected result.
    NE.toList (selectAutopromotionCandidate 2 PromotionStrategyRandom (NE.fromList candidates))
      `shouldBe` [fst bob2]

  it "returns all members for the all strategy in stable order" do
    NE.toList (selectAutopromotionCandidate 0 PromotionStrategyAll (NE.fromList candidates))
      `shouldBe` (fmap fst [alice, bob1, bob2, charlie])

qualified :: Domain -> Text -> Qualified UserId
qualified d uid = Qualified (mkUserId uid) d

mkUserId :: Text -> UserId
mkUserId uid = Id (fromJust (UUID.fromText uid))

alice, bob1, bob2, charlie :: (Qualified UserId, Name)
alice =
  (qualified domain "00000000-0000-0000-0000-000000000001", Name "alice")
bob1 =
  (qualified domain "00000000-0000-0000-0000-000000000002", Name "bob")
bob2 =
  (qualified domain "00000000-0000-0000-0000-000000000004", Name "bob")
charlie =
  (qualified domain "00000000-0000-0000-0000-000000000003", Name "charlie")

domain :: Domain
domain = Domain "wire.com"
