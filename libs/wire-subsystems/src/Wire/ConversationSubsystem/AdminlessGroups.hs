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

module Wire.ConversationSubsystem.AdminlessGroups
  ( selectAutopromotionCandidate,
  )
where

import Data.Id (UserId)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Qualified
import Imports
import Wire.API.Team.Feature (PreventAdminlessGroupsPromotionStrategy (..))
import Wire.API.User.Profile (Name (..))

-- | Select the members that should be promoted when a team conversation would
-- otherwise become adminless.
--
-- The helper is kept pure on purpose. The caller can provide a random seed for
-- the random strategy, which makes the selection deterministic in tests while
-- leaving the actual source of randomness to the eventual wiring layer.
selectAutopromotionCandidate ::
  Word64 ->
  PreventAdminlessGroupsPromotionStrategy ->
  NonEmpty (Qualified UserId, Name) ->
  NonEmpty (Qualified UserId)
selectAutopromotionCandidate seed strategy candidates =
  case strategy of
    PromotionStrategyAlphabetical -> fst (NE.head sortedCandidates) :| []
    PromotionStrategyRandom ->
      (fst (pickBySeed sortedCandidates)) :| []
    PromotionStrategyAll -> fmap fst sortedCandidates
  where
    sortedCandidates :: NonEmpty (Qualified UserId, Name)
    sortedCandidates = NE.sortOn (\(quid, name) -> (name, qUnqualified quid)) candidates

    pickBySeed :: NonEmpty a -> a
    pickBySeed xs =
      let idx = seed `mod` fromIntegral (NE.length xs)
          x = lookup idx (zip [0 ..] (NE.toList xs))
       in fromMaybe (NE.head xs) x
