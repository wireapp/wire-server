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

module Wire.API.Team.Size
  ( TeamSize (..),
    teamSizeTotal,
    updateTeamSize,
  )
where

import Control.Lens ((?~))
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import Numeric.Natural
import Test.QuickCheck (arbitrarySizedNatural)
import Wire.API.User.Search
import Wire.Arbitrary

data TeamSize = TeamSize
  { regulars :: Natural,
    apps :: Natural
  }
  deriving (Show, Eq)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema TeamSize)

-- | Total team members (regulars + apps).
teamSizeTotal :: TeamSize -> Natural
teamSizeTotal ts = ts.regulars + ts.apps

-- Increase or decrease a team size component, depending on user type.

-- If the result of a decrease is <0, it is set to 1 (regulars) or 0
-- (apps).  This handles corner cases where ES reports lower numbers
-- from the past.
updateTeamSize :: UserTypeFilter -> TeamSize -> Int -> TeamSize
updateTeamSize = go
  where
    go :: UserTypeFilter -> TeamSize -> Int -> TeamSize
    go UserTypeFilterRegular (TeamSize rs as) n = TeamSize (upd 1 rs n) as
    go UserTypeFilterApp (TeamSize rs as) n = TeamSize rs (upd 0 as n)

    upd :: Int -> Natural -> Int -> Natural
    upd low n i = fromIntegral . max low $ fromIntegral n + i

instance ToSchema TeamSize where
  schema =
    objectWithDocModifier (description ?~ "Team member counts broken down by user type.") $
      fromTeamSize .= tripleSchema `withParser` validate
    where
      fromTeamSize :: TeamSize -> (Natural, Natural, Maybe Natural)
      fromTeamSize ts = (ts.regulars, ts.apps, Just (teamSizeTotal ts))
      tripleSchema :: ObjectSchema SwaggerDoc (Natural, Natural, Maybe Natural)
      tripleSchema =
        (,,)
          <$> (\(r, _, _) -> r) .= fieldWithDocModifier "teamSizeRegulars" (description ?~ "Number of regular users in team.") schema
          <*> (\(_, a, _) -> a) .= fieldWithDocModifier "teamSizeApps" (description ?~ "Number of apps in team.") schema
          <*> (\(_, _, t) -> t) .= maybe_ (optFieldWithDocModifier "teamSize" (description ?~ "Total team members (teamSizeRegulars + teamSizeApps).") schema)
      validate :: (Natural, Natural, Maybe Natural) -> A.Parser TeamSize
      validate (r, a, Nothing) = pure TeamSize {regulars = r, apps = a}
      validate (r, a, Just t)
        | r + a == t = pure TeamSize {regulars = r, apps = a}
        | otherwise = fail $ "teamSize (" <> show t <> ") != regulars + apps (" <> show (r + a) <> ")"

instance Arbitrary TeamSize where
  arbitrary = TeamSize <$> arbitrarySizedNatural <*> arbitrarySizedNatural
