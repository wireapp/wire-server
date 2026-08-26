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
  )
where

import Control.Lens ((?~))
import Data.Aeson qualified as A
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import Numeric.Natural
import Test.QuickCheck (arbitrarySizedNatural)
import Wire.Arbitrary

data TeamSize = TeamSize
  { teamSize :: Natural,
    apps :: Natural,
    collaborators :: Natural
  }
  deriving (Show, Eq)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema TeamSize)

instance ToSchema TeamSize where
  schema =
    objectWithDocModifier (description ?~ "Number of team members (paid seats, always regular users), appps, collaborators.") $
      TeamSize
        <$> (.teamSize) .= field "teamSize" schema
        <*> (.apps) .= field "apps" schema
        <*> (.collaborators) .= field "collaborators" schema

instance Arbitrary TeamSize where
  arbitrary = TeamSize <$> arbitrarySizedNatural <*> arbitrarySizedNatural <*> arbitrarySizedNatural
