-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.TeamStore.Env where

import Data.Range
import Imports
import Proto.TeamEvents qualified as E
import Wire.API.Team.Feature
import Galley.Types.Teams (FeatureDefaults)
import Wire.API.Team.Member (HardTruncationLimit)

data TeamStoreEnv = TeamStoreEnv
  { fanoutLimit :: Range 1 HardTruncationLimit Int32,
    legalholdDefaults :: FeatureDefaults LegalholdConfig,
    enqueueTeamEvent :: Maybe (E.TeamEvent -> IO ())
  }
