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

module Galley.Cassandra.LegalHold (isTeamLegalholdWhitelisted) where

import Cassandra
import Data.Id
import Galley.Data.Queries as Q
import Galley.Types.Teams
import Imports

isTeamLegalholdWhitelisted :: FeatureLegalHold -> TeamId -> Client Bool
isTeamLegalholdWhitelisted FeatureLegalHoldDisabledPermanently _ = pure False
isTeamLegalholdWhitelisted FeatureLegalHoldDisabledByDefault _ = pure False
isTeamLegalholdWhitelisted FeatureLegalHoldWhitelistTeamsAndImplicitConsent tid =
  isJust <$> (runIdentity <$$> retry x5 (query1 Q.selectLegalHoldWhitelistedTeam (params LocalQuorum (Identity tid))))
