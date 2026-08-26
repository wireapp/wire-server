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
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.LegalHold where

import Data.Default (def)
import Data.Id (TeamId)
import Imports
import Polysemy
import Polysemy.Input (Input, input)
import Wire.API.Team.Feature
import Wire.API.Team.FeatureFlags
import Wire.LegalHoldStore qualified as LegalHoldData

computeLegalHoldFeatureStatus ::
  ( Member LegalHoldData.LegalHoldStore r,
    Member (Input (FeatureDefaults LegalholdConfig)) r
  ) =>
  TeamId ->
  DbFeature LegalholdConfig ->
  Sem r FeatureStatus
computeLegalHoldFeatureStatus tid dbFeature = do
  featureLegalHold <- input @(FeatureDefaults LegalholdConfig)
  case featureLegalHold of
    FeatureLegalHoldDisabledPermanently -> pure FeatureStatusDisabled
    FeatureLegalHoldDisabledByDefault ->
      pure (applyDbFeature dbFeature def).status
    FeatureLegalHoldWhitelistTeamsAndImplicitConsent -> do
      wl <- LegalHoldData.isTeamLegalholdWhitelisted tid
      pure $ if wl then FeatureStatusEnabled else FeatureStatusDisabled
