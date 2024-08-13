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

module Galley.API.LegalHold.Team
  ( isLegalHoldEnabledForTeam,
    computeLegalHoldFeatureStatus,
    assertLegalHoldEnabledForTeam,
    ensureNotTooLargeToActivateLegalHold,
    teamSizeBelowLimit,
  )
where

import Data.Default
import Data.Id
import Data.Range
import Galley.Effects
import Galley.Effects.BrigAccess
import Galley.Effects.LegalHoldStore qualified as LegalHoldData
import Galley.Effects.TeamFeatureStore
import Galley.Effects.TeamStore
import Galley.Types.Teams as Team
import Imports
import Polysemy
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Team.Feature
import Wire.API.Team.Size

assertLegalHoldEnabledForTeam ::
  forall r.
  ( Member LegalHoldStore r,
    Member TeamStore r,
    Member TeamFeatureStore r,
    Member (ErrorS 'LegalHoldNotEnabled) r
  ) =>
  TeamId ->
  Sem r ()
assertLegalHoldEnabledForTeam tid =
  unlessM (isLegalHoldEnabledForTeam tid) $
    throwS @'LegalHoldNotEnabled

computeLegalHoldFeatureStatus ::
  ( Member TeamStore r,
    Member LegalHoldStore r
  ) =>
  TeamId ->
  DbFeature LegalholdConfig ->
  Sem r FeatureStatus
computeLegalHoldFeatureStatus tid dbFeature =
  getLegalHoldFlag >>= \case
    FeatureLegalHoldDisabledPermanently -> pure FeatureStatusDisabled
    FeatureLegalHoldDisabledByDefault ->
      pure (applyDbFeature dbFeature def).status
    FeatureLegalHoldWhitelistTeamsAndImplicitConsent -> do
      wl <- LegalHoldData.isTeamLegalholdWhitelisted tid
      pure $ if wl then FeatureStatusEnabled else FeatureStatusDisabled

isLegalHoldEnabledForTeam ::
  forall r.
  ( Member LegalHoldStore r,
    Member TeamStore r,
    Member TeamFeatureStore r
  ) =>
  TeamId ->
  Sem r Bool
isLegalHoldEnabledForTeam tid = do
  dbFeature <- getDbFeature tid
  status <- computeLegalHoldFeatureStatus tid dbFeature
  pure $ status == FeatureStatusEnabled

ensureNotTooLargeToActivateLegalHold ::
  ( Member BrigAccess r,
    Member (ErrorS 'CannotEnableLegalHoldServiceLargeTeam) r,
    Member TeamStore r
  ) =>
  TeamId ->
  Sem r ()
ensureNotTooLargeToActivateLegalHold tid = do
  (TeamSize teamSize) <- getSize tid
  unlessM (teamSizeBelowLimit (fromIntegral teamSize)) $
    throwS @'CannotEnableLegalHoldServiceLargeTeam

teamSizeBelowLimit :: (Member TeamStore r) => Int -> Sem r Bool
teamSizeBelowLimit teamSize = do
  limit <- fromIntegral . fromRange <$> fanoutLimit
  let withinLimit = teamSize <= limit
  getLegalHoldFlag >>= \case
    FeatureLegalHoldDisabledPermanently -> pure withinLimit
    FeatureLegalHoldDisabledByDefault -> pure withinLimit
    FeatureLegalHoldWhitelistTeamsAndImplicitConsent ->
      -- unlimited, see docs of 'ensureNotTooLargeForLegalHold'
      pure True
