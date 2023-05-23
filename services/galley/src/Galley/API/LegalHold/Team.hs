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
    assertLegalHoldEnabledForTeam,
    ensureNotTooLargeToActivateLegalHold,
    teamSizeBelowLimit,
  )
where

import Data.Id
import Data.Range
import Galley.Effects
import Galley.Effects.BrigAccess
import qualified Galley.Effects.LegalHoldStore as LegalHoldData
import qualified Galley.Effects.TeamFeatureStore as TeamFeatures
import Galley.Effects.TeamStore
import Galley.Types.Teams as Team
import Imports
import Polysemy
import Wire.API.Error
import Wire.API.Error.Galley
import qualified Wire.API.Team.Feature as Public
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

isLegalHoldEnabledForTeam ::
  forall r.
  ( Member LegalHoldStore r,
    Member TeamStore r,
    Member TeamFeatureStore r
  ) =>
  TeamId ->
  Sem r Bool
isLegalHoldEnabledForTeam tid = do
  getLegalHoldFlag >>= \case
    FeatureLegalHoldDisabledPermanently -> do
      pure False
    FeatureLegalHoldDisabledByDefault -> do
      statusValue <-
        Public.wssStatus <$$> TeamFeatures.getFeatureConfig Public.FeatureSingletonLegalholdConfig tid
      pure $ case statusValue of
        Just Public.FeatureStatusEnabled -> True
        Just Public.FeatureStatusDisabled -> False
        Nothing -> False
    FeatureLegalHoldWhitelistTeamsAndImplicitConsent ->
      LegalHoldData.isTeamLegalholdWhitelisted tid

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

teamSizeBelowLimit :: Member TeamStore r => Int -> Sem r Bool
teamSizeBelowLimit teamSize = do
  limit <- fromIntegral . fromRange <$> fanoutLimit
  let withinLimit = teamSize <= limit
  getLegalHoldFlag >>= \case
    FeatureLegalHoldDisabledPermanently -> pure withinLimit
    FeatureLegalHoldDisabledByDefault -> pure withinLimit
    FeatureLegalHoldWhitelistTeamsAndImplicitConsent ->
      -- unlimited, see docs of 'ensureNotTooLargeForLegalHold'
      pure True
