module Wire.LegalHold where

import Data.Default (def)
import Data.Id (TeamId)
import Galley.Types.Teams
import Imports
import Polysemy
import Polysemy.Input (Input, input)
import Wire.API.Team.Feature
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
