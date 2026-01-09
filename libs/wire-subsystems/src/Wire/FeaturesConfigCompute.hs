{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Wire.FeaturesConfigCompute where

import Data.Id (TeamId, UserId)
import Polysemy
import Wire.API.Team.Feature

data FeaturesConfigCompute m a where
  ResolveGenericDbFeature :: (IsFeatureConfig cfg) => TeamId -> LockableFeature cfg -> DbFeature cfg -> FeaturesConfigCompute m (LockableFeature cfg)
  ResolveServerFeature :: forall cfg m. (GetFeatureConfig cfg) => FeaturesConfigCompute m (LockableFeature cfg)
  ResolveLegalhold :: TeamId -> LockableFeature LegalholdConfig -> DbFeature LegalholdConfig -> FeaturesConfigCompute m (LockableFeature LegalholdConfig)
  ResolveConferenceCalling :: TeamId -> LockableFeature ConferenceCallingConfig -> DbFeature ConferenceCallingConfig -> FeaturesConfigCompute m (LockableFeature ConferenceCallingConfig)
  ResolveConferenceCallingUser :: UserId -> FeaturesConfigCompute m (LockableFeature ConferenceCallingConfig)
  ResolveExposeInvitationURLsToTeamAdmin :: TeamId -> LockableFeature ExposeInvitationURLsToTeamAdminConfig -> DbFeature ExposeInvitationURLsToTeamAdminConfig -> FeaturesConfigCompute m (LockableFeature ExposeInvitationURLsToTeamAdminConfig)

makeSem ''FeaturesConfigCompute
