{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Wire.FeaturesConfigCompute.Interpreter where

import Data.Default
import Galley.Types.Teams
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.Team.Feature
import Wire.BrigAPIAccess (BrigAPIAccess, getAccountConferenceCallingConfigClient)
import Wire.FeaturesConfigCompute
import Wire.LegalHold
import Wire.LegalHoldStore

runFeaturesConfigCompute ::
  ( Member (Input FeatureFlags) r,
    Member (Input ExposeInvitationURLsAllowlist) r,
    Member LegalHoldStore r,
    Member (Input (FeatureDefaults LegalholdConfig)) r,
    Member BrigAPIAccess r
  ) =>
  Sem (FeaturesConfigCompute : r) a ->
  Sem r a
runFeaturesConfigCompute = interpret $ \case
  ResolveGenericDbFeature _tid defFeature dbFeature ->
    pure $ resolveDbFeature defFeature dbFeature
  ResolveServerFeature ->
    doResolveServerFeature
  ResolveLegalhold tid defFeature dbFeature ->
    setLockableFeatureStatus defFeature <$> computeLegalHoldFeatureStatus tid dbFeature
  ResolveConferenceCalling _tid defFeature dbFeature ->
    pure $
      let feat = applyDbFeature dbFeature defFeature {status = FeatureStatusEnabled}
       in case feat.lockStatus of
            LockStatusLocked -> setLockableFeatureLockStatus defFeature LockStatusLocked
            LockStatusUnlocked -> feat
  ResolveConferenceCallingUser uid -> do
    feat <- getAccountConferenceCallingConfigClient uid
    pure $ withLockStatus (def @(LockableFeature ConferenceCallingConfig)).lockStatus feat
  ResolveExposeInvitationURLsToTeamAdmin tid defFeature dbFeature -> do
    (ExposeInvitationURLsAllowlist allowList) <- inputs id
    let teamAllowed = tid `elem` allowList
        lockStatus = if teamAllowed then LockStatusUnlocked else LockStatusLocked
    pure $ resolveDbFeature defFeature (dbFeatureLockStatus lockStatus <> dbFeature)

doResolveServerFeature ::
  forall cfg r.
  ( GetFeatureDefaults (FeatureDefaults cfg),
    NpProject cfg Features,
    Member (Input FeatureFlags) r
  ) =>
  Sem r (LockableFeature cfg)
doResolveServerFeature = inputs $ featureDefaults @cfg
