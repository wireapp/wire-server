{-# LANGUAGE TypeOperators #-}

module Galley.API.Teams.Features.Interpreter where

import Control.Lens
import Data.SOP
import Galley.API.LegalHold.Team (computeLegalHoldFeatureStatus)
import Galley.Effects
import Galley.Effects.TeamFeatureStore
import Galley.Options
import Galley.Types.Teams
import Galley.API.Teams.Features.Get (getFeatureForServer, getTeamAndCheckMembership, assertTeamExists)
import Polysemy
import Polysemy.Input
import Wire.API.Team.Feature
import Wire.FeaturesConfigCompute
import Wire.FeaturesConfigRead
import Wire.FeaturesConfigRead.Types
import Wire.BrigAPIAccess (getAccountConferenceCallingConfigClient)
import qualified Wire.TeamStore as TeamStore
import Imports

runFeaturesConfigCompute ::
  ( Member (Input Opts) r,
    Member LegalHoldStore r,
    Member TeamStore r,
    Member (Input (FeatureDefaults LegalholdConfig)) r,
    Member BrigAPIAccess r
  ) =>
  Sem (FeaturesConfigCompute : r) a ->
  Sem r a
runFeaturesConfigCompute = interpret $ \case
  ResolveGenericDbFeature _tid defFeature dbFeature ->
    pure $ resolveDbFeature defFeature dbFeature
  ResolveLegalhold tid defFeature dbFeature -> do
    status <- computeLegalHoldFeatureStatus tid dbFeature
    pure $ defFeature {status = status}
  ResolveConferenceCalling _tid defFeature dbFeature ->
    pure $
      let feat = applyDbFeature dbFeature defFeature {status = FeatureStatusEnabled}
       in case feat.lockStatus of
            LockStatusLocked -> defFeature {lockStatus = LockStatusLocked}
            LockStatusUnlocked -> feat
  ResolveConferenceCallingUser uid -> do
    feat <- getAccountConferenceCallingConfigClient uid
    pure $ withLockStatus (def @(LockableFeature ConferenceCallingConfig)).lockStatus feat
  ResolveExposeInvitationURLsToTeamAdmin tid defFeature dbFeature -> do
    allowList <- inputs $ view (settings . exposeInvitationURLsTeamAllowlist . to (fromMaybe []))
    let teamAllowed = tid `elem` allowList
        lockStatus = if teamAllowed then LockStatusUnlocked else LockStatusLocked
    pure $ resolveDbFeature defFeature (dbFeatureLockStatus lockStatus <> dbFeature)

runFeaturesConfigRead ::
  forall r a.
  ( Member (Input Opts) r,
    Member TeamFeatureStore r,
    Member FeaturesConfigCompute r,
    Member TeamStore r,
    Member (Input (FeatureDefaults LegalholdConfig)) r,
    Member LegalHoldStore r,
    Member BrigAPIAccess r
  ) =>
  Sem (FeaturesConfigRead : r) a ->
  Sem r a
runFeaturesConfigRead = interpret $ \case
  GetFeatureForTeam tid -> do
    dbFeature <- getDbFeature tid
    defFeature <- getFeatureForServer
    computeFeature tid defFeature dbFeature
  GetFeatureForServer ->
    getFeatureForServer
  GetFeatureForUser uid -> do
    defFeat <- getFeatureForServer
    getFeatureForUser uid defFeat
  GetFeatureForTeamUser uid mTid ->
    case mTid of
      Nothing -> do
        defFeat <- getFeatureForServer
        getFeatureForUser uid defFeat
      Just tid -> do
        dbFeature <- getDbFeature tid
        defFeature <- getFeatureForServer
        computeFeature tid defFeature dbFeature
  GetAllTeamFeaturesForTeam tid -> do
    features <- getAllDbFeatures tid
    defFeatures <- getAllTeamFeaturesForServerEffect
    hsequence' $ hcliftA2 (Proxy @(GetAllFeaturesForServerConstraints r)) compute defFeatures features
    where
      compute ::
        forall p. (GetFeatureConfig p) =>
        LockableFeature p ->
        DbFeature p ->
        (Sem r :.: LockableFeature) p
      compute defFeature feat = Comp $ computeFeature tid defFeature feat
  GetAllTeamFeaturesForServer ->
    getAllTeamFeaturesForServerEffect
  GetAllTeamFeaturesForUser uid -> do
    mTid <- TeamStore.getOneUserTeam uid
    case mTid of
      Nothing -> hsequence' $ hcpure (Proxy @(GetAllTeamFeaturesForUserConstraints r)) $ Comp $ do
        defFeat <- getFeatureForServer
        getFeatureForUser uid defFeat
      Just tid -> do
        features <- getAllDbFeatures tid
        defFeatures <- getAllTeamFeaturesForServerEffect
        hsequence' $ hcliftA2 (Proxy @(GetAllFeaturesForServerConstraints r)) compute defFeatures features
        where
          compute :: forall p. (GetFeatureConfig p) => LockableFeature p -> DbFeature p -> (Sem r :.: LockableFeature) p
          compute defFeature feat = Comp $ computeFeature tid defFeature feat

getAllTeamFeaturesForServerEffect ::
  forall r.
  (Member (Input Opts) r) =>
  Sem r AllTeamFeatures
getAllTeamFeaturesForServerEffect =
  hsequence' $
    hcpure (Proxy @GetFeatureConfig) $
      Comp getFeatureForServer
