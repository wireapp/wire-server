{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Wire.FeaturesConfigSubsystem.Interpreter where

import Data.Aeson.Types qualified as A
import Data.Id
import Data.Qualified (tUnqualified)
import Data.SOP
import Data.Text.Lazy qualified as LT
import Galley.Types.Teams
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Team.Feature
import Wire.FeaturesConfigSubsystem
import Wire.FeaturesConfigSubsystem.Types
import Wire.FeaturesConfigSubsystem.Utils
import Wire.TeamFeatureStore
import Wire.TeamFeatureStore.Error (TeamFeatureStoreError (..))
import Wire.TeamSubsystem (TeamSubsystem)
import Wire.TeamSubsystem qualified as TeamSubsystem

runFeaturesConfigSubsystem ::
  forall r a.
  ( Member TeamFeatureStore r,
    Member TeamSubsystem r,
    Member (Error TeamFeatureStoreError) r,
    Member (ErrorS 'NotATeamMember) r,
    GetFeatureConfigEffects r
  ) =>
  Sem (FeaturesConfigSubsystem : r) a ->
  Sem r a
runFeaturesConfigSubsystem = interpret $ \case
  GetDbFeatureRawInternal tid -> getDbFeatureRawInternalImpl tid
  GetFeature uid tid -> do
    void $ TeamSubsystem.internalGetTeamMember uid tid >>= noteS @'NotATeamMember
    getFeatureForTeamImpl tid
  GetFeatureForTeam tid ->
    getFeatureForTeamImpl tid
  GetFeatureForServer ->
    resolveServerFeature
  GetFeatureForTeamUser uid mTid ->
    getFeatureForTeamUserImpl uid mTid
  GetAllTeamFeaturesForTeamMember luid tid -> do
    void $ TeamSubsystem.internalGetTeamMember (tUnqualified luid) tid >>= noteS @'NotATeamMember
    getAllTeamFeaturesImpl tid
  GetAllTeamFeaturesForTeam tid ->
    getAllTeamFeaturesImpl tid
  GetAllTeamFeaturesForServer ->
    getAllTeamFeaturesForServerImpl

-- Internal helpers

getFeatureForTeamImpl ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    Member TeamFeatureStore r,
    Member (Error TeamFeatureStoreError) r,
    GetFeatureConfigEffects r
  ) =>
  TeamId ->
  Sem r (LockableFeature cfg)
getFeatureForTeamImpl tid = do
  dbFeature <- getDbFeatureRawInternalImpl tid
  defFeature <- resolveServerFeature
  computeFeature tid defFeature dbFeature

getFeatureForTeamUserImpl ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    Member TeamFeatureStore r,
    Member (Error TeamFeatureStoreError) r,
    GetFeatureConfigEffects r
  ) =>
  UserId ->
  Maybe TeamId ->
  Sem r (LockableFeature cfg)
getFeatureForTeamUserImpl uid Nothing = getFeatureForUser uid
getFeatureForTeamUserImpl _uid (Just tid) = getFeatureForTeamImpl tid

getAllTeamFeaturesImpl ::
  forall r.
  ( Member TeamFeatureStore r,
    Member (Error TeamFeatureStoreError) r,
    GetFeatureConfigEffects r
  ) =>
  TeamId ->
  Sem r AllTeamFeatures
getAllTeamFeaturesImpl tid = do
  features <- getAllDbFeatures tid
  defFeatures <- getAllTeamFeaturesForServerImpl
  hsequence' $ hcliftA2 (Proxy @(GetAllFeaturesForServerConstraints r)) compute defFeatures features
  where
    compute :: forall p. (GetFeatureConfig p) => LockableFeature p -> K (Maybe DbFeaturePatch) p -> (Sem r :.: LockableFeature) p
    compute defFeature (K mPatch) = Comp $ do
      dbFeature <- fromMaybe mempty <$> traverse parseDbFeatureOrThrow mPatch
      computeFeature tid defFeature dbFeature

getAllTeamFeaturesForServerImpl :: forall r. (Member (Input FeatureFlags) r) => Sem r AllTeamFeatures
getAllTeamFeaturesForServerImpl =
  hsequence' $
    hcpure (Proxy @GetFeatureConfig) $
      Comp resolveServerFeature

getDbFeatureRawInternalImpl ::
  forall cfg r.
  ( IsFeatureConfig cfg,
    Member (Error TeamFeatureStoreError) r,
    Member TeamFeatureStore r
  ) =>
  TeamId -> Sem r (DbFeature cfg)
getDbFeatureRawInternalImpl tid =
  fromMaybe mempty <$> (getDbFeature @cfg tid >>= traverse parseDbFeatureOrThrow)

parseDbFeatureOrThrow ::
  forall cfg r.
  ( IsFeatureConfig cfg,
    Member (Error TeamFeatureStoreError) r
  ) =>
  DbFeaturePatch ->
  Sem r (DbFeature cfg)
parseDbFeatureOrThrow feat =
  mapError (TeamFeatureStoreErrorInternalError . LT.pack)
    . fromEither
    $ A.parseEither (const (parseDbFeature feat)) ()
