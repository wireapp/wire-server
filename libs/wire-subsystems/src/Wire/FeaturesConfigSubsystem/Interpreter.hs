{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Wire.FeaturesConfigSubsystem.Interpreter where

import Control.Error (hush)
import Data.Aeson.Types qualified as A
import Data.Id
import Data.Qualified (tUnqualified)
import Data.SOP
import Data.Tagged
import Data.Text.Lazy qualified as LT
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.Conversation (ConversationMetadata (..))
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Team.Feature
import Wire.API.Team.FeatureFlags
import Wire.BrigAPIAccess (BrigAPIAccess)
import Wire.ConversationStore qualified as ConversationStore
import Wire.FeaturesConfigSubsystem
import Wire.FeaturesConfigSubsystem.Types
import Wire.FeaturesConfigSubsystem.Utils
import Wire.LegalHoldStore (LegalHoldStore)
import Wire.TeamFeatureStore
import Wire.TeamFeatureStore.Error (TeamFeatureStoreError (..))
import Wire.TeamStore qualified as TeamStore
import Wire.TeamSubsystem (TeamSubsystem)
import Wire.TeamSubsystem qualified as TeamSubsystem

runFeaturesConfigSubsystem ::
  forall r a.
  ( Member TeamFeatureStore r,
    Member TeamSubsystem r,
    Member TeamStore.TeamStore r,
    Member ConversationStore.ConversationStore r,
    Member (Error TeamFeatureStoreError) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'AccessDenied) r,
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
  GuardSecondFactorDisabled uid cid ->
    guardSecondFactorDisabledImpl uid cid
  FeatureEnabledForTeam (Proxy :: Proxy cfg) tid ->
    featureEnabledForTeamImpl @cfg tid
  GetAllTeamFeaturesForUser uid ->
    getAllTeamFeaturesForUserImpl uid
  GetSingleFeatureForUser uid ->
    getSingleFeatureForUserImpl uid
  GetFeatureInternal tid ->
    getFeatureInternalImpl tid

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

getFeatureInternalImpl ::
  ( GetFeatureConfig cfg,
    Member TeamFeatureStore r,
    Member LegalHoldStore r,
    Member TeamSubsystem r,
    Member BrigAPIAccess r,
    Member (Input FeatureFlags) r,
    Member (Input (FeatureDefaults LegalholdConfig)) r,
    Member (Input ExposeInvitationURLsAllowlist) r,
    Member (Error TeamFeatureStoreError) r
  ) =>
  TeamId ->
  Sem r (LockableFeature cfg)
getFeatureInternalImpl tid = do
  TeamSubsystem.assertTeamExists tid
  getFeatureForTeamImpl tid

getTeamAndCheckMembership ::
  ( Member TeamStore.TeamStore r,
    Member (ErrorS 'NotATeamMember) r,
    Member TeamSubsystem r
  ) =>
  UserId ->
  Sem r (Maybe TeamId)
getTeamAndCheckMembership uid = do
  mTid <- TeamStore.getOneUserTeam uid
  for_ mTid $ \tid -> do
    zusrMembership <- TeamSubsystem.internalGetTeamMember uid tid
    void $ maybe (throwS @'NotATeamMember) pure zusrMembership
    TeamSubsystem.assertTeamExists tid
  pure mTid

getAllTeamFeatures ::
  forall r.
  ( Member TeamFeatureStore r,
    Member LegalHoldStore r,
    Member BrigAPIAccess r,
    Member (Input FeatureFlags) r,
    Member (Input (FeatureDefaults LegalholdConfig)) r,
    Member (Input ExposeInvitationURLsAllowlist) r,
    Member (Error TeamFeatureStoreError) r
  ) =>
  TeamId ->
  Sem r AllTeamFeatures
getAllTeamFeatures tid = getAllTeamFeaturesImpl tid

getAllTeamFeaturesForUserImpl ::
  forall r.
  ( Member (ErrorS 'NotATeamMember) r,
    Member TeamStore.TeamStore r,
    Member TeamSubsystem r,
    Member TeamFeatureStore r,
    Member (Error TeamFeatureStoreError) r,
    GetFeatureConfigEffects r
  ) =>
  UserId ->
  Sem r AllTeamFeatures
getAllTeamFeaturesForUserImpl uid = do
  mTid <- getTeamAndCheckMembership uid
  case mTid of
    Nothing -> hsequence' $ hcpure (Proxy @(GetAllTeamFeaturesForUserConstraints r)) $ Comp $ getFeatureForUser uid
    Just tid -> getAllTeamFeatures tid

getSingleFeatureForUserImpl ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    Member (ErrorS 'NotATeamMember) r,
    Member (Error TeamFeatureStoreError) r,
    Member TeamStore.TeamStore r,
    Member TeamSubsystem r,
    Member TeamFeatureStore r,
    Member BrigAPIAccess r,
    Member LegalHoldStore r,
    Member (Input FeatureFlags) r,
    Member (Input (FeatureDefaults LegalholdConfig)) r,
    Member (Input ExposeInvitationURLsAllowlist) r
  ) =>
  UserId ->
  Sem r (LockableFeature cfg)
getSingleFeatureForUserImpl uid = do
  mTid <- getTeamAndCheckMembership uid
  getFeatureForTeamUserImpl @cfg uid mTid

-- | If second factor auth is enabled, make sure that end-points that don't support it, but
-- should, are blocked completely.  (This is a workaround until we have 2FA for those
-- end-points as well.)
--
-- This function exists to resolve a cyclic dependency.
guardSecondFactorDisabledImpl ::
  forall r.
  ( Member (ErrorS 'AccessDenied) r,
    Member (Error TeamFeatureStoreError) r,
    Member ConversationStore.ConversationStore r,
    Member TeamSubsystem r,
    Member TeamFeatureStore r,
    Member BrigAPIAccess r,
    Member LegalHoldStore r,
    Member (Input FeatureFlags) r,
    Member (Input (FeatureDefaults LegalholdConfig)) r,
    Member (Input ExposeInvitationURLsAllowlist) r
  ) =>
  UserId ->
  ConvId ->
  Sem r ()
guardSecondFactorDisabledImpl uid cid = do
  mTid <- fmap hush . runError @() $ do
    convData <- ConversationStore.getConversationMetadata cid >>= note ()
    tid <- note () convData.cnvmTeam
    mapError (unTagged @'TeamNotFound @()) $ TeamSubsystem.assertTeamExists tid
    pure tid

  tf <- getFeatureForTeamUserImpl @SndFactorPasswordChallengeConfig uid mTid
  case tf.status of
    FeatureStatusDisabled -> pure ()
    FeatureStatusEnabled -> throwS @'AccessDenied

featureEnabledForTeamImpl ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    Member TeamSubsystem r,
    Member TeamFeatureStore r,
    Member LegalHoldStore r,
    Member BrigAPIAccess r,
    Member (Input FeatureFlags) r,
    Member (Input (FeatureDefaults LegalholdConfig)) r,
    Member (Input ExposeInvitationURLsAllowlist) r,
    Member (Error TeamFeatureStoreError) r
  ) =>
  TeamId ->
  Sem r Bool
featureEnabledForTeamImpl tid =
  (==) FeatureStatusEnabled
    . (.status)
    <$> getFeatureInternalImpl @cfg tid
