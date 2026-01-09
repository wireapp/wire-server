{-# LANGUAGE TypeOperators #-}

module Galley.API.Teams.Features.Interpreter where

import Control.Error (hush)
import Control.Lens
import Data.Id
import Data.Qualified (Local, tUnqualified)
import Data.SOP
import Data.Tagged (unTagged)
import Galley.API.LegalHold.Team (computeLegalHoldFeatureStatus)
import Galley.Effects
import Galley.Effects.TeamFeatureStore
import Galley.Options
import Galley.Types.Teams
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.Conversation (cnvmTeam)
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Team.Feature
import Wire.BrigAPIAccess (getAccountConferenceCallingConfigClient)
import Wire.ConversationStore (ConversationStore)
import Wire.ConversationStore qualified as ConversationStore
import Wire.FeaturesConfigCompute
import Wire.FeaturesConfigRead
import Wire.FeaturesConfigRead.Types
import Wire.TeamStore qualified as TeamStore
import Wire.TeamSubsystem (TeamSubsystem)
import Wire.TeamSubsystem qualified as TeamSubsystem

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
    Member BrigAPIAccess r,
    Member TeamSubsystem r,
    Member ConversationStore r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'AccessDenied) r
  ) =>
  Sem (FeaturesConfigRead : r) a ->
  Sem r a
runFeaturesConfigRead = interpret $ \case
  GetFeature uid tid -> do
    void $ TeamSubsystem.internalGetTeamMember uid tid >>= noteS @'NotATeamMember
    doGetFeatureForTeam tid
  GetFeatureInternal tid -> do
    doAssertTeamExists tid
    doGetFeatureForTeam tid
  GetFeatureForTeam tid ->
    doGetFeatureForTeam tid
  GetFeatureForServer ->
    doGetFeatureForServer
  GetFeatureForUser uid ->
    doGetFeatureForUser uid
  GetFeatureForTeamUser uid mTid ->
    doGetFeatureForTeamUser uid mTid
  GetSingleFeatureForUser uid -> do
    mTid <- doGetTeamAndCheckMembership uid
    doGetFeatureForTeamUser uid mTid
  GetAllTeamFeaturesForTeamMember luid tid -> do
    void $ TeamSubsystem.internalGetTeamMember (tUnqualified luid) tid >>= noteS @'NotATeamMember
    doGetAllTeamFeatures tid
  GetAllTeamFeaturesForTeam tid ->
    doGetAllTeamFeatures tid
  GetAllTeamFeaturesForServer ->
    doGetAllTeamFeaturesForServer
  GetAllTeamFeaturesForUser uid -> do
    mTid <- doGetTeamAndCheckMembership uid
    case mTid of
      Nothing -> hsequence' $ hcpure (Proxy @(GetAllTeamFeaturesForUserConstraints r)) $ Comp $ doGetFeatureForUser uid
      Just tid -> doGetAllTeamFeatures tid
  GuardSecondFactorDisabled uid cid -> do
    mTid <- fmap hush . runError @() $ do
      convData <- ConversationStore.getConversationMetadata cid >>= note ()
      tid <- note () convData.cnvmTeam
      mapError (unTagged @'TeamNotFound @()) $ doAssertTeamExists tid
      pure tid
    tf <- doGetFeatureForTeamUser @SndFactorPasswordChallengeConfig uid mTid
    case tf.status of
      FeatureStatusDisabled -> pure ()
      FeatureStatusEnabled -> throwS @'AccessDenied
  FeatureEnabledForTeam tid ->
    (==) FeatureStatusEnabled
      . (.status)
      <$> (doAssertTeamExists tid >> doGetFeatureForTeam tid)

-- Internal helpers

doGetFeatureForTeam :: forall cfg r. (GetFeatureConfig cfg, Member TeamFeatureStore r, Member (Input Opts) r, Member FeaturesConfigCompute r) => TeamId -> Sem r (LockableFeature cfg)
doGetFeatureForTeam tid = do
  dbFeature <- getDbFeature tid
  defFeature <- doGetFeatureForServer
  computeFeature tid defFeature dbFeature

doGetFeatureForServer :: forall cfg r. (GetFeatureDefaults (FeatureDefaults cfg), NpProject cfg Features, Member (Input Opts) r) => Sem r (LockableFeature cfg)
doGetFeatureForServer = inputs $ view (settings . featureFlags . to (featureDefaults @cfg))

doGetFeatureForUser :: forall cfg r. (GetFeatureConfig cfg, Member (Input Opts) r, Member FeaturesConfigCompute r) => UserId -> Sem r (LockableFeature cfg)
doGetFeatureForUser uid = do
  defFeat <- doGetFeatureForServer
  getFeatureForUser uid defFeat

doGetFeatureForTeamUser :: forall cfg r. (GetFeatureConfig cfg, Member (Input Opts) r, Member TeamFeatureStore r, Member FeaturesConfigCompute r) => UserId -> Maybe TeamId -> Sem r (LockableFeature cfg)
doGetFeatureForTeamUser uid Nothing = doGetFeatureForUser uid
doGetFeatureForTeamUser _uid (Just tid) = doGetFeatureForTeam tid

doGetAllTeamFeatures :: forall r. (Member (Input Opts) r, Member LegalHoldStore r, Member TeamFeatureStore r, Member TeamStore r, Member (Input (FeatureDefaults LegalholdConfig)) r, Member FeaturesConfigCompute r) => TeamId -> Sem r AllTeamFeatures
doGetAllTeamFeatures tid = do
  features <- getAllDbFeatures tid
  defFeatures <- doGetAllTeamFeaturesForServer
  hsequence' $ hcliftA2 (Proxy @(GetAllFeaturesForServerConstraints r)) compute defFeatures features
  where
    compute :: forall p. (GetFeatureConfig p) => LockableFeature p -> DbFeature p -> (Sem r :.: LockableFeature) p
    compute defFeature feat = Comp $ computeFeature tid defFeature feat

doGetAllTeamFeaturesForServer :: forall r. (Member (Input Opts) r) => Sem r AllTeamFeatures
doGetAllTeamFeaturesForServer =
  hsequence' $
    hcpure (Proxy @GetFeatureConfig) $
      Comp doGetFeatureForServer

doAssertTeamExists :: (Member TeamStore r, Member (ErrorS 'TeamNotFound) r) => TeamId -> Sem r ()
doAssertTeamExists tid =
  void $ TeamStore.getTeam tid >>= noteS @'TeamNotFound

doGetTeamAndCheckMembership :: (Member TeamStore r, Member (ErrorS 'NotATeamMember) r, Member (ErrorS 'TeamNotFound) r, Member TeamSubsystem r) => UserId -> Sem r (Maybe TeamId)
doGetTeamAndCheckMembership uid = do
  mTid <- TeamStore.getOneUserTeam uid
  for_ mTid $ \tid -> do
    zusrMembership <- TeamSubsystem.internalGetTeamMember uid tid
    void $ maybe (throwS @'NotATeamMember) pure zusrMembership
    doAssertTeamExists tid
  pure mTid
