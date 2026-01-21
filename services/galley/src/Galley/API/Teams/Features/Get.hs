{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

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

module Galley.API.Teams.Features.Get
  ( getFeature,
    getFeatureInternal,
    getAllTeamFeaturesForServer,
    getAllTeamFeaturesForTeam,
    getAllTeamFeaturesForUser,
    getSingleFeatureForUser,
    GetFeatureConfig (..),
    getFeatureForTeam,
    guardSecondFactorDisabled,
    DoAuth (..),
    featureEnabledForTeam,
    toTeamStatus,
  )
where

import Control.Error (hush)
import Data.Id
import Data.SOP
import Data.Tagged
import Galley.API.Util
import Galley.Effects
import Imports
import Polysemy
import Polysemy.Error
import Wire.API.Conversation (cnvmTeam)
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti qualified as Multi
import Wire.API.Team.Feature
import Wire.ConversationStore as ConversationStore
import Wire.FeaturesConfigSubsystem
import Wire.FeaturesConfigSubsystem.Types
import Wire.TeamFeatureStore
import Wire.TeamStore qualified as TeamStore
import Wire.TeamSubsystem (TeamSubsystem)
import Wire.TeamSubsystem qualified as TeamSubsystem

data DoAuth = DoAuth UserId | DontDoAuth

getFeatureInternal ::
  ( GetFeatureConfig cfg,
    Member (ErrorS 'TeamNotFound) r,
    Member TeamStore r,
    Member FeaturesConfigSubsystem r
  ) =>
  TeamId ->
  Sem r (LockableFeature cfg)
getFeatureInternal tid = do
  assertTeamExists tid
  getFeatureForTeam tid

toTeamStatus :: TeamId -> LockableFeature cfg -> Multi.TeamStatus cfg
toTeamStatus tid feat = Multi.TeamStatus tid feat.status

getTeamAndCheckMembership ::
  ( Member TeamStore r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TeamNotFound) r,
    Member TeamSubsystem r
  ) =>
  UserId ->
  Sem r (Maybe TeamId)
getTeamAndCheckMembership uid = do
  mTid <- TeamStore.getOneUserTeam uid
  for_ mTid $ \tid -> do
    zusrMembership <- TeamSubsystem.internalGetTeamMember uid tid
    void $ maybe (throwS @'NotATeamMember) pure zusrMembership
    assertTeamExists tid
  pure mTid

getAllTeamFeatures ::
  forall r.
  ( Member TeamFeatureStore r,
    Member FeaturesConfigSubsystem r,
    GetFeatureConfigEffects r
  ) =>
  TeamId ->
  Sem r AllTeamFeatures
getAllTeamFeatures tid = do
  features <- getAllDbFeatures tid
  defFeatures <- getAllTeamFeaturesForServer
  hsequence' $ hcliftA2 (Proxy @(GetAllFeaturesForServerConstraints r)) compute defFeatures features
  where
    compute ::
      (GetFeatureConfig p) =>
      LockableFeature p ->
      DbFeature p ->
      (Sem r :.: LockableFeature) p
    compute defFeature feat = Comp $ computeFeature tid defFeature feat

getAllTeamFeaturesForUser ::
  forall r.
  ( Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TeamNotFound) r,
    Member TeamFeatureStore r,
    Member TeamStore r,
    Member TeamSubsystem r,
    Member FeaturesConfigSubsystem r,
    GetFeatureConfigEffects r
  ) =>
  UserId ->
  Sem r AllTeamFeatures
getAllTeamFeaturesForUser uid = do
  mTid <- getTeamAndCheckMembership uid
  case mTid of
    Nothing -> hsequence' $ hcpure (Proxy @(GetAllTeamFeaturesForUserConstraints r)) $ Comp $ getFeatureForUser uid
    Just tid -> getAllTeamFeatures tid

getSingleFeatureForUser ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TeamNotFound) r,
    Member TeamStore r,
    Member TeamSubsystem r,
    Member FeaturesConfigSubsystem r
  ) =>
  UserId ->
  Sem r (LockableFeature cfg)
getSingleFeatureForUser uid = do
  mTid <- getTeamAndCheckMembership uid
  getFeatureForTeamUser @_ @cfg uid mTid

-- | If second factor auth is enabled, make sure that end-points that don't support it, but
-- should, are blocked completely.  (This is a workaround until we have 2FA for those
-- end-points as well.)
--
-- This function exists to resolve a cyclic dependency.
guardSecondFactorDisabled ::
  forall r.
  ( Member (ErrorS 'AccessDenied) r,
    Member TeamStore r,
    Member ConversationStore r,
    Member FeaturesConfigSubsystem r
  ) =>
  UserId ->
  ConvId ->
  Sem r ()
guardSecondFactorDisabled uid cid = do
  mTid <- fmap hush . runError @() $ do
    convData <- ConversationStore.getConversationMetadata cid >>= note ()
    tid <- note () convData.cnvmTeam
    mapError (unTagged @'TeamNotFound @()) $ assertTeamExists tid
    pure tid

  tf <- getFeatureForTeamUser @_ @SndFactorPasswordChallengeConfig uid mTid
  case tf.status of
    FeatureStatusDisabled -> pure ()
    FeatureStatusEnabled -> throwS @'AccessDenied

featureEnabledForTeam ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    Member (ErrorS 'TeamNotFound) r,
    Member TeamStore r,
    Member FeaturesConfigSubsystem r
  ) =>
  TeamId ->
  Sem r Bool
featureEnabledForTeam tid =
  (==) FeatureStatusEnabled
    . (.status)
    <$> getFeatureInternal @cfg tid
