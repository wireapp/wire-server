{-# LANGUAGE UndecidableSuperClasses #-}
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
    getFeatureForServer,
    guardSecondFactorDisabled,
    DoAuth (..),
    featureEnabledForTeam,
    toTeamStatus,
  )
where

import Control.Error (hush)
import Control.Lens
import Data.Default
import Data.Id
import Data.Kind
import Data.Qualified (Local, tUnqualified)
import Data.SOP
import Data.Tagged
import Galley.API.LegalHold.Team
import Galley.API.Util
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
import Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti qualified as Multi
import Wire.API.Team.Feature
import Wire.BrigAPIAccess (getAccountConferenceCallingConfigClient)
import Wire.ConversationStore as ConversationStore
import Wire.TeamStore qualified as TeamStore
import Wire.TeamSubsystem (TeamSubsystem)
import Wire.TeamSubsystem qualified as TeamSubsystem
import Wire.FeaturesConfigRead
import Wire.FeaturesConfigRead.Types

data DoAuth = DoAuth UserId | DontDoAuth

getFeature ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    Member FeaturesConfigRead r,
    Member (ErrorS 'NotATeamMember) r,
    Member TeamSubsystem r
  ) =>
  UserId ->
  TeamId ->
  Sem r (LockableFeature cfg)
getFeature uid tid = do
  void $ TeamSubsystem.internalGetTeamMember uid tid >>= noteS @'NotATeamMember
  Wire.FeaturesConfigRead.getFeatureForTeam @cfg tid

getFeatureInternal ::
  ( GetFeatureConfig cfg,
    Member FeaturesConfigRead r,
    Member (ErrorS 'TeamNotFound) r,
    Member TeamStore r
  ) =>
  TeamId ->
  Sem r (LockableFeature cfg)
getFeatureInternal tid = do
  assertTeamExists tid
  Wire.FeaturesConfigRead.getFeatureForTeam tid

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

getAllTeamFeaturesForTeam ::
  forall r.
  ( Member (ErrorS 'NotATeamMember) r,
    Member TeamSubsystem r,
    Member FeaturesConfigRead r
  ) =>
  Local UserId ->
  TeamId ->
  Sem r AllTeamFeatures
getAllTeamFeaturesForTeam luid tid = do
  void $ TeamSubsystem.internalGetTeamMember (tUnqualified luid) tid >>= noteS @'NotATeamMember
  Wire.FeaturesConfigRead.getAllTeamFeaturesForTeam tid

getAllTeamFeaturesForServer ::
  forall r.
  (Member FeaturesConfigRead r) =>
  Sem r AllTeamFeatures
getAllTeamFeaturesForServer =
  Wire.FeaturesConfigRead.getAllTeamFeaturesForServer

getAllTeamFeaturesForUser ::
  forall r.
  ( Member FeaturesConfigRead r
  ) =>
  UserId ->
  Sem r AllTeamFeatures
getAllTeamFeaturesForUser uid =
  Wire.FeaturesConfigRead.getAllTeamFeaturesForUser uid

getSingleFeatureForUser ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    Member FeaturesConfigRead r
  ) =>
  UserId ->
  Sem r (LockableFeature cfg)
getSingleFeatureForUser uid =
  Wire.FeaturesConfigRead.getSingleFeatureForUser @cfg uid

getFeatureForTeamUser ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    Member FeaturesConfigRead r
  ) =>
  UserId ->
  Maybe TeamId ->
  Sem r (LockableFeature cfg)
getFeatureForTeamUser uid mTid =
  Wire.FeaturesConfigRead.getFeatureForTeamUser @cfg uid mTid

getFeatureForServer ::
  forall cfg r.
  ( GetFeatureDefaults (FeatureDefaults cfg),
    NpProject cfg Features,
    Member (Input Opts) r
  ) =>
  Sem r (LockableFeature cfg)
getFeatureForServer = inputs $ view (settings . featureFlags . to (featureDefaults @cfg))

assertTeamExists :: (Member TeamStore r, Member (ErrorS 'TeamNotFound) r) => TeamId -> Sem r ()
assertTeamExists tid =
  void $ TeamStore.getTeam tid >>= noteS @'TeamNotFound

-------------------------------------------------------------------------------
-- GetFeatureConfig instances moved to Wire.FeaturesConfigRead.Types

-- | If second factor auth is enabled, make sure that end-points that don't support it, but
-- should, are blocked completely.  (This is a workaround until we have 2FA for those
-- end-points as well.)
--
-- This function exists to resolve a cyclic dependency.
guardSecondFactorDisabled ::
  forall r.
  ( Member FeaturesConfigRead r,
    Member (ErrorS 'AccessDenied) r,
    Member TeamStore r,
    Member ConversationStore r
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

  tf <- getFeatureForTeamUser @SndFactorPasswordChallengeConfig uid mTid
  case tf.status of
    FeatureStatusDisabled -> pure ()
    FeatureStatusEnabled -> throwS @'AccessDenied

featureEnabledForTeam ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    Member (ErrorS 'TeamNotFound) r,
    Member TeamStore r,
    Member FeaturesConfigRead r
  ) =>
  TeamId ->
  Sem r Bool
featureEnabledForTeam tid =
  (==) FeatureStatusEnabled
    . (.status)
    <$> getFeatureInternal @cfg tid
