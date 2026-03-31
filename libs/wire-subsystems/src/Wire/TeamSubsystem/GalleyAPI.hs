-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.TeamSubsystem.GalleyAPI where

import Data.LegalHold (UserLegalHoldStatus (..))
import Imports
import Polysemy
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Team.Feature (FeatureStatus (FeatureStatusEnabled), LockableFeature (..))
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.GalleyAPIAccess qualified as GalleyAPIAccess
import Wire.TeamSubsystem

interpretTeamSubsystemToGalleyAPI ::
  ( Member GalleyAPIAccess r,
    Member (ErrorS TeamMemberNotFound) r,
    Member (ErrorS TeamNotFound) r
  ) =>
  InterpreterFor TeamSubsystem r
interpretTeamSubsystemToGalleyAPI = interpret $ \case
  InternalGetTeamMember userId teamId -> GalleyAPIAccess.getTeamMember userId teamId
  InternalGetTeamMembersWithLimit teamId maxResults -> GalleyAPIAccess.getTeamMembersWithLimit teamId maxResults
  InternalSelectTeamMemberInfos teamId userIds -> GalleyAPIAccess.selectTeamMemberInfos teamId userIds
  InternalSelectTeamMembers teamId userIds -> GalleyAPIAccess.selectTeamMembers teamId userIds
  InternalGetTeamAdmins teamId -> GalleyAPIAccess.getTeamAdmins teamId
  InternalGetOneUserTeam userId -> GalleyAPIAccess.getTeamId userId
  InternalFinalizeDeleteTeam lusr mcon teamId -> GalleyAPIAccess.finalizeDeleteTeam lusr mcon teamId
  GetUserStatus lusr tid uid -> do
    GalleyAPIAccess.getTeamMember uid tid >>= \case
      Nothing -> throwS @'TeamMemberNotFound
      Just _ -> do
        GalleyAPIAccess.getUserLegalholdStatus lusr tid >>= \case
          Nothing -> throwS @'TeamNotFound
          Just status -> pure status
  GetTeamMembersForFanout tid ->
    GalleyAPIAccess.getTeamMembersWithLimit tid Nothing
  AssertTeamExists tid -> do
    void $ GalleyAPIAccess.getTeam tid
  GetLHStatusForUsers uids -> do
    for uids $ \uid -> do
      mteamId <- GalleyAPIAccess.getTeamId uid
      status <- case mteamId of
        Nothing -> pure UserLegalHoldDisabled
        Just tid -> do
          GalleyAPIAccess.getTeamMember uid tid >>= \case
            Nothing -> pure UserLegalHoldDisabled
            Just _ -> do
              LockableFeature {status} <- GalleyAPIAccess.getTeamLegalHoldStatus tid
              pure $
                if status == FeatureStatusEnabled
                  then UserLegalHoldEnabled
                  else UserLegalHoldDisabled
      pure (uid, status)
  GetLHStatus mtid uid -> do
    case mtid of
      Nothing -> pure UserLegalHoldDisabled
      Just tid -> do
        GalleyAPIAccess.getTeamMember uid tid >>= \case
          Nothing -> pure UserLegalHoldDisabled
          Just _ -> do
            LockableFeature {status} <- GalleyAPIAccess.getTeamLegalHoldStatus tid
            pure $
              if status == FeatureStatusEnabled
                then UserLegalHoldEnabled
                else UserLegalHoldDisabled
