{-# LANGUAGE TemplateHaskell #-}

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

module Wire.TeamSubsystem where

import Data.Id
import Data.LegalHold
import Data.Map qualified as Map
import Data.Qualified
import Data.Range
import Data.Singletons (Demote, Sing, SingKind, fromSing)
import Imports
import Polysemy
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Team.LegalHold (UserLegalHoldStatusResponse)
import Wire.API.Team.Member
import Wire.API.Team.Member.Error
import Wire.API.Team.Member.Info (TeamMemberInfoList)

data PermissionCheckArgs teamAssociation where
  PermissionCheckArgs ::
    forall k (p :: k) teamAssociation.
    ( SingKind k,
      IsPerm teamAssociation (Demote k)
    ) =>
    Sing p ->
    Maybe teamAssociation ->
    PermissionCheckArgs teamAssociation

data TeamSubsystem m a where
  InternalGetTeamMember :: UserId -> TeamId -> TeamSubsystem m (Maybe TeamMember)
  InternalGetTeamMembersWithLimit :: TeamId -> Maybe (Range 1 HardTruncationLimit Int32) -> TeamSubsystem m TeamMemberList
  InternalSelectTeamMembers :: TeamId -> [UserId] -> TeamSubsystem m [TeamMember]
  InternalSelectTeamMemberInfos :: TeamId -> [UserId] -> TeamSubsystem m TeamMemberInfoList
  InternalGetTeamAdmins :: TeamId -> TeamSubsystem m TeamMemberList
  InternalGetOneUserTeam :: UserId -> TeamSubsystem m (Maybe TeamId)
  InternalFinalizeDeleteTeam :: Local UserId -> Maybe ConnId -> TeamId -> TeamSubsystem m ()
  GetUserStatus ::
    Local UserId ->
    TeamId ->
    UserId ->
    TeamSubsystem m UserLegalHoldStatusResponse
  GetTeamMembersForFanout ::
    TeamId ->
    TeamSubsystem m TeamMemberList
  AssertTeamExists ::
    TeamId ->
    TeamSubsystem m ()
  GetLHStatusForUsers ::
    [UserId] ->
    TeamSubsystem m [(UserId, UserLegalHoldStatus)]
  GetLHStatus ::
    Maybe TeamId ->
    UserId ->
    TeamSubsystem m UserLegalHoldStatus

makeSem ''TeamSubsystem

assertOnTeam ::
  ( Member (ErrorS 'NotATeamMember) r,
    Member TeamSubsystem r
  ) =>
  UserId ->
  TeamId ->
  Sem r ()
assertOnTeam uid tid =
  internalGetTeamMember uid tid >>= \case
    Nothing -> throwS @'NotATeamMember
    Just _ -> pure ()

-- | If a team member is not given throw 'notATeamMember'; if the given team
-- member does not have the given permission, throw 'operationDenied'.
-- Otherwise, return the team member.
permissionCheck ::
  ( IsPerm teamAssociation perm,
    ( Member (ErrorS OperationDenied) r,
      Member (ErrorS 'NotATeamMember) r
    )
  ) =>
  perm ->
  Maybe teamAssociation ->
  Sem r teamAssociation
-- FUTUREWORK: factor `noteS` out of this function.
permissionCheck p = \case
  Just m -> do
    if m `hasPermission` p
      then pure m
      else throwS @OperationDenied
  -- FUTUREWORK: factor `noteS` out of this function.
  Nothing -> throwS @'NotATeamMember

-- | Same as 'permissionCheck', but for a statically known permission.
permissionCheckS ::
  forall teamAssociation perm (p :: perm) r.
  ( SingKind perm,
    IsPerm teamAssociation (Demote perm),
    ( Member (ErrorS (PermError p)) r,
      Member (ErrorS 'NotATeamMember) r
    )
  ) =>
  Sing p ->
  Maybe teamAssociation ->
  Sem r teamAssociation
permissionCheckS p =
  \case
    Just m -> do
      if m `hasPermission` fromSing p
        then pure m
        else throwS @(PermError p)
    -- FUTUREWORK: factor `noteS` out of this function.
    Nothing -> throwS @'NotATeamMember

data ConsentGiven = ConsentGiven | ConsentNotGiven
  deriving (Eq, Ord, Show)

consentGiven :: UserLegalHoldStatus -> ConsentGiven
consentGiven = \case
  UserLegalHoldDisabled -> ConsentGiven
  UserLegalHoldPending -> ConsentGiven
  UserLegalHoldEnabled -> ConsentGiven
  UserLegalHoldNoConsent -> ConsentNotGiven

checkConsent ::
  (Member TeamSubsystem r) =>
  Map UserId TeamId ->
  UserId ->
  Sem r ConsentGiven
checkConsent teamsOfUsers other = do
  consentGiven <$> getLHStatus (Map.lookup other teamsOfUsers) other
