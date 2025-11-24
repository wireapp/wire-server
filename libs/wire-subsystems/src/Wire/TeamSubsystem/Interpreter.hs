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

module Wire.TeamSubsystem.Interpreter where

import Control.Lens ((%~), (^.))
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (..))
import Imports
import Polysemy
import Wire.API.Team.HardTruncationLimit
import Wire.API.Team.Member
import Wire.API.Team.Member.Info (TeamMemberInfoList (TeamMemberInfoList))
import Wire.LegalHoldStore (LegalHoldStore)
import Wire.LegalHoldStore qualified as LH
import Wire.TeamStore (TeamStore)
import Wire.TeamStore qualified as E
import Wire.TeamSubsystem

interpretTeamSubsystem :: (Member TeamStore r, Member LegalHoldStore r) => InterpreterFor TeamSubsystem r
interpretTeamSubsystem = interpret $ \case
  InternalGetTeamMember uid tid -> do
    tms <- E.getTeamMemberTempName tid uid
    for tms $ \tm -> do
      hasImplicitConsent <- LH.isTeamLegalholdWhitelisted tid
      pure $ if hasImplicitConsent then grantImplicitConsent tm else tm
  InternalGetTeamMembers tid ->
    adjustMembersForImplicitConsent tid =<< E.getTeamMembersTempName tid
  InternalGetTeamMembersWithLimit tid maxResults -> do
    tmList <- E.getTeamMembersWithLimitTempName tid (fromMaybe hardTruncationLimitRange maxResults)
    ms <- adjustMembersForImplicitConsent tid (tmList ^. teamMembers)
    pure $ newTeamMemberList ms (tmList ^. teamMemberListType)
  InternalSelectTeamMemberInfos tid uids -> TeamMemberInfoList <$> E.selectTeamMemberInfos tid uids
  InternalSelectTeamMembers tid uids -> do
    tms <- E.selectTeamMembersTempName tid uids
    adjustMembersForImplicitConsent tid tms
  InternalGetTeamAdmins tid -> do
    admins <-
      E.getTeamAdmins tid
        >>= E.selectTeamMembersTempName tid
        >>= adjustMembersForImplicitConsent tid
    pure $ newTeamMemberList admins ListComplete

adjustMembersForImplicitConsent :: (Member LegalHoldStore r) => TeamId -> [TeamMember] -> Sem r [TeamMember]
adjustMembersForImplicitConsent tid ms = do
  hasImplicitConsent <- LH.isTeamLegalholdWhitelisted tid
  pure $ if hasImplicitConsent then map grantImplicitConsent ms else ms

grantImplicitConsent :: TeamMember -> TeamMember
grantImplicitConsent =
  legalHoldStatus %~ \case
    UserLegalHoldNoConsent -> UserLegalHoldDisabled
    UserLegalHoldDisabled -> UserLegalHoldDisabled
    UserLegalHoldPending -> UserLegalHoldPending
    UserLegalHoldEnabled -> UserLegalHoldEnabled
