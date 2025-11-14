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

module Wire.MockInterpreters.EnterpriseLoginSubsystem where

import Imports
import Polysemy
import Wire.API.EnterpriseLogin
import Wire.EnterpriseLoginSubsystem

enterpriseLoginSubsystemTestInterpreter :: Maybe DomainRegistration -> InterpreterFor EnterpriseLoginSubsystem r
enterpriseLoginSubsystemTestInterpreter constMbGuardResult =
  interpret \case
    LockDomain _ -> undefined
    UnlockDomain _ -> undefined
    PreAuthorizeDomain _ -> undefined
    UnAuthorizeDomain _ -> undefined
    UpdateDomainRegistration _ _ -> undefined
    DeleteDomain _ -> undefined
    GetDomainRegistration _ -> pure $ mkDomainRegistrationResponse <$> constMbGuardResult
    UpdateDomainRedirect {} -> undefined
    UpdateTeamInvite {} -> undefined
    GetDomainRegistrationPublic _ -> undefined
    CreateDomainVerificationChallenge _ -> undefined
    VerifyChallenge {} -> undefined
    AuthorizeTeam {} -> undefined
    GetRegisteredDomains {} -> undefined
    DeleteTeamDomain {} -> undefined
