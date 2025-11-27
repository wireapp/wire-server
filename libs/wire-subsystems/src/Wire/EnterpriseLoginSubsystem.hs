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

module Wire.EnterpriseLoginSubsystem where

import Data.Domain
import Data.Id
import Data.Qualified
import Imports
import Polysemy
import Wire.API.EnterpriseLogin
import Wire.API.Routes.Public.Brig.DomainVerification

data EnterpriseLoginSubsystem m a where
  LockDomain :: Domain -> EnterpriseLoginSubsystem m ()
  UnlockDomain :: Domain -> EnterpriseLoginSubsystem m ()
  PreAuthorizeDomain :: Domain -> EnterpriseLoginSubsystem m ()
  UnAuthorizeDomain :: Domain -> EnterpriseLoginSubsystem m ()
  UpdateDomainRegistration :: Domain -> DomainRegistrationUpdate -> EnterpriseLoginSubsystem m ()
  DeleteDomain :: Domain -> EnterpriseLoginSubsystem m ()
  GetDomainRegistration :: Domain -> EnterpriseLoginSubsystem m (Maybe (DomainRegistrationResponse v))
  UpdateDomainRedirect ::
    Token ->
    Domain ->
    DomainRedirectConfigV9 ->
    EnterpriseLoginSubsystem m ()
  UpdateTeamInvite ::
    Local UserId ->
    Domain ->
    TeamInviteConfig ->
    EnterpriseLoginSubsystem m ()
  GetDomainRegistrationPublic ::
    GetDomainRegistrationRequest ->
    EnterpriseLoginSubsystem m (DomainRedirectResponse v)
  CreateDomainVerificationChallenge ::
    Domain ->
    EnterpriseLoginSubsystem m DomainVerificationChallenge
  VerifyChallenge ::
    Maybe (Local UserId) ->
    Domain ->
    ChallengeId ->
    Token ->
    EnterpriseLoginSubsystem m Token
  AuthorizeTeam :: Local UserId -> Domain -> DomainOwnershipToken -> EnterpriseLoginSubsystem m ()
  GetRegisteredDomains :: Local UserId -> TeamId -> EnterpriseLoginSubsystem m (RegisteredDomains v)
  DeleteTeamDomain :: Local UserId -> TeamId -> Domain -> EnterpriseLoginSubsystem m ()

makeSem ''EnterpriseLoginSubsystem
