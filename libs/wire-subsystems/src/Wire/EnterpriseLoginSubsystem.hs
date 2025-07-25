{-# LANGUAGE TemplateHaskell #-}

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
