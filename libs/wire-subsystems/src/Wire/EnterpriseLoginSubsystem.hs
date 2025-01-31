{-# LANGUAGE TemplateHaskell #-}

module Wire.EnterpriseLoginSubsystem where

import Data.Domain
import Data.Id
import Data.Qualified
import Imports
import Polysemy
import Text.Email.Parser
import Wire.API.EnterpriseLogin
import Wire.API.Routes.Public.Brig.DomainVerification
import Wire.Arbitrary

data InvitationFlow = ExistingUser | NewUser
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via GenericUniform InvitationFlow

data EnterpriseLoginSubsystem m a where
  LockDomain :: Domain -> EnterpriseLoginSubsystem m ()
  UnlockDomain :: Domain -> EnterpriseLoginSubsystem m ()
  PreAuthorizeDomain :: Domain -> EnterpriseLoginSubsystem m ()
  UnAuthorizeDomain :: Domain -> EnterpriseLoginSubsystem m ()
  UpdateDomainRegistration :: Domain -> DomainRegistrationUpdate -> EnterpriseLoginSubsystem m ()
  DeleteDomain :: Domain -> EnterpriseLoginSubsystem m ()
  GuardEmailDomainRegistrationTeamInvitation :: InvitationFlow -> TeamId -> EmailAddress -> EnterpriseLoginSubsystem m ()
  GuardEmailDomainRegistrationRegister :: EmailAddress -> EnterpriseLoginSubsystem m ()
  GetDomainRegistration :: Domain -> EnterpriseLoginSubsystem m DomainRegistrationResponse
  TryGetDomainRegistration :: Domain -> EnterpriseLoginSubsystem m (Maybe DomainRegistrationResponse)
  UpdateDomainRedirect ::
    Token ->
    Domain ->
    DomainRedirectConfig ->
    EnterpriseLoginSubsystem m ()
  UpdateTeamInvite ::
    Local UserId ->
    Domain ->
    TeamInviteConfig ->
    EnterpriseLoginSubsystem m ()
  GetDomainRegistrationPublic ::
    GetDomainRegistrationRequest ->
    EnterpriseLoginSubsystem m DomainRedirect
  CreateDomainVerificationChallenge ::
    Domain ->
    EnterpriseLoginSubsystem m DomainVerificationChallenge
  VerifyChallenge ::
    Domain ->
    ChallengeId ->
    Token ->
    EnterpriseLoginSubsystem m Token
  AuthorizeTeam :: Local UserId -> Domain -> DomainOwnershipToken -> EnterpriseLoginSubsystem m ()

makeSem ''EnterpriseLoginSubsystem
