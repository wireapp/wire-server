{-# LANGUAGE TemplateHaskell #-}

module Wire.EnterpriseLoginSubsystem where

import Data.Domain
import Data.Id
import Data.Qualified
import Imports
import Polysemy
import Text.Email.Validate
import Wire.API.EnterpriseLogin
import Wire.API.Routes.Public.Brig.DomainVerification
import Wire.API.User.EmailAddress

-- | FUTUREWORK: Rename this to `DomainRegistrationSubsystem`
data EnterpriseLoginSubsystem m a where
  LockDomain :: Domain -> EnterpriseLoginSubsystem m ()
  UnlockDomain :: Domain -> EnterpriseLoginSubsystem m ()
  PreAuthorizeDomain :: Domain -> EnterpriseLoginSubsystem m ()
  UnAuthorizeDomain :: Domain -> EnterpriseLoginSubsystem m ()
  UpdateDomainRegistration :: Domain -> DomainRegistrationUpdate -> EnterpriseLoginSubsystem m ()
  DeleteDomain :: Domain -> EnterpriseLoginSubsystem m ()
  GetDomainRegistration :: Domain -> EnterpriseLoginSubsystem m (Maybe DomainRegistrationResponse)
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
    EnterpriseLoginSubsystem m DomainRedirectResponse
  CreateDomainVerificationChallenge ::
    Domain ->
    EnterpriseLoginSubsystem m DomainVerificationChallenge
  VerifyChallenge ::
    Domain ->
    ChallengeId ->
    Token ->
    EnterpriseLoginSubsystem m Token
  AuthorizeTeam :: Local UserId -> Domain -> DomainOwnershipToken -> EnterpriseLoginSubsystem m ()
  GetRegisteredDomains :: Local UserId -> TeamId -> EnterpriseLoginSubsystem m RegisteredDomains
  DeleteTeamDomain :: Local UserId -> TeamId -> Domain -> EnterpriseLoginSubsystem m ()

makeSem ''EnterpriseLoginSubsystem

getDomainRegistrationByEmail ::
  forall r.
  (Member EnterpriseLoginSubsystem r) =>
  EmailAddress ->
  Sem r (Maybe DomainRegistrationResponse)
getDomainRegistrationByEmail email = do
  case emailDomain email of
    Left _ ->
      -- this can only be nothing if EmailAddress and Domain parsers disagree on what
      -- constitutes a valid domain.
      pure Nothing
    Right domain ->
      getDomainRegistration domain
