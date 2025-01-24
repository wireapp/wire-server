{-# LANGUAGE TemplateHaskell #-}

module Wire.DomainRegistrationSubsystem where

import Data.Domain
import Data.Id
import Data.Qualified
import Imports
import Polysemy
import Wire.API.EnterpriseLogin
import Wire.API.Routes.Public.Brig.DomainVerification

data DomainRegistrationSubsystem m a where
  LockDomain :: Domain -> DomainRegistrationSubsystem m ()
  UnlockDomain :: Domain -> DomainRegistrationSubsystem m ()
  PreAuthorizeDomain :: Domain -> DomainRegistrationSubsystem m ()
  UnAuthorizeDomain :: Domain -> DomainRegistrationSubsystem m ()
  UpdateDomainRegistration :: Domain -> DomainRegistrationUpdate -> DomainRegistrationSubsystem m ()
  DeleteDomain :: Domain -> DomainRegistrationSubsystem m ()
  GetDomainRegistration :: Domain -> DomainRegistrationSubsystem m DomainRegistration
  TryGetDomainRegistration :: Domain -> DomainRegistrationSubsystem m (Maybe DomainRegistration)
  RequestDomainVerificationToken ::
    Maybe DomainVerificationAuthToken ->
    Domain ->
    DomainRegistrationSubsystem m DomainVerificationTokenResponse
  RequestDomainVerificationTeamToken ::
    Local UserId ->
    Domain ->
    DomainRegistrationSubsystem m DomainVerificationTokenResponse
  UpdateDomainRedirect ::
    DomainVerificationAuthToken ->
    Domain ->
    DomainRedirectConfig ->
    DomainRegistrationSubsystem m ()
  UpdateTeamInvite ::
    Local UserId ->
    Domain ->
    TeamInviteConfig ->
    DomainRegistrationSubsystem m ()
  GetDomainRegistrationPublic ::
    GetDomainRegistrationRequest ->
    DomainRegistrationSubsystem m DomainRedirect

makeSem ''DomainRegistrationSubsystem
