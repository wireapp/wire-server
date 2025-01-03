{-# LANGUAGE TemplateHaskell #-}

module Wire.EnterpriseLoginSubsystem where

import Data.Domain
import Imports
import Polysemy
import Wire.API.EnterpriseLogin

data EnterpriseLoginSubsystem m a where
  LockDomain :: Domain -> EnterpriseLoginSubsystem m ()
  UnlockDomain :: Domain -> EnterpriseLoginSubsystem m ()
  PreAuthorizeDomain :: Domain -> EnterpriseLoginSubsystem m ()
  UnAuthorizeDomain :: Domain -> EnterpriseLoginSubsystem m ()
  UpdateDomainRegistration :: Domain -> DomainRegistrationUpdate -> EnterpriseLoginSubsystem m ()
  DeleteDomain :: Domain -> EnterpriseLoginSubsystem m ()
  GetDomainRegistration :: Domain -> EnterpriseLoginSubsystem m DomainRegistration
  GetDomainVerificationToken ::
    Domain ->
    DomainVerificationAuthToken ->
    EnterpriseLoginSubsystem m DomainVerificationToken
  VerifyDNSRecord ::
    Domain ->
    DomainVerificationAuthToken ->
    EnterpriseLoginSubsystem m Bool

makeSem ''EnterpriseLoginSubsystem
