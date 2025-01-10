{-# LANGUAGE TemplateHaskell #-}

module Wire.EnterpriseLoginSubsystem where

import Data.Domain
import Data.Id
import Polysemy
import Text.Email.Parser
import Wire.API.EnterpriseLogin

data EnterpriseLoginSubsystem m a where
  LockDomain :: Domain -> EnterpriseLoginSubsystem m ()
  UnlockDomain :: Domain -> EnterpriseLoginSubsystem m ()
  PreAuthorizeDomain :: Domain -> EnterpriseLoginSubsystem m ()
  UnAuthorizeDomain :: Domain -> EnterpriseLoginSubsystem m ()
  UpdateDomainRegistration :: Domain -> DomainRegistrationUpdate -> EnterpriseLoginSubsystem m ()
  DeleteDomain :: Domain -> EnterpriseLoginSubsystem m ()
  GetDomainRegistration :: Domain -> EnterpriseLoginSubsystem m DomainRegistration
  GuardEmailDomainRegistrationState :: TeamId -> EmailAddress -> EnterpriseLoginSubsystem m ()

makeSem ''EnterpriseLoginSubsystem
