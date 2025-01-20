{-# LANGUAGE TemplateHaskell #-}

module Wire.EnterpriseLoginSubsystem where

import Data.Domain
import Data.Id
import Imports
import Polysemy
import Text.Email.Parser
import Wire.API.EnterpriseLogin
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
  GetDomainRegistration :: Domain -> EnterpriseLoginSubsystem m DomainRegistration
  GuardEmailDomainRegistrationTeamInvitation :: InvitationFlow -> TeamId -> EmailAddress -> EnterpriseLoginSubsystem m ()
  GuardEmailDomainRegistrationRegister :: EmailAddress -> EnterpriseLoginSubsystem m ()
  GuardEmailDomainRegistrationActivateSend :: EmailAddress -> EnterpriseLoginSubsystem m ()

makeSem ''EnterpriseLoginSubsystem
