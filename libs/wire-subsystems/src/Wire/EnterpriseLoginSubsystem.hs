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

-- TODO: refactor this some more.  make it more type-safe.
data GuardResult
  = GuardResultSuccess
  | GuardResultIncompatibleValues [(Text, Text)] -- Wai.mkError status409 "enterprise-login-guard-failed" ("condition failed: " <> msg)?  mkStatus 423 "Locked"?
  deriving (Eq, Show, Generic)

data EnterpriseLoginSubsystem m a where
  LockDomain :: Domain -> EnterpriseLoginSubsystem m ()
  UnlockDomain :: Domain -> EnterpriseLoginSubsystem m ()
  PreAuthorizeDomain :: Domain -> EnterpriseLoginSubsystem m ()
  UnAuthorizeDomain :: Domain -> EnterpriseLoginSubsystem m ()
  UpdateDomainRegistration :: Domain -> DomainRegistrationUpdate -> EnterpriseLoginSubsystem m ()
  DeleteDomain :: Domain -> EnterpriseLoginSubsystem m ()
  GetDomainRegistration :: Domain -> EnterpriseLoginSubsystem m DomainRegistration
  -- | These following guard functions encapsulate the parts of the behavior of api end-points
  -- that depends on the domain registration record, if available.  The result can serve as a
  -- DSL to be interpreted by the caller.
  GuardEmailDomainRegistrationTeamInvitation :: InvitationFlow -> TeamId -> EmailAddress -> EnterpriseLoginSubsystem m GuardResult
  GuardEmailDomainRegistrationRegister :: EmailAddress -> EnterpriseLoginSubsystem m GuardResult

makeSem ''EnterpriseLoginSubsystem
