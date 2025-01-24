{-# LANGUAGE TemplateHaskell #-}

module Wire.EnterpriseLoginSubsystem where

import Data.Id
import Imports
import Polysemy
import Text.Email.Parser
import Wire.Arbitrary

data InvitationFlow = ExistingUser | NewUser
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via GenericUniform InvitationFlow

data EnterpriseLoginSubsystem m a where
  GuardEmailDomainRegistrationTeamInvitation :: InvitationFlow -> TeamId -> EmailAddress -> EnterpriseLoginSubsystem m ()
  GuardEmailDomainRegistrationRegister :: EmailAddress -> EnterpriseLoginSubsystem m ()

makeSem ''EnterpriseLoginSubsystem
