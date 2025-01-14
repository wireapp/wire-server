module Wire.MockInterpreters.EnterpriseLoginSubsystem where

import Imports
import Polysemy
import Wire.EnterpriseLoginSubsystem

-- HINT: This is used to test AuthenticationSubsystem, ...; not to test itself!
enterpriseLoginSubsystemTestInterpreter :: InterpreterFor EnterpriseLoginSubsystem r
enterpriseLoginSubsystemTestInterpreter =
  interpret \case
    LockDomain _ -> undefined -- :: Domain -> EnterpriseLoginSubsystem m ()
    UnlockDomain _ -> undefined -- :: Domain -> EnterpriseLoginSubsystem m ()
    PreAuthorizeDomain _ -> undefined -- :: Domain -> EnterpriseLoginSubsystem m ()
    UnAuthorizeDomain _ -> undefined -- :: Domain -> EnterpriseLoginSubsystem m ()
    UpdateDomainRegistration _ _ -> undefined -- :: Domain -> DomainRegistrationUpdate -> EnterpriseLoginSubsystem m ()
    DeleteDomain _ -> undefined -- :: Domain -> EnterpriseLoginSubsystem m ()
    GetDomainRegistration _ -> undefined -- :: Domain -> EnterpriseLoginSubsystem m DomainRegistration
    GuardEmailDomainRegistrationState _ _ _ -> undefined -- :: InvitationFlow -> TeamId -> EmailAddress -> EnterpriseLoginSubsystem m ()
