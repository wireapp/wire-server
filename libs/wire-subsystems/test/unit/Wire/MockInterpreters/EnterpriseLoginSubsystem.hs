module Wire.MockInterpreters.EnterpriseLoginSubsystem where

import Imports
import Polysemy
import Polysemy.Error (Error, throw)
import Wire.EnterpriseLoginSubsystem
import Wire.EnterpriseLoginSubsystem.Error (EnterpriseLoginSubsystemError (EnterpriseLoginSubsystemGuardFailed))

-- HINT: This is used to test AuthenticationSubsystem, ...; not to test itself!
enterpriseLoginSubsystemTestInterpreter ::
  (Member (Error EnterpriseLoginSubsystemError) r) =>
  InterpreterFor EnterpriseLoginSubsystem r
enterpriseLoginSubsystemTestInterpreter =
  interpret \case
    LockDomain _ -> undefined -- :: Domain -> EnterpriseLoginSubsystem m ()
    UnlockDomain _ -> undefined -- :: Domain -> EnterpriseLoginSubsystem m ()
    PreAuthorizeDomain _ -> undefined -- :: Domain -> EnterpriseLoginSubsystem m ()
    UnAuthorizeDomain _ -> undefined -- :: Domain -> EnterpriseLoginSubsystem m ()
    UpdateDomainRegistration _ _ -> undefined -- :: Domain -> DomainRegistrationUpdate -> EnterpriseLoginSubsystem m ()
    DeleteDomain _ -> undefined -- :: Domain -> EnterpriseLoginSubsystem m ()
    GetDomainRegistration _ -> undefined -- :: Domain -> EnterpriseLoginSubsystem m DomainRegistration
    GuardEmailDomainRegistrationState {} -> throw $ EnterpriseLoginSubsystemGuardFailed "error"
