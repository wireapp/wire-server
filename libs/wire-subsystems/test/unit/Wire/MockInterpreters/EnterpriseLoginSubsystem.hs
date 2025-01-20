module Wire.MockInterpreters.EnterpriseLoginSubsystem where

import Imports
import Polysemy
import Polysemy.Error (Error, throw)
import Wire.EnterpriseLoginSubsystem
import Wire.EnterpriseLoginSubsystem.Error

enterpriseLoginSubsystemTestInterpreter ::
  (Member (Error EnterpriseLoginSubsystemError) r) =>
  EnterpriseLoginSubsystemError ->
  InterpreterFor EnterpriseLoginSubsystem r
enterpriseLoginSubsystemTestInterpreter err =
  interpret \case
    LockDomain _ -> undefined -- :: Domain -> EnterpriseLoginSubsystem m ()
    UnlockDomain _ -> undefined -- :: Domain -> EnterpriseLoginSubsystem m ()
    PreAuthorizeDomain _ -> undefined -- :: Domain -> EnterpriseLoginSubsystem m ()
    UnAuthorizeDomain _ -> undefined -- :: Domain -> EnterpriseLoginSubsystem m ()
    UpdateDomainRegistration _ _ -> undefined -- :: Domain -> DomainRegistrationUpdate -> EnterpriseLoginSubsystem m ()
    DeleteDomain _ -> undefined -- :: Domain -> EnterpriseLoginSubsystem m ()
    GetDomainRegistration _ -> undefined -- :: Domain -> EnterpriseLoginSubsystem m DomainRegistration
    GuardEmailDomainRegistrationTeamInvitation {} -> throw err
    GuardEmailDomainRegistrationRegister _ -> throw err
    GuardEmailDomainRegistrationActivateSend _ -> throw err
