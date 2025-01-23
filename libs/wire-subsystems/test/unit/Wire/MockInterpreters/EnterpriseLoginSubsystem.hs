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
    LockDomain _ -> undefined
    UnlockDomain _ -> undefined
    PreAuthorizeDomain _ -> undefined
    UnAuthorizeDomain _ -> undefined
    UpdateDomainRegistration _ _ -> undefined
    DeleteDomain _ -> undefined
    GetDomainRegistration _ -> undefined
    GuardEmailDomainRegistrationTeamInvitation {} -> throw err
    GuardEmailDomainRegistrationRegister _ -> throw err
    TryGetDomainRegistration _ -> undefined
    UpdateDomainRedirect {} -> undefined
    UpdateTeamInvite {} -> undefined
    GetDomainRegistrationPublic _ -> undefined
    CreateDomainVerificationChallenge _ -> undefined
    VerifyChallenge _ _ _ -> undefined
