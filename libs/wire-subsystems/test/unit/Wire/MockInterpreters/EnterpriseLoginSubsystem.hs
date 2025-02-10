module Wire.MockInterpreters.EnterpriseLoginSubsystem where

import Imports
import Polysemy
import Wire.API.EnterpriseLogin
import Wire.EnterpriseLoginSubsystem

enterpriseLoginSubsystemTestInterpreter :: Maybe DomainRegistration -> InterpreterFor EnterpriseLoginSubsystem r
enterpriseLoginSubsystemTestInterpreter constMbGuardResult =
  interpret \case
    LockDomain _ -> undefined
    UnlockDomain _ -> undefined
    PreAuthorizeDomain _ -> undefined
    UnAuthorizeDomain _ -> undefined
    UpdateDomainRegistration _ _ -> undefined
    DeleteDomain _ -> undefined
    GetDomainRegistration _ -> pure $ mkDomainRegistrationResponse <$> constMbGuardResult
    UpdateDomainRedirect {} -> undefined
    UpdateTeamInvite {} -> undefined
    GetDomainRegistrationPublic _ -> undefined
    CreateDomainVerificationChallenge _ -> undefined
    VerifyChallenge {} -> undefined
    AuthorizeTeam {} -> undefined
    GetRegisteredDomains {} -> undefined
    DeleteTeamDomain {} -> undefined
