module Wire.EnterpriseLoginSubsystem.Error where

import Imports
import Network.HTTP.Types
import Network.Wai.Utilities qualified as Wai
import Wire.Error

data EnterpriseLoginSubsystemError
  = EnterpriseLoginSubsystemErrorNotFound
  | EnterpriseLoginSubsystemInternalError
  | EnterpriseLoginSubsystemUnlockError
  | EnterpriseLoginSubsystemErrorInvalidDomainRedirect
  deriving (Show, Eq)

instance Exception EnterpriseLoginSubsystemError

enterpriseLoginSubsystemErrorToHttpError :: EnterpriseLoginSubsystemError -> HttpError
enterpriseLoginSubsystemErrorToHttpError =
  StdError . \case
    EnterpriseLoginSubsystemErrorNotFound -> Wai.mkError status404 "not-found" "Not Found"
    EnterpriseLoginSubsystemInternalError -> Wai.mkError status500 "internal-error" "Internal Server Error"
    EnterpriseLoginSubsystemUnlockError -> Wai.mkError status400 "unlock-error" "Domain can only be unlocked from a locked state"
    EnterpriseLoginSubsystemErrorInvalidDomainRedirect -> Wai.mkError status400 "invalid-domain-redirect" "Invalid domain redirect"
