module Wire.EnterpriseLoginSubsystem.Error where

import Imports
import Network.HTTP.Types
import Network.Wai.Utilities qualified as Wai
import Wire.Error

data EnterpriseLoginSubsystemError
  = EnterpriseLoginSubsystemErrorNotFound
  | EnterpriseLoginSubsystemInternalError LText
  | EnterpriseLoginSubsystemUnlockError
  | EnterpriseLoginSubsystemErrorInvalidDomainRedirect
  | EnterpriseLoginSubsystemErrorUpdateFailure LText
  deriving (Show, Eq)

instance Exception EnterpriseLoginSubsystemError

enterpriseLoginSubsystemErrorToHttpError :: EnterpriseLoginSubsystemError -> HttpError
enterpriseLoginSubsystemErrorToHttpError =
  StdError . \case
    EnterpriseLoginSubsystemErrorNotFound -> Wai.mkError status404 "not-found" "Not Found"
    EnterpriseLoginSubsystemInternalError msg -> Wai.mkError status500 "internal-error" msg
    EnterpriseLoginSubsystemUnlockError -> Wai.mkError status400 "unlock-error" "Domain can only be unlocked from a locked state"
    EnterpriseLoginSubsystemErrorInvalidDomainRedirect -> Wai.mkError status400 "invalid-domain-redirect" "Invalid domain redirect"
    EnterpriseLoginSubsystemErrorUpdateFailure msg -> Wai.mkError status400 "update-failure" msg
