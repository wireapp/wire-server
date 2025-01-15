module Wire.EnterpriseLoginSubsystem.Error where

import Imports
import Network.HTTP.Types
import Network.Wai.Utilities qualified as Wai
import Wire.Error

data EnterpriseLoginSubsystemError
  = EnterpriseLoginSubsystemErrorNotFound
  | EnterpriseLoginSubsystemInternalError LText
  | EnterpriseLoginSubsystemErrorUpdateFailure LText
  | EnterpriseLoginSubsystemUnlockError
  | EnterpriseLoginSubsystemUnAuthorizeError
  | EnterpriseLoginSubsystemPreAuthorizeError
  deriving (Show, Eq)

instance Exception EnterpriseLoginSubsystemError

enterpriseLoginSubsystemErrorToHttpError :: EnterpriseLoginSubsystemError -> HttpError
enterpriseLoginSubsystemErrorToHttpError =
  StdError . \case
    EnterpriseLoginSubsystemErrorNotFound -> Wai.mkError status404 "not-found" "Not Found"
    EnterpriseLoginSubsystemInternalError msg -> Wai.mkError status500 "internal-error" msg
    EnterpriseLoginSubsystemErrorUpdateFailure msg -> Wai.mkError status400 "update-failure" msg
    EnterpriseLoginSubsystemUnlockError -> Wai.mkError status409 "unlock-error" "Domain can only be unlocked from a locked state"
    EnterpriseLoginSubsystemUnAuthorizeError -> Wai.mkError status409 "unauthorize-error" "Domain redirect can not bet set to unauthorized when locked or SSO"
    EnterpriseLoginSubsystemPreAuthorizeError -> Wai.mkError status409 "preauthorize-error" "Domain redirect must be 'none' to be pre-authorized"
