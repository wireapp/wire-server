module Wire.EnterpriseLoginSubsystem.Error where

import Data.Misc (HttpsUrl)
import Imports
import Network.HTTP.Types
import Network.Wai.Utilities qualified as Wai
import SAML2.WebSSO.Types (IdPId)
import Wire.Error

data EnterpriseLoginSubsystemError
  = EnterpriseLoginSubsystemErrorNotFound
  | EnterpriseLoginSubsystemInternalError LText
  | EnterpriseLoginSubsystemErrorUpdateFailure LText
  | EnterpriseLoginSubsystemUnlockError
  | EnterpriseLoginSubsystemUnAuthorizeError
  | EnterpriseLoginSubsystemPreAuthorizeError
  | EnterpriseLoginSubsystemGuardFailed GuardFailure
  | EnterpriseLoginSubsystemGuardInvalidDomain LText
  deriving (Show, Eq)

instance Exception EnterpriseLoginSubsystemError

data GuardFailure
  = BackendRedirect HttpsUrl
  | DomRedirSetToSSO IdPId
  | NoRegistration
  | -- TODO: Will this ever be reached?
    InvalidDomain String
  deriving (Show, Eq)

-- \| EnterpriseLoginSubsystemGuardFailed GuardFailure
-- \| EnterpriseLoginSubsystemGuardInvalidDomain LText
-- EnterpriseLoginSubsystemGuardFailed (BackendRedirect url) -> Wai.mkError status302 "enterprise-login-guard-failed" ("condition failed: " <> msg)
-- EnterpriseLoginSubsystemGuardFailed msg -> Wai.mkError status409 "enterprise-login-guard-failed" ("condition failed: " <> msg)
-- EnterpriseLoginSubsystemGuardInvalidDomain msg -> Wai.mkError status423 "enterprise-login-guard-failed" ("could not parse domain: " <> msg)

enterpriseLoginSubsystemErrorToHttpError :: EnterpriseLoginSubsystemError -> HttpError
enterpriseLoginSubsystemErrorToHttpError =
  StdError . \case
    EnterpriseLoginSubsystemErrorNotFound -> Wai.mkError status404 "not-found" "Not Found"
    EnterpriseLoginSubsystemInternalError msg -> Wai.mkError status500 "internal-error" msg
    EnterpriseLoginSubsystemErrorUpdateFailure msg -> Wai.mkError status400 "update-failure" msg
    EnterpriseLoginSubsystemUnlockError -> Wai.mkError status409 "unlock-error" "Domain can only be unlocked from a locked state"
    EnterpriseLoginSubsystemUnAuthorizeError -> Wai.mkError status409 "unauthorize-error" "Domain redirect can not bet set to unauthorized when locked or SSO"
    EnterpriseLoginSubsystemPreAuthorizeError -> Wai.mkError status409 "preauthorize-error" "Domain redirect must be 'none' to be pre-authorized"
    EnterpriseLoginSubsystemGuardFailed msg -> Wai.mkError status409 "enterprise-login-guard-failed" ("condition failed: " <> msg)
    EnterpriseLoginSubsystemGuardInvalidDomain msg -> Wai.mkError status423 "enterprise-login-guard-failed" ("could not parse domain: " <> msg)
  where
    status423 :: Status
    status423 = mkStatus 423 "Locked"
