module Wire.EnterpriseLoginSubsystem.Error where

import Data.Text.Lazy qualified as LT
import Imports
import Network.HTTP.Types
import Network.Wai.Utilities qualified as Wai
import Wire.Arbitrary
import Wire.Error

data EnterpriseLoginSubsystemError
  = EnterpriseLoginSubsystemErrorNotFound
  | EnterpriseLoginSubsystemInternalError LText
  | EnterpriseLoginSubsystemErrorUpdateFailure LText
  | EnterpriseLoginSubsystemUnlockError
  | EnterpriseLoginSubsystemUnAuthorizeError
  | EnterpriseLoginSubsystemPreAuthorizeError
  | EnterpriseLoginSubsystemGuardFailed GuardFailure
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform EnterpriseLoginSubsystemError)

instance Exception EnterpriseLoginSubsystemError

data GuardFailure
  = DomRedirSetToSSO
  | DomRedirSetToBackend
  | DomRedirSetToNoRegistration
  | TeamInviteSetToNotAllowed
  | TeamInviteRestrictedToOtherTeam
  | InvalidDomain String
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform GuardFailure)

enterpriseLoginSubsystemErrorToHttpError :: EnterpriseLoginSubsystemError -> HttpError
enterpriseLoginSubsystemErrorToHttpError =
  StdError . \case
    EnterpriseLoginSubsystemErrorNotFound -> Wai.mkError status404 "not-found" "Not Found"
    EnterpriseLoginSubsystemInternalError msg -> Wai.mkError status500 "internal-error" msg
    EnterpriseLoginSubsystemErrorUpdateFailure msg -> Wai.mkError status400 "update-failure" msg
    EnterpriseLoginSubsystemUnlockError -> Wai.mkError status409 "unlock-error" "Domain can only be unlocked from a locked state"
    EnterpriseLoginSubsystemUnAuthorizeError -> Wai.mkError status409 "unauthorize-error" "Domain redirect can not bet set to unauthorized when locked or SSO"
    EnterpriseLoginSubsystemPreAuthorizeError -> Wai.mkError status409 "preauthorize-error" "Domain redirect must be 'none' to be pre-authorized"
    EnterpriseLoginSubsystemGuardFailed err ->
      let e403 msg = Wai.mkError status403 "condition-failed" msg
          e400 msg = Wai.mkError status400 "invalid-domain" (LT.pack msg)
       in case err of
            DomRedirSetToSSO -> e403 "`domain_redirect` is set to `sso:{code}`"
            DomRedirSetToBackend -> e403 "`domain_redirect` is set to `backend`"
            DomRedirSetToNoRegistration -> e403 "`domain_redirect` is set to `no-registration`"
            TeamInviteSetToNotAllowed -> e403 "`teamInvite` is set to `not-allowed`"
            TeamInviteRestrictedToOtherTeam -> e403 "`teamInvite` is restricted to another team."
            InvalidDomain msg -> e400 msg -- probably impossible.
