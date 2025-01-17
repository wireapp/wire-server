module Wire.EnterpriseLoginSubsystem.Error where

import Data.Text.Lazy qualified as LT
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
  | EnterpriseLoginSubsystemGuardFailed GuardFailure
  deriving (Show, Eq)

instance Exception EnterpriseLoginSubsystemError

data GuardFailure
  = DomRedirSetToSSO
  | DomRedirSetToBackend
  | DomRedirSetToNoRegistration
  | TeamInviteSetToNotAllowed
  | TeamInviteRestrictedToOtherTeam
  | InvalidDomain String
  deriving (Show, Eq)

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
      -- TODO: maybe we can show these wai errors to client devs and see if they have change
      -- requests?  also, are we leaking too much info?
      let e409 msg = Wai.mkError status409 "enterprise-login-guard-failed" ("condition failed: " <> msg)
          e423 msg = Wai.mkError status423 "enterprise-login-guard-failed" ("could not parse domain: " <> LT.pack msg)
       in case err of
            DomRedirSetToSSO -> e409 "`domain_redirect` is set to `sso:{code}`"
            DomRedirSetToBackend -> e409 "`domain_redirect` is set to `backend`" -- TODO: https://wearezeta.atlassian.net/wiki/spaces/ENGINEERIN/pages/1570832467/Email+domain+registration+and+configuration says this should be 403 with another label?
            DomRedirSetToNoRegistration -> e409 "`domain_redirect` is set to `no-registration`"
            TeamInviteSetToNotAllowed -> e409 "`teamInvite` is set to `not-allowed`"
            TeamInviteRestrictedToOtherTeam -> e409 "`teamInvite` is restricted to another team."
            InvalidDomain msg -> e423 msg
  where
    status423 :: Status
    status423 = mkStatus 423 "Locked"
