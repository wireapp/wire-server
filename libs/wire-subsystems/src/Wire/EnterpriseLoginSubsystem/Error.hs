module Wire.EnterpriseLoginSubsystem.Error where

import Data.Text.Lazy qualified as LT
import Imports
import Network.HTTP.Types
import Network.Wai.Utilities qualified as Wai
import Wire.API.Error
import Wire.API.Error.Brig
import Wire.Arbitrary
import Wire.Error

data EnterpriseLoginSubsystemError
  = EnterpriseLoginSubsystemErrorNotFound
  | EnterpriseLoginSubsystemErrorUpdateFailure LText
  | EnterpriseLoginSubsystemGuardFailed GuardFailure
  | EnterpriseLoginSubsystemInvalidDomain
  | EnterpriseLoginSubsystemDomainVerificationFailed
  | EnterpriseLoginSubsystemOperationForbidden
  | EnterpriseLoginSubsystemAuthFailure
  | EnterpriseLoginSubsystemPaymentRequired
  | EnterpriseLoginSubsystemNotEnabled
  | EnterpriseLoginSubsystemChallengeNotFound
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
    EnterpriseLoginSubsystemErrorNotFound -> errorToWai @DomainVerificationErrorNotFound
    EnterpriseLoginSubsystemErrorUpdateFailure msg -> Wai.mkError status400 "update-failure" msg
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
    EnterpriseLoginSubsystemInvalidDomain -> errorToWai @DomainVerificationInvalidDomain
    EnterpriseLoginSubsystemDomainVerificationFailed -> errorToWai @DomainVerificationDomainVerificationFailed
    EnterpriseLoginSubsystemOperationForbidden -> errorToWai @DomainVerificationOperationForbidden
    EnterpriseLoginSubsystemPaymentRequired -> errorToWai @DomainVerificationPaymentRequired
    EnterpriseLoginSubsystemNotEnabled -> errorToWai @DomainVerificationNotEnabled
    EnterpriseLoginSubsystemChallengeNotFound -> errorToWai @DomainVerificationChallengeNotFound
    EnterpriseLoginSubsystemAuthFailure -> errorToWai @DomainVerificationAuthFailure
