module Wire.EnterpriseLoginSubsystem.Error where

import Imports
import Wire.API.Error
import Wire.API.Error.Brig
import Wire.Arbitrary
import Wire.Error

data EnterpriseLoginSubsystemError
  = EnterpriseLoginSubsystemErrorNotFound
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

enterpriseLoginSubsystemErrorToHttpError :: EnterpriseLoginSubsystemError -> HttpError
enterpriseLoginSubsystemErrorToHttpError =
  StdError . \case
    EnterpriseLoginSubsystemErrorNotFound -> errorToWai @DomainVerificationErrorNotFound
    EnterpriseLoginSubsystemInvalidDomain -> errorToWai @DomainVerificationInvalidDomain
    EnterpriseLoginSubsystemDomainVerificationFailed -> errorToWai @DomainVerificationDomainVerificationFailed
    EnterpriseLoginSubsystemOperationForbidden -> errorToWai @DomainVerificationOperationForbidden
    EnterpriseLoginSubsystemPaymentRequired -> errorToWai @DomainVerificationPaymentRequired
    EnterpriseLoginSubsystemNotEnabled -> errorToWai @DomainVerificationNotEnabled
    EnterpriseLoginSubsystemChallengeNotFound -> errorToWai @DomainVerificationChallengeNotFound
    EnterpriseLoginSubsystemAuthFailure -> errorToWai @DomainVerificationAuthFailure
