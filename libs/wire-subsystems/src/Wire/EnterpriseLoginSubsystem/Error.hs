module Wire.EnterpriseLoginSubsystem.Error where

import Data.Text.Lazy as LT
import Imports
import Wire.API.Error
import Wire.API.Error.Brig
import Wire.Arbitrary
import Wire.Error

data EnterpriseLoginSubsystemError
  = EnterpriseLoginSubsystemErrorNotFound
  | EnterpriseLoginSubsystemInvalidDomain
  | EnterpriseLoginSubsystemInvalidDomainUpdate String
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
    EnterpriseLoginSubsystemInvalidDomainUpdate msg -> errorToWaiWithMessage @DomainVerificationInvalidDomainUpdate (LT.pack msg)
    EnterpriseLoginSubsystemDomainVerificationFailed -> errorToWai @DomainVerificationDomainVerificationFailed
    EnterpriseLoginSubsystemOperationForbidden -> errorToWai @DomainVerificationOperationForbidden
    EnterpriseLoginSubsystemPaymentRequired -> errorToWai @DomainVerificationPaymentRequired
    EnterpriseLoginSubsystemNotEnabled -> errorToWai @DomainVerificationNotEnabled
    EnterpriseLoginSubsystemChallengeNotFound -> errorToWai @DomainVerificationChallengeNotFound
    EnterpriseLoginSubsystemAuthFailure -> errorToWai @DomainVerificationAuthFailure
