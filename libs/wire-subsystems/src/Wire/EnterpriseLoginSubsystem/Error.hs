-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

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
