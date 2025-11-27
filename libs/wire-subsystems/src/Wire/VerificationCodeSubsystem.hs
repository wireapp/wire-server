{-# LANGUAGE TemplateHaskell #-}

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

module Wire.VerificationCodeSubsystem where

import Data.ByteString.Conversion
import Data.Code
import Data.RetryAfter
import Data.UUID (UUID)
import Imports hiding (lookup)
import Polysemy
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.Error
import Wire.VerificationCode
import Wire.VerificationCodeGen

data VerificationCodeSubsystemError
  = VerificationCodeThrottled RetryAfter
  deriving (Show, Eq)

verificationCodeSubsystemErrorToHttpError :: VerificationCodeSubsystemError -> HttpError
verificationCodeSubsystemErrorToHttpError = \case
  VerificationCodeThrottled t ->
    RichError
      (errorToWai @E.VerificationCodeThrottled)
      ()
      [("Retry-After", toByteString' (retryAfterSeconds t))]

newtype CodeAlreadyExists = CodeAlreadyExists Code
  deriving (Show, Eq)

data VerificationCodeSubsystem m a where
  CreateCode ::
    -- | The 'Gen'erator to use.
    VerificationCodeGen ->
    -- | The scope of the generated code.
    Scope ->
    -- | Maximum verification attempts.
    Retries ->
    -- | Time-to-live in seconds.
    Timeout ->
    -- | Associated account ID.
    Maybe UUID ->
    VerificationCodeSubsystem m (Either CodeAlreadyExists Code)
  CreateCodeOverwritePrevious ::
    -- | The 'Gen'erator to use.
    VerificationCodeGen ->
    -- | The scope of the generated code.
    Scope ->
    -- | Maximum verification attempts.
    Retries ->
    -- | Time-to-live in seconds.
    Timeout ->
    -- | Associated account ID.
    Maybe UUID ->
    VerificationCodeSubsystem m Code
  -- Returns the 'Code' iff verification suceeds.
  VerifyCode :: Key -> Scope -> Value -> VerificationCodeSubsystem m (Maybe Code)
  DeleteCode :: Key -> Scope -> VerificationCodeSubsystem m ()
  -- For internal endpoints
  InternalLookupCode :: Key -> Scope -> VerificationCodeSubsystem m (Maybe Code)

makeSem ''VerificationCodeSubsystem
