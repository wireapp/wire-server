{-# LANGUAGE TemplateHaskell #-}

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

verificationCodeSubsystemErrorToHttpError :: VerificationCodeSubsystemError -> HttpError
verificationCodeSubsystemErrorToHttpError = \case
  VerificationCodeThrottled t ->
    RichError
      (errorToWai @E.VerificationCodeThrottled)
      ()
      [("Retry-After", toByteString' (retryAfterSeconds t))]

newtype CodeAlreadyExists = CodeAlreadyExists Code

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
  VerifyCode :: Key -> Scope -> Value -> VerificationCodeSubsystem m (Maybe Code)
  DeleteCode :: Key -> Scope -> VerificationCodeSubsystem m ()
  -- For internal endpoints
  InternalLookupCode :: Key -> Scope -> VerificationCodeSubsystem m (Maybe Code)

makeSem ''VerificationCodeSubsystem
