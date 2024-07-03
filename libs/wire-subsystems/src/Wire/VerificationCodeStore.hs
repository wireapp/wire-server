{-# LANGUAGE TemplateHaskell #-}

module Wire.VerificationCodeStore where

import Data.RetryAfter
import Imports
import Polysemy
import Wire.VerificationCode

data VerificationCodeStore m a where
  InsertCode :: Code -> VerificationCodeStore m ()
  LookupCode :: Key -> Scope -> VerificationCodeStore m (Maybe Code)
  DeleteCode :: Key -> Scope -> VerificationCodeStore m ()
  InsertThrottle :: Key -> Scope -> Int -> VerificationCodeStore m ()
  LookupThrottle :: Key -> Scope -> VerificationCodeStore m (Maybe RetryAfter)

makeSem ''VerificationCodeStore
