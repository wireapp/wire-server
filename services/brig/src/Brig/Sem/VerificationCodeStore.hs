{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.Sem.VerificationCodeStore where

import Brig.Types.Code
import Data.UUID
import Imports
import Polysemy
import Wire.API.User.Identity

data Code = Code
  { codeKey :: !Key,
    codeScope :: !Scope,
    codeValue :: !Value,
    codeRetries :: !Retries,
    codeTTL :: !Timeout,
    codeFor :: !CodeFor,
    codeAccount :: !(Maybe UUID)
  }
  deriving (Eq, Show)

data CodeFor
  = ForEmail !Email
  | ForPhone !Phone
  deriving (Eq, Show)

-- | The same 'Key' can exist with different 'Value's in different
-- 'Scope's at the same time.
data Scope
  = AccountDeletion
  | IdentityVerification
  | PasswordReset
  | AccountLogin
  | AccountApproval
  | CreateScimToken
  | DeleteTeam
  deriving (Eq, Show)

newtype Retries = Retries {numRetries :: Word8}
  deriving (Eq, Show, Ord, Num, Integral, Enum, Real)

data VerificationCodeStore m a where
  GetPendingCode :: Key -> Scope -> VerificationCodeStore m (Maybe Code) -- 'lookup' in 'Brig.Code'
  InsertCode :: Code -> VerificationCodeStore m () -- 'insert' in 'Brig.Code'

makeSem ''VerificationCodeStore

-- | Lookup and verify the code for the given key and scope
-- against the given value.
verifyCode ::
  Member VerificationCodeStore r =>
  Key ->
  Scope ->
  Value ->
  Sem r (Maybe Code)
verifyCode k s v = getPendingCode k s >>= maybe (pure Nothing) continue
  where
    continue c
      | codeValue c == v = pure (Just c)
      | codeRetries c > 0 = do
        insertCode (c {codeRetries = codeRetries c - 1})
        pure Nothing
      | otherwise = pure Nothing

codeForEmail :: Code -> Maybe Email
codeForEmail c
  | ForEmail e <- codeFor c = Just e
  | otherwise = Nothing

codeForPhone :: Code -> Maybe Phone
codeForPhone c
  | ForPhone p <- codeFor c = Just p
  | otherwise = Nothing
