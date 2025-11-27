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

module Wire.MockInterpreters.VerificationCodeStore where

import Control.Error
import Data.Map qualified as Map
import Data.RetryAfter
import Data.Time
import Imports
import Polysemy
import Polysemy.State
import Wire.Sem.Now as Now
import Wire.VerificationCode
import Wire.VerificationCodeStore

type ExpiresAt = UTCTime

type CodeState = Map (Key, Scope) (Code, UTCTime)

type ThrottleState = Map (Key, Scope) (Word, UTCTime)

inMemoryVerificationCodeStore ::
  forall r.
  ( Member Now r,
    Member (State CodeState) r,
    Member (State ThrottleState) r
  ) =>
  InterpreterFor VerificationCodeStore r
inMemoryVerificationCodeStore =
  interpret
    \case
      InsertCode code -> do
        expiresAt <- (addUTCTime code.codeTTL.timeoutDiffTime) <$> Now.get
        modify $ Map.insert (code.codeKey, code.codeScope) (code, expiresAt)
      LookupCode key scope -> lookupWithExpiry (key, scope)
      DeleteCode key scope -> modify @CodeState $ Map.delete (key, scope)
      InsertThrottle key scope ttl -> do
        expiresAt <- (addUTCTime (fromIntegral ttl)) <$> Now.get
        modify $ Map.insert (key, scope) (ttl, expiresAt)
      LookupThrottle key scope -> RetryAfter . fromIntegral <$$> lookupWithExpiry (key, scope)

runInMemoryVerificationCodeStore :: (Member Now r) => InterpreterFor VerificationCodeStore r
runInMemoryVerificationCodeStore =
  evalState mempty
    . evalState mempty
    . inMemoryVerificationCodeStore
    . raiseUnder @(State CodeState)
    . raiseUnder @(State ThrottleState)

lookupWithExpiry ::
  ( Member Now r,
    Member (State (Map k (v, UTCTime))) r,
    Ord k
  ) =>
  k ->
  Sem r (Maybe v)
lookupWithExpiry k = runMaybeT $ do
  (v, expiresAt) <- MaybeT $ gets $ Map.lookup k
  now <- lift $ Now.get
  if now <= expiresAt
    then pure v
    else MaybeT $ do
      modify $ Map.delete k
      pure Nothing
