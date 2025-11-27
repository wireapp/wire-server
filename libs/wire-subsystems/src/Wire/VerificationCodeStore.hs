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

module Wire.VerificationCodeStore where

import Data.RetryAfter
import Imports
import Polysemy
import Wire.VerificationCode

data VerificationCodeStore m a where
  InsertCode :: Code -> VerificationCodeStore m ()
  LookupCode :: Key -> Scope -> VerificationCodeStore m (Maybe Code)
  DeleteCode :: Key -> Scope -> VerificationCodeStore m ()
  InsertThrottle :: Key -> Scope -> Word -> VerificationCodeStore m ()
  LookupThrottle :: Key -> Scope -> VerificationCodeStore m (Maybe RetryAfter)

makeSem ''VerificationCodeStore
