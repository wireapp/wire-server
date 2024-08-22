{-# LANGUAGE QuantifiedConstraints #-}
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
{-# LANGUAGE TemplateHaskell #-}

module Wire.PasswordResetCodeStore where

import Data.Id
import Data.Time.Clock
import Imports
import Polysemy
import Wire.API.User.Password

-- | Parameters used in password reset queries. The f type constructor is used
-- either as 'Identity' or 'Maybe'.
data PRQueryData f = PRQueryData
  { prqdCode :: PasswordResetCode,
    prqdUser :: UserId,
    prqdRetries :: f Int32,
    prqdTimeout :: f UTCTime
  }

deriving instance (forall a. (Show a) => Show (f a)) => Show (PRQueryData f)

deriving instance (forall a. (Eq a) => Eq (f a)) => Eq (PRQueryData f)

mapPRQueryData :: (forall a. (f1 a -> f2 a)) -> PRQueryData f1 -> PRQueryData f2
mapPRQueryData f prqd = prqd {prqdRetries = f prqd.prqdRetries, prqdTimeout = f prqd.prqdTimeout}

data PasswordResetCodeStore m a where
  GenerateEmailCode :: PasswordResetCodeStore m PasswordResetCode
  GeneratePhoneCode :: PasswordResetCodeStore m PasswordResetCode
  CodeSelect :: PasswordResetKey -> PasswordResetCodeStore m (Maybe (PRQueryData Maybe))
  CodeInsert :: PasswordResetKey -> PRQueryData Identity -> Int32 -> PasswordResetCodeStore m ()
  CodeDelete :: PasswordResetKey -> PasswordResetCodeStore m ()

makeSem ''PasswordResetCodeStore
