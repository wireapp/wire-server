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

module Brig.Effects.PasswordResetStore where

import Brig.Types.User (PasswordResetPair)
import Data.Id
import Imports
import Polysemy
import Wire.API.User.Identity
import Wire.API.User.Password

data PasswordResetStore m a where
  CreatePasswordResetCode ::
    UserId ->
    Either Email Phone ->
    PasswordResetStore m PasswordResetPair
  LookupPasswordResetCode ::
    UserId ->
    PasswordResetStore m (Maybe PasswordResetCode)
  VerifyPasswordResetCode ::
    PasswordResetPair ->
    PasswordResetStore m (Maybe UserId)

makeSem ''PasswordResetStore
