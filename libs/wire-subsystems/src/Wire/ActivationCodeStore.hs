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

module Wire.ActivationCodeStore where

import Data.Id
import Imports
import Polysemy
import Util.Timeout
import Wire.API.User.Activation
import Wire.UserKeyStore

data ActivationCodeStore :: Effect where
  LookupActivationCode :: EmailKey -> ActivationCodeStore m (Maybe (Maybe UserId, ActivationCode))
  -- | Create a code for a new pending activation for a given 'EmailKey'
  NewActivationCode ::
    EmailKey ->
    -- | The timeout for the activation code.
    Timeout ->
    -- | The user with whom to associate the activation code.
    Maybe UserId ->
    ActivationCodeStore m Activation

makeSem ''ActivationCodeStore
