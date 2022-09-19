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

module Brig.Effects.ActivationKeyStore where

import Cassandra (Ascii)
import Data.Id
import Imports
import Polysemy
import Wire.API.User.Activation

-- | Max. number of activation attempts per 'ActivationKey'.
maxAttempts :: Int32
maxAttempts = 3

type GetKeyTuple = (Int32, Ascii, Text, ActivationCode, Maybe UserId, Int32)

type InsertKeyTuple = (ActivationKey, Text, Text, ActivationCode, Maybe UserId, Int32, Int32)

data ActivationKeyStore m a where
  GetActivationKey :: ActivationKey -> ActivationKeyStore m (Maybe GetKeyTuple)
  InsertActivationKey :: InsertKeyTuple -> ActivationKeyStore m ()
  DeleteActivationPair :: ActivationKey -> ActivationKeyStore m ()

makeSem ''ActivationKeyStore
