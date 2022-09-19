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

module Brig.Effects.UserHandleStore where

import Data.Handle
import Data.Id
import Imports
import Polysemy

-- | An enum with a one-to-one mapping to 'Cassandra.Consistency'.
data Consistency
  = One
  | LocalQuorum
  | All
  deriving (Eq)

data UserHandleStore m a where
  InsertHandle :: Handle -> UserId -> UserHandleStore m ()
  -- | Lookup the current owner of a 'Handle'.                                                                        │
  GetHandleWithConsistency :: Consistency -> Handle -> UserHandleStore m (Maybe UserId)
  DeleteHandle :: Handle -> UserHandleStore m ()

makeSem ''UserHandleStore

-- | Lookup the current owner of a 'Handle'.
lookupHandle :: Member UserHandleStore r => Handle -> Sem r (Maybe UserId)
lookupHandle = getHandleWithConsistency LocalQuorum
