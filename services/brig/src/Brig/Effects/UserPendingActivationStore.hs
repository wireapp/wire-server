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

module Brig.Effects.UserPendingActivationStore where

import Data.Id
import Data.Time.Clock
import Imports
import Polysemy
import Wire.Sem.Paging

data UserPendingActivation = UserPendingActivation
  { upaUserId :: !UserId,
    upaDay :: !UTCTime
  }
  deriving stock (Eq, Show, Ord)

data UserPendingActivationStore p m a where
  Add :: UserPendingActivation -> UserPendingActivationStore p m ()
  List ::
    Maybe (PagingState p UserPendingActivation) ->
    UserPendingActivationStore p m (Page p UserPendingActivation)
  RemoveMultiple :: [UserId] -> UserPendingActivationStore p m ()

makeSem ''UserPendingActivationStore

remove :: forall p r. (Member (UserPendingActivationStore p) r) => UserId -> Sem r ()
remove uid = removeMultiple [uid]
