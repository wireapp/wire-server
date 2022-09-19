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

module Brig.Effects.GundeckAccess where

import Brig.Types.User.Event
import Data.Id
import Data.List.NonEmpty
import Gundeck.Types.Push.V2
import Imports
import Polysemy

data GundeckAccess m a where
  PushEvents ::
    -- | The events to push.
    NonEmpty Event ->
    -- | The users to push to.
    NonEmpty UserId ->
    -- | The originator of the events.
    UserId ->
    -- | The push routing strategy.
    Route ->
    -- | The originating device connection.
    Maybe ConnId ->
    GundeckAccess m ()
  PushEventsAsync ::
    -- | The events to push.
    NonEmpty Event ->
    -- | The users to push to.
    NonEmpty UserId ->
    -- | The originator of the events.
    UserId ->
    -- | The push routing strategy.
    Route ->
    -- | The originating device connection.
    Maybe ConnId ->
    GundeckAccess m ()

makeSem ''GundeckAccess

-- | (Asynchronously) notifies other users of events.
notify ::
  Member GundeckAccess r =>
  NonEmpty Event ->
  -- | Origin user, TODO: Delete
  UserId ->
  -- | Push routing strategy.
  Route ->
  -- | Origin device connection, if any.
  Maybe ConnId ->
  -- | Users to notify.
  NonEmpty UserId ->
  Sem r ()
notify events orig route conn recipients =
  pushEventsAsync events recipients orig route conn

notifySelf ::
  Member GundeckAccess r =>
  NonEmpty Event ->
  -- | Origin user.
  UserId ->
  -- | Push routing strategy.
  Route ->
  -- | Origin device connection, if any.
  Maybe ConnId ->
  Sem r ()
notifySelf events orig route conn =
  notify events orig route conn (pure orig)
