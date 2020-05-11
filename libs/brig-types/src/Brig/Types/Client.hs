{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

-- TODO: Move to Brig.Types.User.Client
module Brig.Types.Client
  ( -- re-exports
    module C,
    module P,
    ClientType (..),
    ClientClass (..),
    NewClient (..),
    newClient,
    Client (..),
    PubClient (..),
    RmClient (..),
    UpdateClient (..),
  )
where

import Brig.Types.Client.Prekey as P
import Brig.Types.Common as C
import Wire.API.User.Client
