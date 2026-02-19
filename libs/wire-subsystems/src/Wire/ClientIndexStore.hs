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

module Wire.ClientIndexStore
  ( -- * ClientIndexStore Effect
    ClientIndexStore (..),

    -- * Create client
    createClient,

    -- * Get client
    getClients,

    -- * Delete client
    deleteClient,
    deleteClients,

    -- * Configuration
    useIntraClientListing,
  )
where

import Data.Id
import Galley.Types.Clients
import Imports
import Polysemy

data ClientIndexStore m a where
  GetClients :: [UserId] -> ClientIndexStore m Clients
  CreateClient :: UserId -> ClientId -> ClientIndexStore m ()
  DeleteClient :: UserId -> ClientId -> ClientIndexStore m ()
  DeleteClients :: UserId -> ClientIndexStore m ()
  UseIntraClientListing :: ClientIndexStore m Bool

makeSem ''ClientIndexStore
