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

module Wire.AppStore where

import Data.Aeson
import Data.Id
import Data.UUID
import Imports
import Polysemy
import Wire.API.PostgresMarshall

data StoredApp = StoredApp
  { id :: UserId,
    teamId :: TeamId,
    meta :: Object
  }
  deriving (Eq, Ord, Show)

instance PostgresMarshall StoredApp (UUID, UUID, Value) where
  postgresMarshall app =
    ( postgresMarshall app.id,
      postgresMarshall app.teamId,
      postgresMarshall app.meta
    )

instance PostgresUnmarshall (UUID, UUID, Value) StoredApp where
  postgresUnmarshall (uid, teamId, meta) =
    StoredApp
      <$> postgresUnmarshall uid
      <*> postgresUnmarshall teamId
      <*> postgresUnmarshall meta

data AppStore m a where
  CreateApp :: StoredApp -> AppStore m ()
  GetApp :: UserId -> AppStore m (Maybe StoredApp)

makeSem ''AppStore
