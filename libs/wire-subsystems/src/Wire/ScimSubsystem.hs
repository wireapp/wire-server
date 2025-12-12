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

module Wire.ScimSubsystem where

import Data.Id
import Data.Int
import Data.Maybe
import Polysemy
import Web.Scim.Class.Group qualified as SCG
import Web.Scim.Filter qualified as Scim
import Web.Scim.Schema.ListResponse qualified as Scim
import Wire.API.User.Scim (SparTag)

data ScimSubsystem m a where
  ScimCreateUserGroup :: TeamId -> SCG.Group -> ScimSubsystem m (SCG.StoredGroup SparTag)
  ScimGetUserGroup :: TeamId -> UserGroupId -> ScimSubsystem m (SCG.StoredGroup SparTag)
  ScimUpdateUserGroup :: TeamId -> UserGroupId -> SCG.Group -> ScimSubsystem m (SCG.StoredGroup SparTag)
  ScimDeleteUserGroup :: TeamId -> SCG.GroupId SparTag -> ScimSubsystem m ()
  ScimGetUserGroups :: TeamId -> Maybe Scim.Filter -> Maybe Int -> Maybe Int -> ScimSubsystem m (Scim.ListResponse (SCG.StoredGroup SparTag))

makeSem ''ScimSubsystem
