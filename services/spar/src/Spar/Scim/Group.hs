{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

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

module Spar.Scim.Group where

import Data.Aeson qualified as Aeson
import Imports
import Polysemy
import Web.Scim.Class.Auth
import Web.Scim.Class.Group qualified as SCG
import Web.Scim.Handler
import Web.Scim.Schema.ListResponse
import Wire.API.User.Scim
import Wire.ScimSubsystem

----------------------------------------------------------------------------
-- GroupDB instance

instance (AuthDB SparTag (Sem r), Member ScimSubsystem r) => SCG.GroupDB SparTag (Sem r) where
  getGroups ::
    AuthInfo SparTag ->
    ScimHandler m (ListResponse (SCG.StoredGroup SparTag))
  getGroups = undefined

  -- \| Get a single group by ID.
  --
  -- Should throw 'notFound' if the group does not.
  getGroup ::
    AuthInfo SparTag ->
    SCG.GroupId SparTag ->
    ScimHandler (Sem r) (SCG.StoredGroup SparTag)
  getGroup ((.stiTeam) -> tid) gid = lift $ scimGetUserGroup tid gid

  -- \| Create a new group.
  --
  -- Should throw 'conflict' if uniqueness constraints are violated.
  postGroup ::
    AuthInfo SparTag ->
    SCG.Group ->
    ScimHandler (Sem r) (SCG.StoredGroup SparTag)
  postGroup ((.stiTeam) -> team) grp = lift $ scimCreateUserGroup team grp

  -- no additional helpers

  -- \| Overwrite an existing group.
  --
  -- Should throw 'notFound' if the group does not exist, and 'conflict' if uniqueness
  -- constraints are violated.
  putGroup ::
    AuthInfo SparTag ->
    SCG.GroupId SparTag ->
    SCG.Group ->
    ScimHandler m (SCG.StoredGroup SparTag)
  putGroup = undefined

  -- \| Modify an existing group.
  --
  -- Should throw 'notFound' if the group doesn't exist, and 'conflict' if uniqueness
  -- constraints are violated.
  --
  -- FUTUREWORK: add types for PATCH (instead of 'Aeson.Value').
  -- See <https://tools.ietf.org/html/rfc7644#section-3.5.2>
  patchGroup ::
    AuthInfo SparTag ->
    SCG.GroupId SparTag ->
    -- \| PATCH payload
    Aeson.Value ->
    ScimHandler m (SCG.StoredGroup SparTag)
  patchGroup = undefined

  -- \| Delete a group.
  --
  -- Should throw 'notFound' if the group does not exist.
  deleteGroup ::
    AuthInfo SparTag ->
    SCG.GroupId SparTag ->
    ScimHandler m ()
  deleteGroup = undefined
