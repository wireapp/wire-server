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
import Data.Id
import Data.Json.Util (fromUTCTimeMillis)
import Data.Text qualified as Text
import Data.Vector qualified as V
import Imports
import Network.URI (URI, parseURI)
import Polysemy
import Web.Scim.Class.Auth
import Web.Scim.Class.Group qualified as SCG
import Web.Scim.Handler
import Web.Scim.Schema.Common qualified as Common
import Web.Scim.Schema.Error qualified as ScimErr
import Web.Scim.Schema.ListResponse
import Web.Scim.Schema.Meta qualified as Meta
import Web.Scim.Schema.ResourceType qualified as RT
import Wire.API.User.Scim
import Wire.API.UserGroup
import Wire.UserGroupSubsystem qualified as UserGroup

----------------------------------------------------------------------------
-- GroupDB instance

instance (AuthDB SparTag (Sem r), Member UserGroup.UserGroupSubsystem r) => SCG.GroupDB SparTag (Sem r) where
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
    ScimHandler m (SCG.StoredGroup SparTag)
  getGroup = undefined

  -- \| Create a new group.
  --
  -- Should throw 'conflict' if uniqueness constraints are violated.
  postGroup ::
    AuthInfo SparTag ->
    SCG.Group ->
    ScimHandler (Sem r) (SCG.StoredGroup SparTag)
  postGroup _tok _grp = do
    {-
    let team = tok.stiTeam
    ugName <- case userGroupNameFromText grp.displayName of
      Left e -> throwScim (ScimErr.badRequest ScimErr.InvalidValue (Just e))
      Right n -> pure n
    -- parse member user ids
    uids <- forM grp.members $ \m -> case parseIdFromText m.value of
      Left _ -> throwScim (ScimErr.badRequest ScimErr.InvalidValue (Just "invalid member id"))
      Right (uid :: UserId) -> pure uid
    -- create group
    let newGroup = NewUserGroup {name = ugName, members = V.fromList uids}
    ug <- lift $ UserGroup.createScimGroup team newGroup
    pure $ toStoredGroup ug
    where
      toStoredGroup :: UserGroup -> SCG.StoredGroup SparTag
      toStoredGroup ug =
        let created = ug.createdAt
            mkLocation :: String -> URI
            mkLocation pathSuffix =
              let uri =
                    -- best-effort absolute URI; replaced by real base elsewhere if needed
                    "https://example.com" <> pathSuffix
               in fromMaybe (error "invalid SCIM group location URI") (parseURI uri)
            meta =
              Meta.Meta
                { Meta.resourceType = RT.GroupResource,
                  Meta.created = fromUTCTimeMillis created,
                  Meta.lastModified = fromUTCTimeMillis created,
                  Meta.version = Meta.Weak "v1",
                  Meta.location =
                    Common.URI . mkLocation $
                      "/Groups/" <> Text.unpack (idToText ug.id_)
                }
            groupVal =
              SCG.Group
                { schemas = ["urn:ietf:params:scim:schemas:core:2.0:Group"],
                  displayName = userGroupNameToText ug.name,
                  members =
                    [ SCG.Member
                        { value = idToText uid,
                          typ = "User",
                          ref = "https://example.com/Users/" <> idToText uid
                        }
                      | uid <- toList (runIdentity ug.members)
                    ]
                }
         in Meta.WithMeta meta (Common.WithId () groupVal)

    -}
    undefined

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
