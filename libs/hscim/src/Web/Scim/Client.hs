{-# LANGUAGE AllowAmbiguousTypes #-}

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

module Web.Scim.Client
  ( HasScimClient,

    -- * config
    spConfig,
    getSchemas,
    schema,
    resourceTypes,

    -- * user
    scimClients,
    getUsers,
    getUser,
    postUser,
    putUser,
    patchUser,
    deleteUser,

    -- * group
    getGroups,
    getGroup,
    postGroup,
    putGroup,
    patchGroup,
    deleteGroup,
  )
where

import Control.Exception
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text
import Servant.API
import Servant.Client
import Servant.Client.Generic
import qualified Web.Scim.Capabilities.MetaSchema as MetaSchema
import Web.Scim.Class.Auth
import Web.Scim.Class.Group (Group, GroupId, StoredGroup)
import Web.Scim.Class.User (StoredUser)
import Web.Scim.Filter (Filter)
import Web.Scim.Schema.ListResponse (ListResponse)
import Web.Scim.Schema.PatchOp (PatchOp)
import qualified Web.Scim.Schema.ResourceType as ResourceType
import Web.Scim.Schema.User (User)
import Web.Scim.Schema.UserTypes (UserExtra, UserId)
import Web.Scim.Server

type HasScimClient tag =
  ( AuthTypes tag,
    ToJSON (UserExtra tag),
    FromJSON (UserExtra tag),
    FromJSON (UserId tag),
    FromJSON (GroupId tag),
    ToHttpApiData (AuthData tag),
    ToHttpApiData (UserId tag),
    ToHttpApiData (GroupId tag)
  )

scimClients :: (HasScimClient tag) => ClientEnv -> Site tag (AsClientT IO)
scimClients env = genericClientHoist $ \x -> runClientM x env >>= either throwIO pure

-- config

spConfig ::
  forall tag.
  (HasScimClient tag) =>
  ClientEnv ->
  IO MetaSchema.Configuration
spConfig env = case config @tag (scimClients env) of ((r :<|> _) :<|> (_ :<|> _)) -> r

getSchemas ::
  forall tag.
  (HasScimClient tag) =>
  ClientEnv ->
  IO (ListResponse Value)
getSchemas env = case config @tag (scimClients env) of ((_ :<|> r) :<|> (_ :<|> _)) -> r

schema ::
  forall tag.
  (HasScimClient tag) =>
  ClientEnv ->
  Text ->
  IO Value
schema env = case config @tag (scimClients env) of ((_ :<|> _) :<|> (r :<|> _)) -> r

resourceTypes ::
  forall tag.
  (HasScimClient tag) =>
  ClientEnv ->
  IO (ListResponse ResourceType.Resource)
resourceTypes env = case config @tag (scimClients env) of ((_ :<|> _) :<|> (_ :<|> r)) -> r

-- users

getUsers ::
  (HasScimClient tag) =>
  ClientEnv ->
  Maybe (AuthData tag) ->
  Maybe Filter ->
  IO (ListResponse (StoredUser tag))
getUsers env tok = case users (scimClients env) tok of ((r :<|> (_ :<|> _)) :<|> (_ :<|> (_ :<|> _))) -> r

getUser ::
  (HasScimClient tag) =>
  ClientEnv ->
  Maybe (AuthData tag) ->
  UserId tag ->
  IO (StoredUser tag)
getUser env tok = case users (scimClients env) tok of ((_ :<|> (r :<|> _)) :<|> (_ :<|> (_ :<|> _))) -> r

postUser ::
  (HasScimClient tag) =>
  ClientEnv ->
  Maybe (AuthData tag) ->
  User tag ->
  IO (StoredUser tag)
postUser env tok = case users (scimClients env) tok of ((_ :<|> (_ :<|> r)) :<|> (_ :<|> (_ :<|> _))) -> r

putUser ::
  (HasScimClient tag) =>
  ClientEnv ->
  Maybe (AuthData tag) ->
  UserId tag ->
  User tag ->
  IO (StoredUser tag)
putUser env tok = case users (scimClients env) tok of ((_ :<|> (_ :<|> _)) :<|> (r :<|> (_ :<|> _))) -> r

patchUser ::
  (HasScimClient tag) =>
  ClientEnv ->
  Maybe (AuthData tag) ->
  UserId tag ->
  PatchOp tag ->
  IO (StoredUser tag)
patchUser env tok = case users (scimClients env) tok of ((_ :<|> (_ :<|> _)) :<|> (_ :<|> (r :<|> _))) -> r

deleteUser ::
  forall tag.
  (HasScimClient tag) =>
  ClientEnv ->
  Maybe (AuthData tag) ->
  UserId tag ->
  IO NoContent
deleteUser env tok = case users @tag (scimClients env) tok of ((_ :<|> (_ :<|> _)) :<|> (_ :<|> (_ :<|> r))) -> r

-- groups

getGroups ::
  ClientEnv ->
  Maybe (AuthData tag) ->
  IO (ListResponse (StoredGroup tag))
getGroups = error "groups are not authenticated at the moment; implement that first!"

getGroup ::
  ClientEnv ->
  Maybe (AuthData tag) ->
  GroupId tag ->
  IO (StoredGroup tag)
getGroup = error "groups are not authenticated at the moment; implement that first!"

postGroup ::
  ClientEnv ->
  Maybe (AuthData tag) ->
  Group ->
  IO (StoredGroup tag)
postGroup = error "groups are not authenticated at the moment; implement that first!"

putGroup ::
  ClientEnv ->
  Maybe (AuthData tag) ->
  GroupId tag ->
  IO (StoredGroup tag)
putGroup = error "groups are not authenticated at the moment; implement that first!"

patchGroup ::
  ClientEnv ->
  Maybe (AuthData tag) ->
  GroupId tag ->
  IO (StoredGroup tag)
patchGroup = error "groups are not authenticated at the moment; implement that first!"

deleteGroup ::
  ClientEnv ->
  Maybe (AuthData tag) ->
  GroupId tag ->
  IO DeleteNoContent
deleteGroup = error "groups are not authenticated at the moment; implement that first!"
