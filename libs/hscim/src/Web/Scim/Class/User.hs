{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

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

module Web.Scim.Class.User
  ( UserDB (..),
    StoredUser,
    UserSite (..),
    userServer,
  )
where

import Data.Aeson.Types (FromJSON)
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import Web.Scim.Class.Auth
import Web.Scim.ContentType
import Web.Scim.Filter
import Web.Scim.Handler
import Web.Scim.Schema.Common
import Web.Scim.Schema.Error
import Web.Scim.Schema.ListResponse hiding (schemas)
import Web.Scim.Schema.Meta
import Web.Scim.Schema.PatchOp
import Web.Scim.Schema.User

----------------------------------------------------------------------------
-- /Users API

type StoredUser tag = WithMeta (WithId (UserId tag) (User tag))

data UserSite tag route = UserSite
  { usGetUsers ::
      route
        :- QueryParam "filter" Filter
        :> UVerb
             'GET
             '[SCIM]
             [ WithStatus 200 (ListResponse (StoredUser tag)),
               BadRequest
             ],
    usGetUser ::
      route
        :- Capture "id" (UserId tag)
        :> UVerb
             'GET
             '[SCIM]
             [ WithStatus 200 (StoredUser tag),
               NotFound
             ],
    usPostUser ::
      route
        :- ReqBody '[SCIM] (User tag)
        :> PostCreated '[SCIM] (StoredUser tag),
    usPutUser ::
      route
        :- Capture "id" (UserId tag)
        :> ReqBody '[SCIM] (User tag)
        :> Put '[SCIM] (StoredUser tag),
    usPatchUser ::
      route
        :- Capture "id" (UserId tag)
        :> ReqBody '[SCIM] (PatchOp tag)
        :> Patch '[SCIM] (StoredUser tag),
    usDeleteUser ::
      route
        :- Capture "id" (UserId tag)
        :> DeleteNoContent
  }
  deriving (Generic)

----------------------------------------------------------------------------
-- Methods used by the API

class (Monad m, AuthTypes tag, UserTypes tag) => UserDB tag (protoM :: * -> * -> *) where
  -- | Get all users, optionally filtered by a 'Filter'.
  getUsers ::
    AuthInfo tag ->
    Maybe Filter ->
    ScimHandler
      m
      ( Union
          '[ WithStatus 200 (ListResponse (StoredUser tag)),
             BadRequest
           ]
      )

  -- | Get a single user by ID.
  --
  -- Should throw 'notFound' if the user doesn't exist.
  getUser ::
    AuthInfo tag ->
    UserId tag ->
    ScimHandler
      m
      ( Union
          '[ WithStatus 200 (StoredUser tag),
             NotFound
           ]
      )

  -- | Create a new user.
  --
  -- Should throw 'conflict' if uniqueness constraints are violated.
  postUser ::
    AuthInfo tag ->
    User tag ->
    ScimHandler m (StoredUser tag)

  -- | Overwrite an existing user.
  --
  -- Should throw 'notFound' if the user doesn't exist, and 'conflict' if
  -- uniqueness constraints are violated.
  putUser ::
    AuthInfo tag ->
    UserId tag ->
    User tag ->
    ScimHandler m (StoredUser tag)

  -- | Modify an existing user.
  --
  -- Should throw 'notFound' if the user doesn't exist, and 'conflict' if
  -- uniqueness constraints are violated.
  --
  --  https://tools.ietf.org/html/rfc7644#section-3.5.2
  --
  --    If the target location already contains the value specified, no changes
  --    SHOULD be made to the resource, and a success response SHOULD be
  --    returned.  Unless other operations change the resource, this operation
  --    SHALL NOT change the modify timestamp of the resource.
  --
  --  Given that PUT has the same constraints, we can implement PATCH in terms
  --  of some magic in this library, GET and PUT.
  --
  --  SCIM's Patch semantics are hard to get right. So we advice using the
  --  library built-in implementation.  we implement PATCH in terms of a GET
  --  followed by a PUT.  GET will retrieve the entire record; we then modify
  --  this record by a series of PATCH operations, and then PUT the entire
  --  record.
  patchUser ::
    AuthInfo tag ->
    UserId tag ->
    -- | PATCH payload
    PatchOp tag ->
    ScimHandler m (StoredUser tag)

  -- default patchUser ::
  --   (Patchable (UserExtra tag), FromJSON (UserExtra tag)) =>
  --   AuthInfo tag ->
  --   UserId tag ->
  --   -- | PATCH payload
  --   PatchOp tag ->
  --   ScimHandler m (StoredUser tag)
  -- patchUser info uid op' = do
  --   -- https://hackage.haskell.org/package/servant-0.18.2/docs/Servant-API-UVerb-Union.html#v:matchUnion
  --   (WithMeta _ (WithId _ (user :: User tag))) <- getUser info uid
  --   (newUser :: User tag) <- applyPatch user op'
  --   putUser info uid newUser

  -- | Delete a user.
  --
  -- Should throw 'notFound' if the user doesn't exist.
  deleteUser ::
    AuthInfo tag ->
    UserId tag ->
    ScimHandler m ()

----------------------------------------------------------------------------
-- API handlers

userServer ::
  forall tag m.
  (AuthDB tag m, UserDB tag m) =>
  Maybe (AuthData tag) ->
  UserSite tag (AsServerT (ScimHandler m))
userServer authData =
  UserSite
    { usGetUsers = \mbFilter -> do
        auth <- authCheck @tag authData
        getUsers @tag auth mbFilter,
      usGetUser = \uid -> do
        auth <- authCheck @tag authData
        getUser @tag auth uid,
      usPostUser = \user -> do
        auth <- authCheck @tag authData
        postUser @tag auth user,
      usPutUser = \uid user -> do
        auth <- authCheck @tag authData
        putUser @tag auth uid user,
      usPatchUser = \uid patch -> do
        auth <- authCheck @tag authData
        patchUser @tag @m auth uid patch,
      usDeleteUser = \uid -> do
        auth <- authCheck @tag authData
        deleteUser @tag auth uid
        pure NoContent
    }
