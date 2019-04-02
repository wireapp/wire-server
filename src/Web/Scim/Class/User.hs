{-# LANGUAGE AllowAmbiguousTypes #-}

module Web.Scim.Class.User
    ( UserDB (..)
    , StoredUser
    , UserSite (..)
    , userServer
    ) where

import           GHC.Generics (Generic)
import           Web.Scim.Schema.User
import           Web.Scim.Schema.Meta
import           Web.Scim.Schema.Common
import           Web.Scim.Schema.ListResponse hiding (schemas)
import           Web.Scim.Handler
import           Web.Scim.Filter
import           Web.Scim.ContentType
import           Web.Scim.Class.Auth
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic

import qualified Data.Aeson as Aeson

----------------------------------------------------------------------------
-- /Users API

type StoredUser tag = WithMeta (WithId (UserId tag) (User tag))

data UserSite tag route = UserSite
  { usGetUsers :: route :-
      QueryParam "filter" Filter :>
      Get '[SCIM] (ListResponse (StoredUser tag))
  , usGetUser :: route :-
      Capture "id" (UserId tag) :>
      Get '[SCIM] (StoredUser tag)
  , usPostUser :: route :-
      ReqBody '[SCIM] (User tag) :>
      PostCreated '[SCIM] (StoredUser tag)
  , usPutUser :: route :-
      Capture "id" (UserId tag) :>
      ReqBody '[SCIM] (User tag) :>
      Put '[SCIM] (StoredUser tag)
  , usPatchUser :: route :-
      Capture "id" (UserId tag) :>
      ReqBody '[SCIM] Aeson.Value :>
      Patch '[SCIM] (StoredUser tag)
  , usDeleteUser :: route :-
      Capture "id" (UserId tag) :>
      DeleteNoContent '[SCIM] NoContent
  } deriving (Generic)

----------------------------------------------------------------------------
-- Methods used by the API

class (Monad m, AuthTypes tag, UserTypes tag) => UserDB tag m where
  -- | Get all users, optionally filtered by a 'Filter'.
  getUsers
    :: AuthInfo tag
    -> Maybe Filter
    -> ScimHandler m (ListResponse (StoredUser tag))

  -- | Get a single user by ID.
  --
  -- Should throw 'notFound' if the user doesn't exist.
  getUser
    :: AuthInfo tag
    -> UserId tag
    -> ScimHandler m (StoredUser tag)

  -- | Create a new user.
  --
  -- Should throw 'conflict' if uniqueness constraints are violated.
  postUser
    :: AuthInfo tag
    -> User tag
    -> ScimHandler m (StoredUser tag)

  -- | Overwrite an existing user.
  --
  -- Should throw 'notFound' if the user doesn't exist, and 'conflict' if uniqueness
  -- constraints are violated.
  putUser
    :: AuthInfo tag
    -> UserId tag
    -> User tag
    -> ScimHandler m (StoredUser tag)

  -- | Modify an existing user.
  --
  -- Should throw 'notFound' if the user doesn't exist, and 'conflict' if uniqueness
  -- constraints are violated.
  --
  -- FUTUREWORK: add types for PATCH (instead of 'Aeson.Value').
  -- See <https://tools.ietf.org/html/rfc7644#section-3.5.2>
  patchUser
    :: AuthInfo tag
    -> UserId tag
    -> Aeson.Value  -- ^ PATCH payload
    -> ScimHandler m (StoredUser tag)

  -- | Delete a user.
  --
  -- Should throw 'notFound' if the user doesn't exist.
  deleteUser
    :: AuthInfo tag
    -> UserId tag
    -> ScimHandler m ()

----------------------------------------------------------------------------
-- API handlers

userServer
    :: forall tag m. (AuthDB tag m, UserDB tag m)
    => Maybe (AuthData tag) -> UserSite tag (AsServerT (ScimHandler m))
userServer authData = UserSite
  { usGetUsers = \mbFilter -> do
      auth <- authCheck @tag authData
      getUsers @tag auth mbFilter
  , usGetUser = \uid -> do
      auth <- authCheck @tag authData
      getUser @tag auth uid
  , usPostUser = \user -> do
      auth <- authCheck @tag authData
      postUser @tag auth user
  , usPutUser = \uid user -> do
      auth <- authCheck @tag authData
      putUser @tag auth uid user
  , usPatchUser = \uid patch -> do
      auth <- authCheck @tag authData
      patchUser @tag auth uid patch
  , usDeleteUser = \uid -> do
      auth <- authCheck @tag authData
      deleteUser @tag auth uid
      pure NoContent
  }
