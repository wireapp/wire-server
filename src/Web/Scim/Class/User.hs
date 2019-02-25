module Web.Scim.Class.User
    ( UserDB (..)
    , StoredUser
    , UserSite (..)
    , userServer
    ) where

import           Control.Monad
import           Data.Text
import           GHC.Generics (Generic)
import           Web.Scim.Schema.User
import           Web.Scim.Schema.Meta
import           Web.Scim.Schema.Common
import           Web.Scim.Schema.Error
import           Web.Scim.Schema.ListResponse hiding (schemas)
import           Web.Scim.Handler
import           Web.Scim.Filter
import           Web.Scim.ContentType
import           Web.Scim.Class.Auth
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic

----------------------------------------------------------------------------
-- /Users API

type StoredUser extra = WithMeta (WithId (User extra))

data UserSite extra route = UserSite
  { getUsers :: route :-
      QueryParam "filter" Filter :>
      Get '[SCIM] (ListResponse (StoredUser extra))
  , getUser :: route :-
      Capture "id" Text :>
      Get '[SCIM] (StoredUser extra)
  , postUser :: route :-
      ReqBody '[SCIM] (User extra) :>
      PostCreated '[SCIM] (StoredUser extra)
  , putUser :: route :-
      Capture "id" Text :>
      ReqBody '[SCIM] (User extra) :>
      Put '[SCIM] (StoredUser extra)
  , patchUser :: route :-
      Capture "id" Text :>
      Patch '[SCIM] (StoredUser extra)
  , deleteUser :: route :-
      Capture "id" Text :>
      DeleteNoContent '[SCIM] NoContent
  } deriving (Generic)

----------------------------------------------------------------------------
-- Methods used by the API

-- TODO: parameterize UserId
class (Monad m, AuthDB m) => UserDB m where
  type UserExtra m
  list :: AuthInfo m -> Maybe Filter -> ScimHandler m (ListResponse (StoredUser (UserExtra m)))
  get :: AuthInfo m -> UserId -> ScimHandler m (Maybe (StoredUser (UserExtra m)))
  create :: AuthInfo m -> User (UserExtra m) -> ScimHandler m (StoredUser (UserExtra m))
  update :: AuthInfo m -> UserId -> User (UserExtra m) -> ScimHandler m (StoredUser (UserExtra m))
  delete :: AuthInfo m -> UserId -> ScimHandler m Bool  -- ^ Return 'False' if the user didn't exist
  getMeta :: AuthInfo m -> ScimHandler m Meta

----------------------------------------------------------------------------
-- API handlers

userServer
    :: UserDB m
    => Maybe (AuthData m) -> UserSite (UserExtra m) (AsServerT (ScimHandler m))
userServer authData = UserSite
  { getUsers = \mbFilter -> do
      auth <- authCheck authData
      getUsers' auth mbFilter
  , getUser = \uid -> do
      auth <- authCheck authData
      getUser' auth uid
  , postUser = \user -> do
      auth <- authCheck authData
      postUser' auth user
  , putUser = \uid user -> do
      auth <- authCheck authData
      putUser' auth uid user
  , patchUser = error "PATCH /Users: not implemented"
  , deleteUser = \uid -> do
      auth <- authCheck authData
      deleteUser' auth uid
  }

getUsers'
    :: UserDB m
    => AuthInfo m
    -> Maybe Filter
    -> ScimHandler m (ListResponse (StoredUser (UserExtra m)))
getUsers' auth mbFilter = do
  list auth mbFilter

getUser'
    :: UserDB m
    => AuthInfo m
    -> UserId
    -> ScimHandler m (StoredUser (UserExtra m))
getUser' auth uid = do
  maybeUser <- get auth uid
  maybe (throwScim (notFound "User" uid)) pure maybeUser

postUser'
    :: UserDB m
    => AuthInfo m
    -> User (UserExtra m)
    -> ScimHandler m (StoredUser (UserExtra m))
postUser' auth user = do
  -- Find users with the same username (case-insensitive)
  --
  -- TODO: it might be worth it to let 'create' handle conflicts if it can
  -- do it in one pass instead of two passes. Same applies to 'updateUser''
  -- and similar functions.
  let filter_ = FilterAttrCompare AttrUserName OpEq (ValString (userName user))
  stored <- list auth (Just filter_)
  when (totalResults stored > 0) $
    throwScim conflict
  create auth user

-- | Fully update a 'User'.
--
-- Spec: <https://tools.ietf.org/html/rfc7644#section-3.5.1>.
--
-- FUTUREWORK: according to the spec, we should handle cases where someone
-- attempts to overwrite @readOnly@ and @immutable@ attributes. Currently we
-- don't have any such attributes.
--
-- See <https://github.com/wireapp/hscim/issues/21>.
putUser'
    :: UserDB m
    => AuthInfo m
    -> UserId
    -> User (UserExtra m)
    -> ScimHandler m (StoredUser (UserExtra m))
putUser' auth uid user = do
  stored <- get auth uid
  case stored of
    Just _ -> update auth uid user
    Nothing -> throwScim (notFound "User" uid)

deleteUser'
    :: UserDB m
    => AuthInfo m -> UserId -> ScimHandler m NoContent
deleteUser' auth uid = do
  deleted <- delete auth uid
  unless deleted $ throwScim (notFound "User" uid)
  pure NoContent
