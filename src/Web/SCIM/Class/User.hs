module Web.SCIM.Class.User
    ( UserDB (..)
    , StoredUser
    , UserSite (..)
    , userServer
    ) where

import           Control.Applicative ((<|>), Alternative)
import           Control.Monad
import           Data.Text
import           GHC.Generics (Generic)
import           Web.SCIM.Schema.User hiding (schemas)
import           Web.SCIM.Schema.Meta
import           Web.SCIM.Schema.Common
import           Web.SCIM.Schema.Error
import           Web.SCIM.Schema.ListResponse hiding (schemas)
import           Web.SCIM.Handler
import           Web.SCIM.Filter
import           Web.SCIM.ContentType
import           Web.SCIM.Class.Auth
import           Servant
import           Servant.Generic

----------------------------------------------------------------------------
-- /Users API

type StoredUser = WithMeta (WithId User)

data UserSite route = UserSite
  { getUsers :: route :-
      QueryParam "filter" Filter :> Get '[SCIM] (ListResponse StoredUser)
  , getUser :: route :-
      Capture "id" Text :> Get '[SCIM] StoredUser
  , postUser :: route :-
      ReqBody '[SCIM] User :> PostCreated '[SCIM] StoredUser
  , putUser :: route :-
      Capture "id" Text :> ReqBody '[SCIM] User :> Put '[SCIM] StoredUser
  , patchUser :: route :-
      Capture "id" Text :> Patch '[SCIM] StoredUser
  , deleteUser :: route :-
      Capture "id" Text :> DeleteNoContent '[SCIM] NoContent
  } deriving (Generic)

----------------------------------------------------------------------------
-- Methods used by the API

-- TODO: parameterize UserId
class (Monad m, AuthDB m) => UserDB m where
  list :: AuthInfo m -> Maybe Filter -> SCIMHandler m (ListResponse StoredUser)
  get :: AuthInfo m -> UserId -> SCIMHandler m (Maybe StoredUser)
  create :: AuthInfo m -> User -> SCIMHandler m StoredUser
  update :: AuthInfo m -> UserId -> User -> SCIMHandler m StoredUser
  delete :: AuthInfo m -> UserId -> SCIMHandler m Bool  -- ^ Return 'False' if the user didn't exist
  getMeta :: AuthInfo m -> SCIMHandler m Meta

----------------------------------------------------------------------------
-- API handlers

userServer
    :: UserDB m
    => Maybe (AuthData m) -> UserSite (AsServerT (SCIMHandler m))
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
    => AuthInfo m -> Maybe Filter -> SCIMHandler m (ListResponse StoredUser)
getUsers' auth mbFilter = do
  list auth mbFilter

getUser'
    :: UserDB m
    => AuthInfo m -> UserId -> SCIMHandler m StoredUser
getUser' auth uid = do
  maybeUser <- get auth uid
  maybe (throwSCIM (notFound "User" uid)) pure maybeUser

postUser'
    :: UserDB m
    => AuthInfo m -> User -> SCIMHandler m StoredUser
postUser' auth user = do
  -- Find users with the same username (case-insensitive)
  --
  -- TODO: it might be worth it to let 'create' handle conflicts if it can
  -- do it in one pass instead of two passes. Same applies to 'updateUser''
  -- and similar functions.
  let filter_ = FilterAttrCompare AttrUserName OpEq (ValString (userName user))
  stored <- list auth (Just filter_)
  when (totalResults stored > 0) $
    throwSCIM conflict
  create auth user

putUser'
    :: UserDB m
    => AuthInfo m -> UserId -> User -> SCIMHandler m StoredUser
putUser' auth uid updatedUser = do
  stored <- get auth uid
  case stored of
    Just (WithMeta _meta (WithId _ existing)) -> do
      let newUser = existing `overwriteWith` updatedUser
      update auth uid newUser
    Nothing -> throwSCIM (notFound "User" uid)

deleteUser'
    :: UserDB m
    => AuthInfo m -> UserId -> SCIMHandler m NoContent
deleteUser' auth uid = do
  deleted <- delete auth uid
  unless deleted $ throwSCIM (notFound "User" uid)
  pure NoContent

----------------------------------------------------------------------------
-- Utilities

overwriteWith :: User -> User -> User
overwriteWith old new = old
  { --externalId :: Unsettable Text
    name = merge name
  , displayName = merge displayName
  , nickName = merge nickName
  , profileUrl = merge profileUrl
  , title = merge title
  , userType = merge userType
  , preferredLanguage = merge preferredLanguage
  , locale = merge locale
  , active = merge active
  , password = merge password
  , emails = mergeList emails
  , phoneNumbers = mergeList phoneNumbers
  , ims = mergeList ims
  , photos = mergeList photos
  , addresses = mergeList addresses
  , entitlements = mergeList entitlements
  , roles = mergeList roles
  , x509Certificates = mergeList x509Certificates
  }
  where
    merge :: (Alternative f) => (User -> f a) -> f a
    merge accessor = (accessor new) <|> (accessor old)

    mergeList :: (User -> Maybe [a]) -> Maybe [a]
    mergeList accessor = case accessor new of
      Just [] -> accessor old
      _ -> (accessor new) <|> (accessor old)
