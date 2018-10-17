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
import           Servant
import           Servant.Generic


type StoredUser = WithMeta (WithId User)

-- TODO: parameterize UserId
class Monad m => UserDB m where
  list     :: Maybe Filter -> SCIMHandler m (ListResponse StoredUser)
  get      :: UserId -> SCIMHandler m (Maybe StoredUser)
  create   :: User -> SCIMHandler m StoredUser
  update   :: UserId -> User -> SCIMHandler m StoredUser
  patch    :: UserId -> SCIMHandler m StoredUser
  delete   :: UserId -> SCIMHandler m Bool  -- ^ Return 'False' if the user didn't exist
  getMeta  :: SCIMHandler m Meta


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

userServer :: UserDB m => UserSite (AsServerT (SCIMHandler m))
userServer = UserSite
  { getUsers   = list
  , getUser    = getUser'
  , postUser   = postUser'
  , putUser    = updateUser'
  , patchUser  = patch
  , deleteUser = deleteUser'
  }

postUser' :: UserDB m => User -> SCIMHandler m StoredUser
postUser' user = do
  -- Find users with the same username (case-insensitive)
  --
  -- TODO: it might be worth it to let 'create' handle conflicts if it can
  -- do it in one pass instead of two passes. Same applies to 'updateUser''
  -- and similar functions.
  let filter_ = FilterAttrCompare AttrUserName OpEq (ValString (userName user))
  stored <- list (Just filter_)
  when (totalResults stored > 0) $
    throwSCIM conflict
  create user

updateUser' :: UserDB m => UserId -> User -> SCIMHandler m StoredUser
updateUser' uid updatedUser = do
  stored <- get uid
  case stored of
    Just (WithMeta _meta (WithId _ existing)) -> do
      let newUser = existing `overwriteWith` updatedUser
      update uid newUser
    Nothing -> throwSCIM (notFound "User" uid)

getUser' :: UserDB m => UserId -> SCIMHandler m StoredUser
getUser' uid = do
  maybeUser <- get uid
  maybe (throwSCIM (notFound "User" uid)) pure maybeUser

deleteUser' :: UserDB m => UserId -> SCIMHandler m NoContent
deleteUser' uid = do
  deleted <- delete uid
  unless deleted $ throwSCIM (notFound "User" uid)
  pure NoContent

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
