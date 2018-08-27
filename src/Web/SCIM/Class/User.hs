{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ConstraintKinds   #-}

module Web.SCIM.Class.User
    ( UserDB (..)
    , StoredUser
    , UpdateError (..)
    , UserSite (..)
    , userServer
    ) where

import           Control.Applicative ((<|>), Alternative)
import           Control.Monad.Except
import           Control.Error.Util (note)
import           Data.Text
import           GHC.Generics (Generic)
import           Web.SCIM.Schema.User hiding (schemas)
import           Web.SCIM.Schema.Meta
import           Web.SCIM.Schema.Common
import           Web.SCIM.Schema.Error
import           Web.SCIM.Schema.ListResponse
import           Web.SCIM.ContentType
import           Servant
import           Servant.Generic


type UserHandler m = (MonadError ServantErr m, UserDB m)

type StoredUser = WithMeta (WithId User)

data UpdateError = NonExisting
                 | Mutability


-- TODO: parameterize UserId
class UserDB m where
  list     :: m (ListResponse StoredUser)
  get      :: UserId -> m (Maybe StoredUser)
  create   :: User -> m StoredUser
  update   :: UserId -> User -> m (Either UpdateError StoredUser)
  patch    :: UserId -> m StoredUser
  delete   :: UserId -> m Bool
  getMeta  :: m Meta


data UserSite route = UserSite
  { getUsers :: route :-
      Get '[SCIM] (ListResponse StoredUser)
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

userServer :: UserHandler m => UserSite (AsServerT m)
userServer = UserSite
  { getUsers   = list
  , getUser    = getUser'
  , postUser   = create
  , putUser    = updateUser'
  , patchUser  = patch
  , deleteUser = deleteUser'
  }


updateUser' :: UserHandler m => UserId -> User -> m StoredUser
updateUser' uid updatedUser = do
  -- TODO: don't fetch here, let User.update do it
  stored <- get uid
  case stored of
    Just (WithMeta _meta (WithId _ existing)) ->
      let newUser = existing `overwriteWith` updatedUser
      in do
        t <- update uid newUser
        case t of
          Left _err -> throwError err400
          Right newStored -> pure newStored
    Nothing -> throwError err400

getUser' :: UserHandler m => UserId -> m StoredUser
getUser' uid = do
  maybeUser <- get uid
  either throwError pure $ note (notFound uid) maybeUser

deleteUser' :: UserHandler m => UserId -> m NoContent
deleteUser' uid = do
    deleted <- delete uid
    if deleted then return NoContent else throwError err404

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
