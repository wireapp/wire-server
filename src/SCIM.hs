{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ConstraintKinds   #-}

module SCIM
  ( app
  , SiteAPI
  ) where

import           API.Group (GroupSite (..), GroupDB, groupServer)
import           Config.Schema (ConfigAPI, Configuration, configServer)
import           Control.Applicative ((<|>), Alternative)
import           Control.Monad.Except
import           Control.Error.Util (note)
import           Data.Text
import           DB.User (StoredUser, UserDB)
import qualified DB.User as User
import           GHC.Generics (Generic)
import           Network.Wai
import           Schema.User hiding (schemas)
import           Schema.Meta
import           Schema.Error
import           Schema.ListResponse
import           Servant
import           Servant.Generic


type SCIMHandler m = (MonadError ServantErr m, UserDB m, GroupDB m)
type SiteAPI = ToServant (Site AsApi)

data Site route = Site
  { config :: route :- ToServant (ConfigAPI AsApi)
  , users :: route :- "Users" :> ToServant (UserSite AsApi)
  , groups :: route :- "Groups" :> ToServant (GroupSite AsApi)
  } deriving (Generic)

data UserSite route = UserSite
  { getUsers :: route :-
      Get '[JSON] (ListResponse StoredUser)
  , getUser :: route :-
      Capture "id" Text :> Get '[JSON] StoredUser
  , postUser :: route :-
      ReqBody '[JSON] User :> PostCreated '[JSON] StoredUser
  , putUser :: route :-
      Capture "id" Text :> ReqBody '[JSON] User :> Put '[JSON] StoredUser
  , patchUser :: route :-
      Capture "id" Text :> Patch '[JSON] StoredUser
  , deleteUser :: route :-
      Capture "id" Text :> DeleteNoContent '[JSON] NoContent
  } deriving (Generic)


siteServer :: SCIMHandler m => UserSite (AsServerT m)
siteServer = UserSite
  { getUsers   = User.list
  , getUser    = getUser'
  , postUser   = User.create
  , putUser    = updateUser'
  , patchUser  = User.patch
  , deleteUser = deleteUser'
  }

superServer :: Configuration -> SCIMHandler m => Site (AsServerT m)
superServer conf = Site
  { config = toServant $ configServer conf
  , users = toServant siteServer
  , groups = toServant groupServer
  }

updateUser' :: SCIMHandler m => UserId -> User -> m StoredUser
updateUser' uid update = do
  -- TODO: don't fetch here, let User.update do it
  stored <- User.get uid
  case stored of
    Just (WithMeta _meta (WithId _ existing)) ->
      let newUser = existing `overwriteWith` update
      in do
        t <- User.update uid newUser
        case t of
          Left _err -> throwError err400
          Right newStored -> pure newStored
    Nothing -> throwError err400

getUser' :: SCIMHandler m => UserId -> m StoredUser
getUser' uid = do
  maybeUser <- User.get uid
  liftEither $ note (notFound uid) maybeUser

deleteUser' :: SCIMHandler m => UserId -> m NoContent
deleteUser' uid = do
    deleted <- User.delete uid
    if deleted then return NoContent else throwError err404

api :: Proxy SiteAPI
api = Proxy

app :: SCIMHandler m => Configuration -> (forall a. m a -> Handler a) -> Application
app c t = serve (Proxy :: Proxy SiteAPI) $ hoistServer api t (toServant $ superServer c)


-- TODO: move to User.hs
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
