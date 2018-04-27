{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ConstraintKinds   #-}

module Web.SCIM.Class.Group (
  GroupSite (..)
  , GroupDB (..)
  , StoredGroup
  , Group (..)
  , GroupId
  , Member (..)
  , groupServer
  , app
  ) where

import           Control.Monad.Except
import           Control.Error.Util (note)
import           Data.Text
import           Data.Aeson
import           GHC.Generics (Generic)
import           Network.Wai
import           Web.SCIM.Schema.Common
import           Web.SCIM.Schema.Error
import           Web.SCIM.Schema.Meta
import           Servant
import           Servant.Generic


type SCIMHandler m = (MonadError ServantErr m, GroupDB m)
type GroupAPI = ToServant (GroupSite AsApi)

type GroupId = Text

type Schema = Text

-- TODO
data Member = Member
  { value :: Text
  , typ :: Text
  , ref :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON Member where
  parseJSON = genericParseJSON parseOptions . jsonLower

instance ToJSON Member where
  toJSON = genericToJSON serializeOptions

data Group = Group
  { schemas :: [Schema]
  , displayName :: Text
  , members :: [Member]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Group
instance ToJSON Group

type StoredGroup = WithMeta (WithId Group)

class GroupDB m where
  list :: m [StoredGroup]
  get :: GroupId -> m (Maybe StoredGroup)
  create :: Group -> m StoredGroup
  update :: GroupId -> Group -> m (Either ServantErr StoredGroup)
  --                                      ^
  -- TODO: should be UpdateError         /
  delete :: GroupId -> m Bool
  getGroupMeta :: m Meta

data GroupSite route = GroupSite
  { getGroups :: route :-
        Get '[JSON] [StoredGroup]
  , getGroup :: route :-
        Capture "id" Text :> Get '[JSON] StoredGroup
  , postGroup :: route :-
        ReqBody '[JSON] Group :> PostCreated '[JSON] StoredGroup
  , putGroup :: route :-
      Capture "id" Text :> ReqBody '[JSON] Group :> Put '[JSON] StoredGroup
  , patchGroup :: route :-
      Capture "id" Text :> Patch '[JSON] StoredGroup
  , deleteGroup :: route :-
      Capture "id" Text :> DeleteNoContent '[JSON] NoContent
  } deriving (Generic)

api :: Proxy GroupAPI
api = Proxy

app :: SCIMHandler m =>
       (forall a. m a -> Handler a) -> Application
app t = serve api $ hoistServer api t (toServant $ groupServer)

groupServer :: SCIMHandler m => GroupSite (AsServerT m)
groupServer = GroupSite
  { getGroups = list
  , getGroup = getGroup'
  , postGroup = create
  , putGroup = putGroup'
  , patchGroup = undefined
  , deleteGroup = deleteGroup'
  }

getGroup' :: SCIMHandler m => GroupId -> m StoredGroup
getGroup' gid = do
  maybeGroup <- get gid
  liftEither $ note (notFound gid) maybeGroup

putGroup' :: SCIMHandler m => GroupId -> Group -> m StoredGroup
putGroup' gid grp = do
  updated <- update gid grp
  liftEither updated

deleteGroup' :: SCIMHandler m => GroupId -> m NoContent
deleteGroup' gid = do
    deleted <- delete gid
    if deleted then return NoContent else throwError err404
