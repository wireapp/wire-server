{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds       #-}
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
  ) where

import           Control.Monad.Except
import           Control.Error.Util (note)
import           Data.Text
import           Data.Aeson
import           GHC.Generics (Generic)
import           Web.SCIM.Schema.Common
import           Web.SCIM.Schema.Error
import           Web.SCIM.Schema.Meta
import           Web.SCIM.ContentType
import           Servant
import           Servant.Generic


type GroupHandler m = (MonadError ServantErr m, GroupDB m)

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

instance FromJSON Group where
  parseJSON = genericParseJSON parseOptions . jsonLower

instance ToJSON Group where
  toJSON = genericToJSON serializeOptions

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
        Get '[SCIM] [StoredGroup]
  , getGroup :: route :-
        Capture "id" Text :> Get '[SCIM] StoredGroup
  , postGroup :: route :-
        ReqBody '[SCIM] Group :> PostCreated '[SCIM] StoredGroup
  , putGroup :: route :-
      Capture "id" Text :> ReqBody '[SCIM] Group :> Put '[SCIM] StoredGroup
  , patchGroup :: route :-
      Capture "id" Text :> Patch '[SCIM] StoredGroup
  , deleteGroup :: route :-
      Capture "id" Text :> DeleteNoContent '[SCIM] NoContent
  } deriving (Generic)

groupServer :: GroupHandler m => GroupSite (AsServerT m)
groupServer = GroupSite
  { getGroups = list
  , getGroup = getGroup'
  , postGroup = create
  , putGroup = putGroup'
  , patchGroup = undefined
  , deleteGroup = deleteGroup'
  }

getGroup' :: GroupHandler m => GroupId -> m StoredGroup
getGroup' gid = do
  maybeGroup <- get gid
  either throwError pure $ note (notFound gid) maybeGroup

putGroup' :: GroupHandler m => GroupId -> Group -> m StoredGroup
putGroup' gid grp = do
  updated <- update gid grp
  either throwError pure updated

deleteGroup' :: GroupHandler m => GroupId -> m NoContent
deleteGroup' gid = do
    deleted <- delete gid
    if deleted then return NoContent else throwError err404
