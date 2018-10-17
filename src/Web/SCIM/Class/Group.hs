module Web.SCIM.Class.Group (
  GroupSite (..)
  , GroupDB (..)
  , StoredGroup
  , Group (..)
  , GroupId
  , Member (..)
  , groupServer
  ) where

import           Control.Monad
import           Data.Text
import           Data.Aeson
import           GHC.Generics (Generic)
import           Web.SCIM.Schema.Common
import           Web.SCIM.Schema.Error
import           Web.SCIM.Schema.Meta
import           Web.SCIM.ContentType
import           Web.SCIM.Handler
import           Servant
import           Servant.Generic

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

class Monad m => GroupDB m where
  list :: SCIMHandler m [StoredGroup]
  get :: GroupId -> SCIMHandler m (Maybe StoredGroup)
  create :: Group -> SCIMHandler m StoredGroup
  update :: GroupId -> Group -> SCIMHandler m StoredGroup
  delete :: GroupId -> SCIMHandler m Bool  -- ^ Return 'False' if the group didn't exist
  getGroupMeta :: SCIMHandler m Meta

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

groupServer :: GroupDB m => GroupSite (AsServerT (SCIMHandler m))
groupServer = GroupSite
  { getGroups = list
  , getGroup = getGroup'
  , postGroup = create
  , putGroup = update
  , patchGroup = error "PATCH /Groups: not implemented"
  , deleteGroup = deleteGroup'
  }

getGroup' :: GroupDB m => GroupId -> SCIMHandler m StoredGroup
getGroup' gid = do
  maybeGroup <- get gid
  maybe (throwSCIM (notFound "Group" gid)) pure maybeGroup

deleteGroup' :: GroupDB m => GroupId -> SCIMHandler m NoContent
deleteGroup' gid = do
  deleted <- delete gid
  unless deleted $ throwSCIM (notFound "Group" gid)
  pure NoContent
