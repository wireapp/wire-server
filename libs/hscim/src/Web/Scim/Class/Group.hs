{-# LANGUAGE AllowAmbiguousTypes #-}

module Web.Scim.Class.Group
  ( GroupSite (..),
    GroupDB (..),
    GroupTypes (..),
    StoredGroup,
    Group (..),
    Member (..),
    groupServer,
  )
where

import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Text
import GHC.Generics (Generic)
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import Web.Scim.Class.Auth
import Web.Scim.ContentType
import Web.Scim.Handler
import Web.Scim.Schema.Common
import Web.Scim.Schema.ListResponse
import Web.Scim.Schema.Meta

----------------------------------------------------------------------------
-- /Groups API

type Schema = Text

-- | Configurable parts of 'Group'.
class GroupTypes tag where
  -- | Group ID type.
  type GroupId tag

-- TODO
data Member
  = Member
      { value :: Text,
        typ :: Text,
        ref :: Text
      }
  deriving (Show, Eq, Generic)

instance FromJSON Member where
  parseJSON = genericParseJSON parseOptions . jsonLower

instance ToJSON Member where
  toJSON = genericToJSON serializeOptions

data Group
  = Group
      { schemas :: [Schema],
        displayName :: Text,
        members :: [Member]
      }
  deriving (Show, Eq, Generic)

instance FromJSON Group where
  parseJSON = genericParseJSON parseOptions . jsonLower

instance ToJSON Group where
  toJSON = genericToJSON serializeOptions

type StoredGroup tag = WithMeta (WithId (GroupId tag) Group)

data GroupSite tag route
  = GroupSite
      { gsGetGroups ::
          route
            :- Get '[SCIM] (ListResponse (StoredGroup tag)),
        gsGetGroup ::
          route
            :- Capture "id" (GroupId tag)
            :> Get '[SCIM] (StoredGroup tag),
        gsPostGroup ::
          route
            :- ReqBody '[SCIM] Group
            :> PostCreated '[SCIM] (StoredGroup tag),
        gsPutGroup ::
          route
            :- Capture "id" (GroupId tag)
            :> ReqBody '[SCIM] Group
            :> Put '[SCIM] (StoredGroup tag),
        gsPatchGroup ::
          route
            :- Capture "id" (GroupId tag)
            :> ReqBody '[SCIM] Aeson.Value
            :> Patch '[SCIM] (StoredGroup tag),
        gsDeleteGroup ::
          route
            :- Capture "id" (GroupId tag)
            :> DeleteNoContent '[SCIM] NoContent
      }
  deriving (Generic)

----------------------------------------------------------------------------
-- Methods used by the API

class (Monad m, GroupTypes tag, AuthDB tag m) => GroupDB tag m where
  -- | Get all groups.
  getGroups ::
    AuthInfo tag ->
    ScimHandler m (ListResponse (StoredGroup tag))

  -- | Get a single group by ID.
  --
  -- Should throw 'notFound' if the group does not.
  getGroup ::
    AuthInfo tag ->
    GroupId tag ->
    ScimHandler m (StoredGroup tag)

  -- | Create a new group.
  --
  -- Should throw 'conflict' if uniqueness constraints are violated.
  postGroup ::
    AuthInfo tag ->
    Group ->
    ScimHandler m (StoredGroup tag)

  -- | Overwrite an existing group.
  --
  -- Should throw 'notFound' if the group does not exist, and 'conflict' if uniqueness
  -- constraints are violated.
  putGroup ::
    AuthInfo tag ->
    GroupId tag ->
    Group ->
    ScimHandler m (StoredGroup tag)

  -- | Modify an existing group.
  --
  -- Should throw 'notFound' if the group doesn't exist, and 'conflict' if uniqueness
  -- constraints are violated.
  --
  -- FUTUREWORK: add types for PATCH (instead of 'Aeson.Value').
  -- See <https://tools.ietf.org/html/rfc7644#section-3.5.2>
  patchGroup ::
    AuthInfo tag ->
    GroupId tag ->
    -- | PATCH payload
    Aeson.Value ->
    ScimHandler m (StoredGroup tag)

  -- | Delete a group.
  --
  -- Should throw 'notFound' if the group does not exist.
  deleteGroup ::
    AuthInfo tag ->
    GroupId tag ->
    ScimHandler m ()

----------------------------------------------------------------------------
-- API handlers

groupServer ::
  forall tag m.
  (Show (GroupId tag), GroupDB tag m) =>
  Maybe (AuthData tag) ->
  GroupSite tag (AsServerT (ScimHandler m))
groupServer authData =
  GroupSite
    { gsGetGroups = do
        auth <- authCheck @tag authData
        getGroups @tag auth,
      gsGetGroup = \gid -> do
        auth <- authCheck @tag authData
        getGroup @tag auth gid,
      gsPostGroup = \gr -> do
        auth <- authCheck @tag authData
        postGroup @tag auth gr,
      gsPutGroup = \gid gr -> do
        auth <- authCheck @tag authData
        putGroup @tag auth gid gr,
      gsPatchGroup = \gid patch -> do
        auth <- authCheck @tag authData
        patchGroup @tag auth gid patch,
      gsDeleteGroup = \gid -> do
        auth <- authCheck @tag authData
        deleteGroup @tag auth gid
        pure NoContent
    }
