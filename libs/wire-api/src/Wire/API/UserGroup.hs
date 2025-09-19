-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.API.UserGroup
  ( module Wire.API.UserGroup,
    UserGroupId,
  )
where

import Control.Applicative
import Data.Aeson qualified as A
import Data.Id
import Data.Json.Util
import Data.Kind
import Data.OpenApi qualified as OpenApi
import Data.Qualified (Qualified)
import Data.Range
import Data.Schema
import Data.Text qualified as Text
import Data.Vector (Vector)
import Imports
import Servant.API
import Wire.API.User.Profile
import Wire.Arbitrary

newtype UserGroupName = UserGroupName {unUserGroupName :: Range 1 4000 Text}
  deriving (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform UserGroupName
  deriving (A.ToJSON, A.FromJSON, OpenApi.ToSchema) via Schema UserGroupName

userGroupNameFromText :: Text -> Either Text UserGroupName
userGroupNameFromText = mapLeft Text.pack . fmap UserGroupName . (checkedEither @Text @1 @4000)

userGroupNameToText :: UserGroupName -> Text
userGroupNameToText = fromRange . unUserGroupName

instance ToSchema UserGroupName where
  schema = UserGroupName <$> unUserGroupName .= schema

instance OpenApi.ToParamSchema UserGroupName

instance FromHttpApiData UserGroupName where
  parseUrlPiece = userGroupNameFromText

instance ToHttpApiData UserGroupName where
  toUrlPiece = userGroupNameToText

data NewUserGroup = NewUserGroup
  { name :: UserGroupName,
    members :: Vector UserId
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform NewUserGroup
  deriving (A.ToJSON, A.FromJSON, OpenApi.ToSchema) via Schema NewUserGroup

instance ToSchema NewUserGroup where
  schema =
    object "NewUserGroup" $
      NewUserGroup
        <$> (.name) .= field "name" schema
        <*> (.members) .= field "members" (vector schema)

data UserGroupUpdate = UserGroupUpdate
  { name :: UserGroupName
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform UserGroupUpdate
  deriving (A.ToJSON, A.FromJSON, OpenApi.ToSchema) via Schema UserGroupUpdate

instance ToSchema UserGroupUpdate where
  schema =
    object "UserGroupUpdate" $
      UserGroupUpdate
        <$> (.name) .= field "name" schema

newtype UserGroupAddUsers = UserGroupAddUsers
  { members :: Vector UserId
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform UserGroupAddUsers
  deriving (A.ToJSON, A.FromJSON, OpenApi.ToSchema) via Schema UserGroupAddUsers

instance ToSchema UserGroupAddUsers where
  schema =
    object "UserGroupAddUsers" $
      UserGroupAddUsers
        <$> (.members) .= field "members" (vector schema)

type UserGroup = UserGroup_ Identity

type UserGroupMeta = UserGroup_ (Const ())

userGroupToMeta :: UserGroup -> UserGroupMeta
userGroupToMeta ug =
  UserGroup_
    { id_ = ug.id_,
      name = ug.name,
      members = Const (),
      channels = Const (),
      membersCount = ug.membersCount,
      channelsCount = ug.channelsCount,
      managedBy = ug.managedBy,
      createdAt = ug.createdAt
    }

data UserGroup_ (f :: Type -> Type) = UserGroup_
  { id_ :: UserGroupId,
    name :: UserGroupName,
    members :: f (Vector UserId),
    channels :: f (Maybe (Vector (Qualified ConvId))),
    membersCount :: Maybe Int,
    channelsCount :: Maybe Int,
    managedBy :: ManagedBy,
    createdAt :: UTCTimeMillis
  }
  deriving (Generic)

deriving instance Eq (UserGroup_ (Const ()))

deriving instance Ord (UserGroup_ (Const ()))

deriving instance Show (UserGroup_ (Const ()))

deriving via GenericUniform (UserGroup_ (Const ())) instance Arbitrary (UserGroup_ (Const ()))

deriving via Schema (UserGroup_ (Const ())) instance A.ToJSON (UserGroup_ (Const ()))

deriving via Schema (UserGroup_ (Const ())) instance A.FromJSON (UserGroup_ (Const ()))

deriving via Schema (UserGroup_ (Const ())) instance OpenApi.ToSchema (UserGroup_ (Const ()))

instance ToSchema (UserGroup_ (Const ())) where
  schema =
    object "UserGroupMeta" $
      UserGroup_
        <$> (.id_) .= field "id" schema
        <*> (.name) .= field "name" schema
        <*> (.members) .= pure mempty
        <*> (.channels) .= pure mempty
        <*> (.membersCount) .= maybe_ (optField "membersCount" schema)
        <*> (.channelsCount) .= maybe_ (optField "channelsCount" schema)
        <*> (.managedBy) .= field "managedBy" schema
        <*> (.createdAt) .= field "createdAt" schema

deriving instance Eq (UserGroup_ Identity)

deriving instance Ord (UserGroup_ Identity)

deriving instance Show (UserGroup_ Identity)

deriving via GenericUniform (UserGroup_ Identity) instance Arbitrary (UserGroup_ Identity)

deriving via Schema (UserGroup_ Identity) instance A.ToJSON (UserGroup_ Identity)

deriving via Schema (UserGroup_ Identity) instance A.FromJSON (UserGroup_ Identity)

deriving via Schema (UserGroup_ Identity) instance OpenApi.ToSchema (UserGroup_ Identity)

instance ToSchema (UserGroup_ Identity) where
  schema =
    object "UserGroup" $
      UserGroup_
        <$> (.id_) .= field "id" schema
        <*> (.name) .= field "name" schema
        <*> (runIdentity . (.members)) .= field "members" (Identity <$> vector schema)
        <*> (runIdentity . (.channels)) .= (Identity <$> maybe_ (optField "channels" (vector schema)))
        <*> (.membersCount) .= maybe_ (optField "membersCount" schema)
        <*> (.channelsCount) .= maybe_ (optField "channelsCount" schema)
        <*> (.managedBy) .= field "managedBy" schema
        <*> (.createdAt) .= field "createdAt" schema

newtype UpdateUserGroupMembers = UpdateUserGroupMembers
  { members :: Vector UserId
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform UpdateUserGroupMembers
  deriving (A.ToJSON, A.FromJSON, OpenApi.ToSchema) via Schema UpdateUserGroupMembers

instance ToSchema UpdateUserGroupMembers where
  schema =
    object "UpdateUserGroupMembers" $
      UpdateUserGroupMembers
        <$> (.members) .= field "members" (vector schema)

newtype UpdateUserGroupChannels = UpdateUserGroupChannels
  { channels :: Vector (Qualified ConvId)
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform UpdateUserGroupChannels
  deriving (A.ToJSON, A.FromJSON, OpenApi.ToSchema) via Schema UpdateUserGroupChannels

instance ToSchema UpdateUserGroupChannels where
  schema =
    object "UpdateUserGroupChannels" $
      UpdateUserGroupChannels
        <$> (.channels) .= field "channels" (vector schema)

newtype CheckUserGroupName = CheckUserGroupName
  { name :: UserGroupName
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform CheckUserGroupName
  deriving (A.ToJSON, A.FromJSON, OpenApi.ToSchema) via Schema CheckUserGroupName

instance ToSchema CheckUserGroupName where
  schema =
    object "CheckUserGroupName" $
      CheckUserGroupName
        <$> (.name) .= field "name" schema

newtype UserGroupNameAvailability = UserGroupNameAvailability
  { available :: Bool
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform UserGroupNameAvailability
  deriving (A.ToJSON, A.FromJSON, OpenApi.ToSchema) via Schema UserGroupNameAvailability

instance ToSchema UserGroupNameAvailability where
  schema =
    object "UserGroupNameAvailability" $
      UserGroupNameAvailability
        <$> (.available) .= field "name_available" schema
