{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Team.Member
  ( -- * TeamMember
    TeamMember,
    mkTeamMember,
    userId,
    permissions,
    invitation,
    legalHoldStatus,
    ntmNewTeamMember,
    teamMemberJson,
    setOptionalPerms,
    setOptionalPermsMany,
    teamMemberObjectSchema,

    -- * TeamMemberList
    TeamMemberList,
    TeamMemberListOptPerms,
    TeamMembersPage (..),
    TeamMembersPagingState,
    teamMemberPagingState,
    newTeamMemberList,
    teamMembers,
    teamMemberListType,
    HardTruncationLimit,
    hardTruncationLimit,
    NewListType (..),
    toNewListType,
    ListType (..),

    -- * NewTeamMember
    NewTeamMember,
    TeamMemberOptPerms,
    mkNewTeamMember,
    nUserId,
    nPermissions,
    optionalPermissions,
    nInvitation,

    -- * TeamMemberDeleteData
    TeamMemberDeleteData,
    newTeamMemberDeleteData,
    tmdAuthPassword,
  )
where

import Cassandra (PageWithState (..))
import qualified Cassandra as C
import Control.Lens (Lens, Lens', makeLenses, (%~), (?~))
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import qualified Data.ByteString.Lazy as LBS
import Data.Id (UserId)
import Data.Json.Util
import Data.Kind
import Data.LegalHold (UserLegalHoldStatus (..), defUserLegalHoldStatus)
import Data.Misc (PlainTextPassword6)
import Data.Proxy
import Data.Schema
import Data.Swagger (ToParamSchema (..))
import qualified Data.Swagger.Schema as S
import GHC.TypeLits
import Imports
import Wire.API.Routes.MultiTablePaging (MultiTablePage (..))
import Wire.API.Routes.MultiTablePaging.State
import Wire.API.Team.Permission (Permissions)
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

data PermissionTag = Required | Optional

type family PermissionType (tag :: PermissionTag) = (t :: Type) | t -> tag where
  PermissionType 'Required = Permissions
  PermissionType 'Optional = Maybe Permissions

--------------------------------------------------------------------------------
-- TeamMember

type TeamMember = TeamMember' 'Required

type TeamMemberOptPerms = TeamMember' 'Optional

data TeamMember' (tag :: PermissionTag) = TeamMember
  { _newTeamMember :: NewTeamMember' tag,
    _legalHoldStatus :: UserLegalHoldStatus
  }
  deriving stock (Generic)

ntmNewTeamMember :: NewTeamMember' tag -> TeamMember' tag
ntmNewTeamMember ntm = TeamMember ntm defUserLegalHoldStatus

deriving instance Eq (PermissionType tag) => Eq (TeamMember' tag)

deriving instance Ord (PermissionType tag) => Ord (TeamMember' tag)

deriving instance Show (PermissionType tag) => Show (TeamMember' tag)

deriving via (GenericUniform TeamMember) instance Arbitrary TeamMember

deriving via (GenericUniform (TeamMember' 'Optional)) instance Arbitrary (TeamMember' 'Optional)

deriving via
  (Schema (TeamMember' tag))
  instance
    (ToSchema (TeamMember' tag)) =>
    ToJSON (TeamMember' tag)

deriving via
  (Schema (TeamMember' tag))
  instance
    (ToSchema (TeamMember' tag)) =>
    FromJSON (TeamMember' tag)

deriving via
  (Schema (TeamMember' tag))
  instance
    (ToSchema (TeamMember' tag)) =>
    S.ToSchema (TeamMember' tag)

mkTeamMember ::
  UserId ->
  PermissionType tag ->
  Maybe (UserId, UTCTimeMillis) ->
  UserLegalHoldStatus ->
  TeamMember' tag
mkTeamMember uid perms inv = TeamMember (NewTeamMember uid perms inv)

instance ToSchema TeamMember where
  schema = object "TeamMember" teamMemberObjectSchema

teamMemberObjectSchema :: ObjectSchema SwaggerDoc TeamMember
teamMemberObjectSchema =
  TeamMember
    <$> _newTeamMember
      .= newTeamMemberSchema
    <*> _legalHoldStatus
      .= (fromMaybe defUserLegalHoldStatus <$> optFieldWithDocModifier "legalhold_status" (description ?~ lhDesc) schema)

instance ToSchema (TeamMember' 'Optional) where
  schema =
    objectWithDocModifier "TeamMember" (description ?~ "team member data") $
      TeamMember
        <$> _newTeamMember
          .= ( NewTeamMember
                 <$> _nUserId
                   .= fieldWithDocModifier "user" (description ?~ "user ID") schema
                 <*> _nPermissions
                   .= maybe_ (optFieldWithDocModifier "permissions" (description ?~ permissionsDesc) schema)
                 <*> _nInvitation
                   .= invitedSchema'
             )
        <*> _legalHoldStatus
          .= (fromMaybe defUserLegalHoldStatus <$> optFieldWithDocModifier "legalhold_status" (description ?~ lhDesc) schema)
    where
      permissionsDesc =
        "The permissions this user has in the given team \
        \ (only visible with permission `GetMemberPermissions`)."

lhDesc :: Text
lhDesc = "The state of Legal Hold compliance for the member"

setPerm :: Bool -> Permissions -> Maybe Permissions
setPerm True = Just
setPerm False = const Nothing

--------------------------------------------------------------------------------
-- TeamMemberList

data TeamMembersTable = TeamMembersTable
  deriving (Eq, Show, Generic)

instance PagingTable TeamMembersTable where
  encodePagingTable TeamMembersTable = 0

  decodePagingTable 0 = pure TeamMembersTable
  decodePagingTable x = fail $ "Expected 0 while parsing TeamMembersTable, got: " <> show x

type TeamMembersPagingName = "TeamMembers"

type TeamMembersPage' = MultiTablePage TeamMembersPagingName "members" TeamMembersTable TeamMemberOptPerms

newtype TeamMembersPage = TeamMembersPage {unTeamMembersPage :: TeamMembersPage'}
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema TeamMembersPage)

instance ToSchema TeamMembersPage where
  schema =
    object "TeamMembersPage" $
      TeamMembersPage
        <$> unTeamMembersPage
          .= ( MultiTablePage
                 <$> mtpResults
                   .= field "members" (array schema)
                 <*> mtpHasMore
                   .= field "hasMore" schema
                 <*> mtpPagingState
                   .= field "pagingState" schema
             )

type TeamMembersPagingState = MultiTablePagingState TeamMembersPagingName TeamMembersTable

teamMemberPagingState :: PageWithState TeamMember -> TeamMembersPagingState
teamMemberPagingState p = MultiTablePagingState TeamMembersTable (LBS.toStrict . C.unPagingState <$> pwsState p)

instance ToParamSchema TeamMembersPagingState where
  toParamSchema _ = toParamSchema (Proxy @Text)

type TeamMemberList = TeamMemberList' 'Required

type TeamMemberListOptPerms = TeamMemberList' 'Optional

data TeamMemberList' (tag :: PermissionTag) = TeamMemberList
  { _teamMembers :: [TeamMember' tag],
    _teamMemberListType :: ListType
  }
  deriving stock (Generic)

deriving instance Eq (PermissionType tag) => Eq (TeamMemberList' tag)

deriving instance Show (PermissionType tag) => Show (TeamMemberList' tag)

deriving via (GenericUniform (TeamMemberList' 'Optional)) instance Arbitrary (TeamMemberList' 'Optional)

deriving via (GenericUniform TeamMemberList) instance Arbitrary TeamMemberList

deriving via
  (Schema (TeamMemberList' tag))
  instance
    ToSchema (TeamMemberList' tag) =>
    FromJSON (TeamMemberList' tag)

deriving via
  (Schema (TeamMemberList' tag))
  instance
    ToSchema (TeamMemberList' tag) =>
    ToJSON (TeamMemberList' tag)

deriving via
  (Schema (TeamMemberList' tag))
  instance
    ToSchema (TeamMemberList' tag) =>
    S.ToSchema (TeamMemberList' tag)

newTeamMemberList :: [TeamMember] -> ListType -> TeamMemberList
newTeamMemberList = TeamMemberList

instance ToSchema (TeamMember' tag) => ToSchema (TeamMemberList' tag) where
  schema =
    objectWithDocModifier "TeamMemberList" (description ?~ "list of team member") $
      TeamMemberList
        <$> _teamMembers
          .= fieldWithDocModifier "members" (description ?~ "the array of team members") (array schema)
        <*> _teamMemberListType
          .= fieldWithDocModifier "hasMore" (description ?~ "true if 'members' doesn't contain all team members") schema

type HardTruncationLimit = (2000 :: Nat)

hardTruncationLimit :: Integral a => a
hardTruncationLimit = fromIntegral $ natVal (Proxy @HardTruncationLimit)

-- | Like 'ListType', but without backwards-compatible and boolean-blind json serialization.
data NewListType
  = NewListComplete
  | NewListTruncated
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewListType)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema NewListType)

instance ToSchema NewListType where
  schema =
    enum @Text "NewListType" $
      mconcat
        [ element "list_complete" NewListComplete,
          element "list_truncated" NewListTruncated
        ]

toNewListType :: ListType -> NewListType
toNewListType ListComplete = NewListComplete
toNewListType ListTruncated = NewListTruncated

data ListType
  = ListComplete
  | ListTruncated
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ListType)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema ListType)

-- This replaces the previous `hasMore` but has no boolean blindness. At the API level
-- though we do want this to remain true/false
instance ToSchema ListType where
  schema =
    enum @Bool "ListType" $
      mconcat [element True ListTruncated, element False ListComplete]

--------------------------------------------------------------------------------
-- NewTeamMember

type NewTeamMember = NewTeamMember' 'Required

mkNewTeamMember :: UserId -> PermissionType 'Required -> Maybe (UserId, UTCTimeMillis) -> NewTeamMember
mkNewTeamMember = NewTeamMember

-- | Like 'TeamMember', but we can receive this from the clients.  Clients are not allowed to
-- set 'UserLegalHoldStatus'.
data NewTeamMember' (tag :: PermissionTag) = NewTeamMember
  { _nUserId :: UserId,
    _nPermissions :: PermissionType tag,
    _nInvitation :: Maybe (UserId, UTCTimeMillis)
  }
  deriving stock (Generic)

deriving instance (Eq (PermissionType tag)) => Eq (NewTeamMember' tag)

deriving instance (Ord (PermissionType tag)) => Ord (NewTeamMember' tag)

deriving instance (Show (PermissionType tag)) => Show (NewTeamMember' tag)

deriving via
  (Schema (NewTeamMember' tag))
  instance
    (ToSchema (NewTeamMember' tag)) =>
    ToJSON (NewTeamMember' tag)

deriving via
  (Schema (NewTeamMember' tag))
  instance
    (ToSchema (NewTeamMember' tag)) =>
    FromJSON (NewTeamMember' tag)

deriving via
  (Schema (NewTeamMember' tag))
  instance
    (ToSchema (NewTeamMember' tag)) =>
    S.ToSchema (NewTeamMember' tag)

deriving via (GenericUniform NewTeamMember) instance Arbitrary NewTeamMember

deriving via (GenericUniform (NewTeamMember' 'Optional)) instance Arbitrary (NewTeamMember' 'Optional)

newTeamMemberSchema :: ObjectSchema SwaggerDoc NewTeamMember
newTeamMemberSchema =
  NewTeamMember
    <$> _nUserId
      .= field "user" schema
    <*> _nPermissions
      .= field "permissions" schema
    <*> _nInvitation
      .= invitedSchema'

invitedSchema :: ObjectSchemaP SwaggerDoc (Maybe (UserId, UTCTimeMillis)) (Maybe UserId, Maybe UTCTimeMillis)
invitedSchema =
  (,)
    <$> fmap fst
      .= optFieldWithDocModifier "created_by" (description ?~ "ID of the inviting user.  Requires created_at.") (maybeWithDefault Null schema)
    <*> fmap snd
      .= optFieldWithDocModifier "created_at" (description ?~ "Timestamp of invitation creation.  Requires created_by.") (maybeWithDefault Null schema)

invitedSchema' :: ObjectSchema SwaggerDoc (Maybe (UserId, UTCTimeMillis))
invitedSchema' = withParser invitedSchema $ \(invby, invat) ->
  case (invby, invat) of
    (Just b, Just a) -> pure $ Just (b, a)
    (Nothing, Nothing) -> pure Nothing
    _ -> fail "created_by, created_at"

instance ToSchema NewTeamMember where
  schema =
    objectWithDocModifier "NewTeamMember" (description ?~ "Required data when creating new team members") $
      fieldWithDocModifier "member" (description ?~ "the team member to add (the legalhold_status field must be null or missing!)") $
        unnamed (object "Unnamed" newTeamMemberSchema)

--------------------------------------------------------------------------------
-- TeamMemberDeleteData

newtype TeamMemberDeleteData = TeamMemberDeleteData
  { _tmdAuthPassword :: Maybe PlainTextPassword6
  }
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema TeamMemberDeleteData)

instance ToSchema TeamMemberDeleteData where
  schema =
    objectWithDocModifier "TeamMemberDeleteData" (description ?~ "Data for a team member deletion request in case of binding teams.") $
      TeamMemberDeleteData <$> _tmdAuthPassword .= optFieldWithDocModifier "password" (description ?~ "The account password to authorise the deletion.") (maybeWithDefault Null schema)

newTeamMemberDeleteData :: Maybe PlainTextPassword6 -> TeamMemberDeleteData
newTeamMemberDeleteData = TeamMemberDeleteData

makeLenses ''TeamMember'
makeLenses ''TeamMemberList'
makeLenses ''NewTeamMember'
makeLenses ''TeamMemberDeleteData

userId :: Lens' TeamMember UserId
userId = newTeamMember . nUserId

optionalPermissions :: TeamMemberOptPerms -> Maybe Permissions
optionalPermissions = _nPermissions . _newTeamMember

permissions :: Lens (TeamMember' tag1) (TeamMember' tag2) (PermissionType tag1) (PermissionType tag2)
permissions = newTeamMember . nPermissions

invitation :: Lens' TeamMember (Maybe (UserId, UTCTimeMillis))
invitation = newTeamMember . nInvitation

teamMemberJson :: (TeamMember -> Bool) -> TeamMember -> Value
teamMemberJson withPerms = toJSON . setOptionalPerms withPerms

setOptionalPerms :: (TeamMember -> Bool) -> TeamMember -> TeamMember' 'Optional
setOptionalPerms withPerms m = m & permissions %~ setPerm (withPerms m)

setOptionalPermsMany :: (TeamMember -> Bool) -> TeamMemberList -> TeamMemberList' 'Optional
setOptionalPermsMany withPerms l =
  l {_teamMembers = map (setOptionalPerms withPerms) (_teamMembers l)}
