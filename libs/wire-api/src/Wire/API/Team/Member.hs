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

    -- * Permissions
    isAdminOrOwner,
    permissionsRole,
    rolePermissions,
    IsPerm (..),
    HiddenPerm (..),
  )
where

import Cassandra (PageWithState (..))
import Cassandra qualified as C
import Control.Lens (Lens, Lens', makeLenses, (%~), (?~), (^.))
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import Data.ByteString.Lazy qualified as LBS
import Data.Id (UserId)
import Data.Json.Util
import Data.Kind
import Data.LegalHold (UserLegalHoldStatus (..), defUserLegalHoldStatus)
import Data.Misc (PlainTextPassword6)
import Data.OpenApi (ToParamSchema (..))
import Data.OpenApi.Schema qualified as S
import Data.Proxy
import Data.Schema
import Data.Set qualified as Set
import Imports
import Wire.API.Error.Galley
import Wire.API.Routes.MultiTablePaging (MultiTablePage (..))
import Wire.API.Routes.MultiTablePaging.State
import Wire.API.Team.HardTruncationLimit
import Wire.API.Team.Permission
import Wire.API.Team.Role
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

deriving instance (Eq (PermissionType tag)) => Eq (TeamMember' tag)

deriving instance (Ord (PermissionType tag)) => Ord (TeamMember' tag)

deriving instance (Show (PermissionType tag)) => Show (TeamMember' tag)

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
    (ToSchema (TeamMember' tag), Typeable tag) =>
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

deriving instance (Eq (PermissionType tag)) => Eq (TeamMemberList' tag)

deriving instance (Show (PermissionType tag)) => Show (TeamMemberList' tag)

deriving via (GenericUniform (TeamMemberList' 'Optional)) instance Arbitrary (TeamMemberList' 'Optional)

deriving via (GenericUniform TeamMemberList) instance Arbitrary TeamMemberList

deriving via
  (Schema (TeamMemberList' tag))
  instance
    (ToSchema (TeamMemberList' tag)) =>
    FromJSON (TeamMemberList' tag)

deriving via
  (Schema (TeamMemberList' tag))
  instance
    (ToSchema (TeamMemberList' tag)) =>
    ToJSON (TeamMemberList' tag)

deriving via
  (Schema (TeamMemberList' tag))
  instance
    (ToSchema (TeamMemberList' tag), Typeable tag) =>
    S.ToSchema (TeamMemberList' tag)

newTeamMemberList :: [TeamMember] -> ListType -> TeamMemberList
newTeamMemberList = TeamMemberList

instance (ToSchema (TeamMember' tag)) => ToSchema (TeamMemberList' tag) where
  schema =
    objectWithDocModifier "TeamMemberList" (description ?~ "list of team member") $
      TeamMemberList
        <$> _teamMembers
          .= fieldWithDocModifier "members" (description ?~ "the array of team members") (array schema)
        <*> _teamMemberListType
          .= fieldWithDocModifier "hasMore" (description ?~ "true if 'members' doesn't contain all team members") schema

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
    (ToSchema (NewTeamMember' tag), Typeable tag) =>
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

-- Note [hidden team roles]
--
-- The problem: the mapping between 'Role' and 'Permissions' is fixed by external contracts:
-- client apps treat permission bit matrices as opaque role identifiers, so if we add new
-- permission flags, things will break there.
--
-- "Hidden" in "HiddenPerm", therefore, refers to a permission hidden from
-- clients, thereby making it internal to the backend.
--
-- The solution: add new permission bits to 'HiddenPerm', 'HiddenPermissions', and make
-- 'hasPermission', 'mayGrantPermission' polymorphic.  Now you can check both for the hidden
-- permission bits and the old ones that we share with the client apps.

-- | See Note [hidden team roles]
data HiddenPerm
  = ChangeLegalHoldTeamSettings
  | ChangeLegalHoldUserSettings
  | ViewLegalHoldUserSettings
  | ChangeTeamFeature
  | ChangeTeamSearchVisibility
  | ViewTeamSearchVisibility
  | ViewSameTeamEmails
  | ReadIdp
  | CreateUpdateDeleteIdp
  | CreateReadDeleteScimToken
  | -- | this has its own permission because we're not sure how
    -- efficient this end-point is.  better not let all team members
    -- play with it unless we have to.
    DownloadTeamMembersCsv
  | ChangeTeamMemberProfiles
  | SearchContacts
  deriving (Eq, Ord, Show)

-- | See Note [hidden team roles]
data HiddenPermissions = HiddenPermissions
  { _hself :: Set HiddenPerm,
    _hcopy :: Set HiddenPerm
  }
  deriving (Eq, Ord, Show)

makeLenses ''HiddenPermissions

rolePermissions :: Role -> Permissions
rolePermissions role = Permissions p p where p = rolePerms role

permissionsRole :: Permissions -> Maybe Role
permissionsRole (Permissions p p') =
  if p /= p'
    then do
      -- we never did use @p /= p'@ for anything, fingers crossed that it doesn't occur anywhere
      -- in the wild.  but if it does, this implementation prevents privilege escalation.
      let p'' = Set.intersection p p'
       in permissionsRole (Permissions p'' p'')
    else permsRole p
  where
    permsRole :: Set Perm -> Maybe Role
    permsRole perms =
      listToMaybe
        [ role
          | role <- [minBound ..],
            -- if a there is a role that is strictly less permissive than the perms set that
            -- we encounter, we downgrade.  this shouldn't happen in real life, but it has
            -- happened to very old users on a staging environment, where a user (probably)
            -- was create before the current publicly visible permissions had been stabilized.
            rolePerms role `Set.isSubsetOf` perms
        ]

-- | Internal function for 'rolePermissions'.  (It works iff the two sets in 'Permissions' are
-- identical for every 'Role', otherwise it'll need to be specialized for the resp. sides.)
rolePerms :: Role -> Set Perm
rolePerms RoleOwner =
  rolePerms RoleAdmin
    <> Set.fromList
      [ GetBilling,
        SetBilling,
        DeleteTeam
      ]
rolePerms RoleAdmin =
  rolePerms RoleMember
    <> Set.fromList
      [ AddTeamMember,
        RemoveTeamMember,
        SetTeamData,
        SetMemberPermissions
      ]
rolePerms RoleMember =
  rolePerms RoleExternalPartner
    <> Set.fromList
      [ DeleteConversation,
        AddRemoveConvMember,
        ModifyConvName,
        GetMemberPermissions
      ]
rolePerms RoleExternalPartner =
  Set.fromList
    [ CreateConversation,
      GetTeamConversations
    ]

roleHiddenPermissions :: Role -> HiddenPermissions
roleHiddenPermissions role = HiddenPermissions p p
  where
    p = roleHiddenPerms role
    roleHiddenPerms :: Role -> Set HiddenPerm
    roleHiddenPerms RoleOwner = roleHiddenPerms RoleAdmin
    roleHiddenPerms RoleAdmin =
      (roleHiddenPerms RoleMember <>) $
        Set.fromList
          [ ChangeLegalHoldTeamSettings,
            ChangeLegalHoldUserSettings,
            ChangeTeamSearchVisibility,
            ChangeTeamFeature,
            ChangeTeamMemberProfiles,
            ReadIdp,
            CreateUpdateDeleteIdp,
            CreateReadDeleteScimToken,
            DownloadTeamMembersCsv
          ]
    roleHiddenPerms RoleMember =
      (roleHiddenPerms RoleExternalPartner <>) $
        Set.fromList
          [ ViewSameTeamEmails,
            SearchContacts
          ]
    roleHiddenPerms RoleExternalPartner =
      Set.fromList
        [ ViewLegalHoldUserSettings,
          ViewTeamSearchVisibility
        ]

isAdminOrOwner :: Permissions -> Bool
isAdminOrOwner perms =
  case permissionsRole perms of
    Just RoleOwner -> True
    Just RoleAdmin -> True
    Just RoleMember -> False
    Just RoleExternalPartner -> False
    Nothing -> False

-- | See Note [hidden team roles]
class IsPerm perm where
  type PermError (e :: perm) :: GalleyError

  roleHasPerm :: Role -> perm -> Bool
  roleGrantsPerm :: Role -> perm -> Bool
  hasPermission :: TeamMember -> perm -> Bool
  hasPermission tm perm = maybe False (`roleHasPerm` perm) . permissionsRole $ tm ^. permissions
  mayGrantPermission :: TeamMember -> perm -> Bool
  mayGrantPermission tm perm = maybe False (`roleGrantsPerm` perm) . permissionsRole $ tm ^. permissions

instance IsPerm Perm where
  type PermError p = 'MissingPermission ('Just p)

  roleHasPerm r p = p `Set.member` (rolePermissions r ^. self)
  roleGrantsPerm r p = p `Set.member` (rolePermissions r ^. copy)
  hasPermission tm p = p `Set.member` (tm ^. permissions . self)
  mayGrantPermission tm p = p `Set.member` (tm ^. permissions . copy)

instance IsPerm HiddenPerm where
  type PermError p = OperationDenied

  roleHasPerm r p = p `Set.member` (roleHiddenPermissions r ^. hself)
  roleGrantsPerm r p = p `Set.member` (roleHiddenPermissions r ^. hcopy)
