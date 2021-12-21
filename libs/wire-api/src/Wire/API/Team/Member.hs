{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

    -- * TODO: remove after servantification
    teamMemberJson,
    teamMemberListJson,

    -- * TeamMemberList
    TeamMemberList,
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
    mkNewTeamMember,
    nUserId,
    nPermissions,
    nInvitation,

    -- * TeamMemberDeleteData
    TeamMemberDeleteData,
    newTeamMemberDeleteData,
    tmdAuthPassword,

    -- * Swagger
    modelTeamMember,
    modelTeamMemberList,
    modelNewTeamMember,
    modelTeamMemberDelete,
  )
where

import Control.Lens (Lens, Lens', makeLenses, (%~))
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import Data.Id (UserId)
import Data.Json.Util
import Data.LegalHold (UserLegalHoldStatus (..), defUserLegalHoldStatus, typeUserLegalHoldStatus)
import Data.Misc (PlainTextPassword (..))
import Data.Proxy
import Data.Schema
import qualified Data.Swagger.Build.Api as Doc
import qualified Data.Swagger.Schema as S
import GHC.TypeLits
import Imports
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))
import Wire.API.Team.Permission (Permissions, modelPermissions)

data PermissionTag = Required | Optional

type family PermissionType (tag :: PermissionTag) = (t :: *) | t -> tag where
  PermissionType 'Required = Permissions
  PermissionType 'Optional = Maybe Permissions

--------------------------------------------------------------------------------
-- TeamMember

type TeamMember = TeamMember' 'Required

data TeamMember' (tag :: PermissionTag) = TeamMember
  { _newTeamMember :: NewTeamMember' tag,
    _legalHoldStatus :: UserLegalHoldStatus
  }

ntmNewTeamMember :: NewTeamMember' tag -> TeamMember' tag
ntmNewTeamMember ntm = TeamMember ntm defUserLegalHoldStatus

deriving instance Eq (PermissionType tag) => Eq (TeamMember' tag)

deriving instance Ord (PermissionType tag) => Ord (TeamMember' tag)

deriving instance Show (PermissionType tag) => Show (TeamMember' tag)

deriving instance Generic (TeamMember' tag)

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
  schema =
    object "TeamMember" $
      TeamMember
        <$> _newTeamMember .= newTeamMemberSchema
        <*> _legalHoldStatus .= (fromMaybe defUserLegalHoldStatus <$> optField "legalhold_status" schema)

instance ToSchema (TeamMember' 'Optional) where
  schema =
    object "TeamMember" $
      TeamMember
        <$> _newTeamMember
          .= ( NewTeamMember
                 <$> _nUserId .= field "user" schema
                 <*> _nPermissions .= maybe_ (optField "permissions" schema)
                 <*> _nInvitation .= invitedSchema'
             )
        <*> _legalHoldStatus .= (fromMaybe defUserLegalHoldStatus <$> optField "legalhold_status" schema)

modelTeamMember :: Doc.Model
modelTeamMember = Doc.defineModel "TeamMember" $ do
  Doc.description "team member data"
  Doc.property "user" Doc.bytes' $
    Doc.description "user ID"
  Doc.property "permissions" (Doc.ref modelPermissions) $ do
    Doc.description
      "The permissions this user has in the given team \
      \ (only visible with permission `GetMemberPermissions`)."
    Doc.optional -- not optional in the type, but in the json instance.  (in
    -- servant, we could probably just add a helper type for this.)
    -- TODO: even without servant, it would be nicer to introduce
    -- a type with optional permissions.
  Doc.property "created_at" Doc.dateTime' $ do
    Doc.description "Timestamp of invitation creation.  Requires created_by."
    Doc.optional
  Doc.property "created_by" Doc.bytes' $ do
    Doc.description "ID of the inviting user.  Requires created_at."
    Doc.optional
  Doc.property "legalhold_status" typeUserLegalHoldStatus $ do
    Doc.description "The state of Legal Hold compliance for the member"
    Doc.optional

-- FUTUREWORK:
-- There must be a cleaner way to do this, with a separate type
-- instead of logic in the JSON instance.
setPerm :: Bool -> Permissions -> Maybe Permissions
setPerm True = Just
setPerm False = const Nothing

--------------------------------------------------------------------------------
-- TeamMemberList

type TeamMemberList = TeamMemberList' 'Required

data TeamMemberList' (tag :: PermissionTag) = TeamMemberList
  { _teamMembers :: [TeamMember' tag],
    _teamMemberListType :: ListType
  }
  deriving (Generic)

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

modelTeamMemberList :: Doc.Model
modelTeamMemberList = Doc.defineModel "TeamMemberList" $ do
  Doc.description "list of team member"
  Doc.property "members" (Doc.unique $ Doc.array (Doc.ref modelTeamMember)) $
    Doc.description "the array of team members"
  Doc.property "hasMore" Doc.bool' $
    Doc.description "true if 'members' doesn't contain all team members"

instance ToSchema (TeamMember' tag) => ToSchema (TeamMemberList' tag) where
  schema =
    object "TeamMemberList" $
      TeamMemberList
        <$> _teamMembers .= field "members" (array schema)
        <*> _teamMemberListType .= field "hasMore" schema

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
  schema = listTypeToBool .= boolToListType <$> schema
    where
      listTypeToBool :: ListType -> Bool
      listTypeToBool ListComplete = False
      listTypeToBool ListTruncated = True

      boolToListType :: Bool -> ListType
      boolToListType False = ListComplete
      boolToListType True = ListTruncated

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

deriving instance (Eq (PermissionType tag)) => Eq (NewTeamMember' tag)

deriving instance (Ord (PermissionType tag)) => Ord (NewTeamMember' tag)

deriving instance (Show (PermissionType tag)) => Show (NewTeamMember' tag)

deriving instance (Generic (PermissionType tag)) => Generic (NewTeamMember' tag)

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
    <$> _nUserId .= field "user" schema
    <*> _nPermissions .= field "permissions" schema
    <*> _nInvitation .= invitedSchema'

invitedSchema :: ObjectSchemaP SwaggerDoc (Maybe (UserId, UTCTimeMillis)) (Maybe UserId, Maybe UTCTimeMillis)
invitedSchema =
  (,) <$> fmap fst .= optField "created_by" (maybeWithDefault Null schema)
    <*> fmap snd .= optField "created_at" (maybeWithDefault Null schema)

invitedSchema' :: ObjectSchema SwaggerDoc (Maybe (UserId, UTCTimeMillis))
invitedSchema' = withParser invitedSchema $ \(invby, invat) ->
  case (invby, invat) of
    (Just b, Just a) -> pure $ Just (b, a)
    (Nothing, Nothing) -> pure $ Nothing
    _ -> fail "created_by, created_at"

instance ToSchema NewTeamMember where
  schema = object "NewTeamMember" newTeamMemberSchema

modelNewTeamMember :: Doc.Model
modelNewTeamMember = Doc.defineModel "NewTeamMember" $ do
  Doc.description "Required data when creating new team members"
  Doc.property "member" (Doc.ref modelTeamMember) $
    Doc.description "the team member to add (the legalhold_status field must be null or missing!)"

--------------------------------------------------------------------------------
-- TeamMemberDeleteData

newtype TeamMemberDeleteData = TeamMemberDeleteData
  { _tmdAuthPassword :: Maybe PlainTextPassword
  }
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema TeamMemberDeleteData)

instance ToSchema TeamMemberDeleteData where
  schema =
    object "TeamMemberDeleteData" $
      TeamMemberDeleteData <$> _tmdAuthPassword .= maybe_ (optField "password" schema)

newTeamMemberDeleteData :: Maybe PlainTextPassword -> TeamMemberDeleteData
newTeamMemberDeleteData = TeamMemberDeleteData

-- FUTUREWORK: fix name of model?
modelTeamMemberDelete :: Doc.Model
modelTeamMemberDelete = Doc.defineModel "teamDeleteData" $ do
  Doc.description "Data for a team member deletion request in case of binding teams."
  Doc.property "password" Doc.string' $
    Doc.description "The account password to authorise the deletion."

makeLenses ''TeamMember'
makeLenses ''TeamMemberList'
makeLenses ''NewTeamMember'
makeLenses ''TeamMemberDeleteData

userId :: Lens' TeamMember UserId
userId = newTeamMember . nUserId

permissions :: Lens (TeamMember' tag1) (TeamMember' tag2) (PermissionType tag1) (PermissionType tag2)
permissions = newTeamMember . nPermissions

invitation :: Lens' TeamMember (Maybe (UserId, UTCTimeMillis))
invitation = newTeamMember . nInvitation

-- JSON serialisation utilities (TODO: remove after servantification)

teamMemberJson :: (TeamMember -> Bool) -> TeamMember -> Value
teamMemberJson withPerms = toJSON . setOptionalPerms withPerms

setOptionalPerms :: (TeamMember -> Bool) -> TeamMember -> TeamMember' 'Optional
setOptionalPerms withPerms m = m & permissions %~ setPerm (withPerms m)

-- | Show a list of team members using 'teamMemberJson'.
teamMemberListJson :: (TeamMember -> Bool) -> TeamMemberList -> Value
teamMemberListJson withPerms l =
  toJSON $
    l {_teamMembers = map (setOptionalPerms withPerms) (_teamMembers l)}
