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
{-# OPTIONS_GHC -Wwarn #-}

module Wire.API.Team.Member
  ( -- * TeamMember
    TeamMember,
    mkTeamMember,
    userId,
    permissions,
    invitation,
    legalHoldStatus,

    -- * TODO: remove after servantification
    teamMemberJson,

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
import qualified Data.HashMap.Strict as HM
import Data.Id (UserId)
import Data.Json.Util
import Data.LegalHold (UserLegalHoldStatus (..), defUserLegalHoldStatus, typeUserLegalHoldStatus)
import Data.Misc (PlainTextPassword (..))
import Data.Proxy
import Data.Schema
import Data.String.Conversions (cs)
import qualified Data.Swagger.Build.Api as Doc
import qualified Data.Swagger.Schema as S
import Deriving.Swagger (CamelToSnake, ConstructorTagModifier, CustomSwagger, StripPrefix)
import GHC.TypeLits
import Imports
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..), arbitrary, shrink)
import Wire.API.Team.Permission (Permissions, modelPermissions)

--------------------------------------------------------------------------------
-- TeamMember

type TeamMember = TeamMember' Permissions

data TeamMember' perm = TeamMember
  { _newTeamMember :: NewTeamMember' perm,
    _legalHoldStatus :: UserLegalHoldStatus
  }
  deriving stock (Eq, Ord, Show, Generic)

deriving via
  (GenericUniform (TeamMember' perm))
  instance
    Arbitrary perm =>
    Arbitrary (TeamMember' perm)

deriving via
  (Schema (TeamMember' perm))
  instance
    (ToSchema (TeamMember' perm)) =>
    ToJSON (TeamMember' perm)

deriving via
  (Schema (TeamMember' perm))
  instance
    (ToSchema (TeamMember' perm)) =>
    FromJSON (TeamMember' perm)

deriving via
  (Schema (TeamMember' perm))
  instance
    (ToSchema (TeamMember' perm)) =>
    S.ToSchema (TeamMember' perm)

mkTeamMember ::
  UserId ->
  perm ->
  Maybe (UserId, UTCTimeMillis) ->
  UserLegalHoldStatus ->
  TeamMember' perm
mkTeamMember uid perms inv lh =
  TeamMember (NewTeamMember uid perms inv) lh

instance ToSchema TeamMember where
  schema =
    object "TeamMember" $
      TeamMember
        <$> _newTeamMember .= newTeamMemberSchema
        <*> _legalHoldStatus .= (fromMaybe defUserLegalHoldStatus <$> optField "legalhold_status" schema)

instance ToSchema (TeamMember' (Maybe Permissions)) where
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

-- instance ToJSON TeamMember where
--   toJSON = teamMemberJson (const True)

-- instance FromJSON TeamMember where
--   parseJSON = parseTeamMember

-- | Show 'Permissions' conditionally.  The condition takes the member that will receive the result
-- into account.  See 'canSeePermsOf'.
--
-- FUTUREWORK:
-- There must be a cleaner way to do this, with a separate type
-- instead of logic in the JSON instance.
setPerm :: Bool -> Permissions -> Maybe Permissions
setPerm True = Just
setPerm False = const Nothing

-- parseTeamMember :: Value -> Parser TeamMember
-- parseTeamMember = withObject "team-member" $ \o ->
--   TeamMember
--     <$> o .: "user"
--     <*> o .: "permissions"
--     <*> parseInvited o
--     -- Default to disabled if missing
--     <*> o .:? "legalhold_status" .!= defUserLegalHoldStatus
--   where
--     parseInvited :: Object -> Parser (Maybe (UserId, UTCTimeMillis))
--     parseInvited o = do
--       invby <- o .:? "created_by"
--       invat <- o .:? "created_at"
--       case (invby, invat) of
--         (Just b, Just a) -> pure $ Just (b, a)
--         (Nothing, Nothing) -> pure $ Nothing
--         _ -> fail "created_by, created_at"

--------------------------------------------------------------------------------
-- TeamMemberList

data TeamMemberList = TeamMemberList
  { _teamMembers :: [TeamMember],
    _teamMemberListType :: ListType
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform TeamMemberList)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema TeamMemberList)

newTeamMemberList :: [TeamMember] -> ListType -> TeamMemberList
newTeamMemberList = TeamMemberList

modelTeamMemberList :: Doc.Model
modelTeamMemberList = Doc.defineModel "TeamMemberList" $ do
  Doc.description "list of team member"
  Doc.property "members" (Doc.unique $ Doc.array (Doc.ref modelTeamMember)) $
    Doc.description "the array of team members"
  Doc.property "hasMore" Doc.bool' $
    Doc.description "true if 'members' doesn't contain all team members"

-- instance ToJSON TeamMemberList where
--   toJSON = teamMemberListJson (const True)

-- | Show a list of team members using 'teamMemberJson'.
teamMemberListJson :: (TeamMember -> Bool) -> TeamMemberList -> Value
teamMemberListJson withPerms l = undefined

-- instance FromJSON TeamMemberList where
--   parseJSON = withObject "team member list" $ \o ->
--     TeamMemberList <$> o .: "members" <*> o .: "hasMore"

instance ToSchema TeamMemberList where
  schema = undefined

-- object "TeamMemberList" $
--  TeamMemberList <$>

type HardTruncationLimit = (2000 :: Nat)

hardTruncationLimit :: Integral a => a
hardTruncationLimit = fromIntegral $ natVal (Proxy @HardTruncationLimit)

-- | Like 'ListType', but without backwards-compatible and boolean-blind json serialization.
data NewListType
  = NewListComplete
  | NewListTruncated
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewListType)
  deriving (S.ToSchema) via (CustomSwagger '[ConstructorTagModifier (StripPrefix "New", CamelToSnake)] NewListType)
  deriving (FromJSON, ToJSON) via (Schema NewListType)

instance ToSchema NewListType where
  schema = undefined

-- This replaces the previous `hasMore` but has no boolean blindness. At the API level
-- though we do want this to remain true/false
-- instance ToJSON NewListType where
--   toJSON NewListComplete = String "list_complete"
--   toJSON NewListTruncated = String "list_truncated"

-- instance FromJSON NewListType where
--   parseJSON (String "list_complete") = pure NewListComplete
--   parseJSON (String "list_truncated") = pure NewListTruncated
--   parseJSON bad = fail $ "NewListType: " <> cs (encode bad)

toNewListType :: ListType -> NewListType
toNewListType ListComplete = NewListComplete
toNewListType ListTruncated = NewListTruncated

data ListType
  = ListComplete
  | ListTruncated
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ListType)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema ListType)

instance ToSchema ListType where
  schema = undefined

-- This replaces the previous `hasMore` but has no boolean blindness. At the API level
-- though we do want this to remain true/false
-- instance ToJSON ListType where
--   toJSON ListComplete = Bool False
--   toJSON ListTruncated = Bool True

-- instance FromJSON ListType where
--   parseJSON (Bool False) = pure ListComplete
--   parseJSON (Bool True) = pure ListTruncated
--   parseJSON bad = fail $ "ListType: " <> cs (encode bad)

--------------------------------------------------------------------------------
-- NewTeamMember

type NewTeamMember = NewTeamMember' Permissions

-- | Like 'TeamMember', but we can receive this from the clients.  Clients are not allowed to
-- set 'UserLegalHoldStatus'.
data NewTeamMember' perm = NewTeamMember
  { _nUserId :: UserId,
    _nPermissions :: perm,
    _nInvitation :: Maybe (UserId, UTCTimeMillis)
  }
  deriving stock (Eq, Ord, Show, Generic)

deriving via
  (Schema (NewTeamMember' perm))
  instance
    (ToSchema (NewTeamMember' perm)) =>
    ToJSON (NewTeamMember' perm)

deriving via
  (Schema (NewTeamMember' perm))
  instance
    (ToSchema (NewTeamMember' perm)) =>
    FromJSON (NewTeamMember' perm)

deriving via
  (Schema (NewTeamMember' perm))
  instance
    (ToSchema (NewTeamMember' perm)) =>
    S.ToSchema (NewTeamMember' perm)

deriving via
  (GenericUniform (NewTeamMember' perm))
  instance
    Arbitrary perm =>
    Arbitrary (NewTeamMember' perm)

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

newNewTeamMember :: UserId -> Permissions -> Maybe (UserId, UTCTimeMillis) -> NewTeamMember
newNewTeamMember = NewTeamMember

modelNewTeamMember :: Doc.Model
modelNewTeamMember = Doc.defineModel "NewTeamMember" $ do
  Doc.description "Required data when creating new team members"
  Doc.property "member" (Doc.ref modelTeamMember) $
    Doc.description "the team member to add (the legalhold_status field must be null or missing!)"

-- instance ToJSON NewTeamMember where
--   toJSON t = object ["member" .= mem]
--     where
--       mem = Object . HM.fromList . fltr . HM.toList $ o
--       o = case toJSON (_ntmNewTeamMember t) of
--         Object o_ -> o_
--         _ -> error "impossible"
--       fltr = filter ((`elem` ["user", "permissions", "created_by", "created_at"]) . fst)

-- instance FromJSON NewTeamMember where
--   parseJSON = withObject "add team member" $ \o -> do
--     mem <- o .: "member"
--     if _legalHoldStatus mem == defUserLegalHoldStatus
--       then pure $ NewTeamMember mem
--       else fail "legalhold_status field cannot be set in NewTeamMember"

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
makeLenses ''TeamMemberList
makeLenses ''NewTeamMember'
makeLenses ''TeamMemberDeleteData

userId :: Lens' TeamMember UserId
userId = newTeamMember . nUserId

permissions :: Lens (TeamMember' perm) (TeamMember' perm') perm perm'
permissions = newTeamMember . nPermissions

invitation :: Lens' TeamMember (Maybe (UserId, UTCTimeMillis))
invitation = newTeamMember . nInvitation

teamMemberJson :: (TeamMember -> Bool) -> TeamMember -> Value
teamMemberJson withPerms m = schemaToJSON (m & permissions %~ setPerm (withPerms m))
