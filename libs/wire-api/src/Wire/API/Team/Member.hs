{-# LANGUAGE DerivingVia #-}
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
    TeamMember (..),
    newTeamMember,
    userId,
    permissions,
    invitation,
    legalHoldStatus,
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
    teamMemberListJson,

    -- * NewTeamMember
    NewTeamMember,
    newNewTeamMember,
    ntmNewTeamMember,

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

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM
import Data.Id (UserId)
import Data.Json.Util
import Data.LegalHold (UserLegalHoldStatus (..), typeUserLegalHoldStatus)
import Data.Misc (PlainTextPassword (..))
import Data.Proxy
import Data.String.Conversions (cs)
import qualified Data.Swagger.Build.Api as Doc
import Data.Swagger.Schema (ToSchema)
import Deriving.Swagger (CamelToSnake, ConstructorTagModifier, CustomSwagger, StripPrefix)
import GHC.TypeLits
import Imports
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))
import Wire.API.Team.Permission (Permissions, modelPermissions)

--------------------------------------------------------------------------------
-- TeamMember

data TeamMember = TeamMember
  { _userId :: UserId,
    _permissions :: Permissions,
    _invitation :: Maybe (UserId, UTCTimeMillis),
    _legalHoldStatus :: UserLegalHoldStatus
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform TeamMember)

newTeamMember ::
  UserId ->
  Permissions ->
  Maybe (UserId, UTCTimeMillis) ->
  TeamMember
newTeamMember uid perm invitation = TeamMember uid perm invitation UserLegalHoldDisabled

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

instance ToJSON TeamMember where
  toJSON = teamMemberJson (const True)

instance FromJSON TeamMember where
  parseJSON = parseTeamMember

-- | Show 'Permissions' conditionally.  The condition takes the member that will receive the result
-- into account.  See 'canSeePermsOf'.
--
-- FUTUREWORK:
-- There must be a cleaner way to do this, with a separate type
-- instead of logic in the JSON instance.
teamMemberJson :: (TeamMember -> Bool) -> TeamMember -> Value
teamMemberJson withPerms m =
  object $
    ["user" .= _userId m]
      <> ["permissions" .= _permissions m | withPerms m]
      <> ["created_by" .= (fst <$> _invitation m)]
      <> ["created_at" .= (snd <$> _invitation m)]
      <> ["legalhold_status" .= _legalHoldStatus m]

parseTeamMember :: Value -> Parser TeamMember
parseTeamMember = withObject "team-member" $ \o ->
  TeamMember
    <$> o .: "user"
    <*> o .: "permissions"
    <*> parseInvited o
    -- Default to disabled if missing
    <*> o .:? "legalhold_status" .!= UserLegalHoldDisabled
  where
    parseInvited :: Object -> Parser (Maybe (UserId, UTCTimeMillis))
    parseInvited o = do
      invby <- o .:? "created_by"
      invat <- o .:? "created_at"
      case (invby, invat) of
        (Just b, Just a) -> pure $ Just (b, a)
        (Nothing, Nothing) -> pure $ Nothing
        _ -> fail "created_by, created_at"

--------------------------------------------------------------------------------
-- TeamMemberList

data TeamMemberList = TeamMemberList
  { _teamMembers :: [TeamMember],
    _teamMemberListType :: ListType
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform TeamMemberList)

newTeamMemberList :: [TeamMember] -> ListType -> TeamMemberList
newTeamMemberList = TeamMemberList

modelTeamMemberList :: Doc.Model
modelTeamMemberList = Doc.defineModel "TeamMemberList" $ do
  Doc.description "list of team member"
  Doc.property "members" (Doc.unique $ Doc.array (Doc.ref modelTeamMember)) $
    Doc.description "the array of team members"
  Doc.property "hasMore" Doc.bool' $
    Doc.description "true if 'members' doesn't contain all team members"

instance ToJSON TeamMemberList where
  toJSON = teamMemberListJson (const True)

-- | Show a list of team members using 'teamMemberJson'.
teamMemberListJson :: (TeamMember -> Bool) -> TeamMemberList -> Value
teamMemberListJson withPerms l =
  object
    [ "members" .= map (teamMemberJson withPerms) (_teamMembers l),
      "hasMore" .= _teamMemberListType l
    ]

instance FromJSON TeamMemberList where
  parseJSON = withObject "team member list" $ \o ->
    TeamMemberList <$> o .: "members" <*> o .: "hasMore"

type HardTruncationLimit = (2000 :: Nat)

hardTruncationLimit :: Integral a => a
hardTruncationLimit = fromIntegral $ natVal (Proxy @HardTruncationLimit)

-- | Like 'ListType', but without backwards-compatible and boolean-blind json serialization.
data NewListType
  = NewListComplete
  | NewListTruncated
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewListType)
  deriving (ToSchema) via (CustomSwagger '[ConstructorTagModifier (StripPrefix "New", CamelToSnake)] NewListType)

-- This replaces the previous `hasMore` but has no boolean blindness. At the API level
-- though we do want this to remain true/false
instance ToJSON NewListType where
  toJSON NewListComplete = String "list_complete"
  toJSON NewListTruncated = String "list_truncated"

instance FromJSON NewListType where
  parseJSON (String "list_complete") = pure NewListComplete
  parseJSON (String "list_truncated") = pure NewListTruncated
  parseJSON bad = fail $ "NewListType: " <> cs (encode bad)

toNewListType :: ListType -> NewListType
toNewListType ListComplete = NewListComplete
toNewListType ListTruncated = NewListTruncated

data ListType
  = ListComplete
  | ListTruncated
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ListType)

-- This replaces the previous `hasMore` but has no boolean blindness. At the API level
-- though we do want this to remain true/false
instance ToJSON ListType where
  toJSON ListComplete = Bool False
  toJSON ListTruncated = Bool True

instance FromJSON ListType where
  parseJSON (Bool False) = pure ListComplete
  parseJSON (Bool True) = pure ListTruncated
  parseJSON bad = fail $ "ListType: " <> cs (encode bad)

--------------------------------------------------------------------------------
-- NewTeamMember

-- | Like 'TeamMember', but we can receive this from the clients.  Clients are not allowed to
-- set 'UserLegalHoldStatus', so both 'newNewTeamMember and {To,From}JSON make sure that is
-- always the default.  I decided to keep the 'TeamMember' inside (rather than making an
-- entirely new type because (1) it's a smaller change and I'm in a hurry; (2) it encodes the
-- identity relationship between the fields that *do* occur in both more explicit.
newtype NewTeamMember = NewTeamMember
  { _ntmNewTeamMember :: TeamMember
  }
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)

newNewTeamMember :: UserId -> Permissions -> Maybe (UserId, UTCTimeMillis) -> NewTeamMember
newNewTeamMember uid perms mbinv = NewTeamMember $ TeamMember uid perms mbinv UserLegalHoldDisabled

modelNewTeamMember :: Doc.Model
modelNewTeamMember = Doc.defineModel "NewTeamMember" $ do
  Doc.description "Required data when creating new team members"
  Doc.property "member" (Doc.ref modelTeamMember) $
    Doc.description "the team member to add (the legalhold_status field must be null or missing!)"

instance ToJSON NewTeamMember where
  toJSON t = object ["member" .= mem]
    where
      mem = Object . HM.fromList . fltr . HM.toList $ o
      o = case toJSON (_ntmNewTeamMember t) of
        Object o_ -> o_
        _ -> error "impossible"
      fltr = filter ((`elem` ["user", "permissions", "created_by", "created_at"]) . fst)

instance FromJSON NewTeamMember where
  parseJSON = withObject "add team member" $ \o -> do
    mem <- o .: "member"
    if (_legalHoldStatus mem == UserLegalHoldDisabled)
      then pure $ NewTeamMember mem
      else fail "legalhold_status field cannot be set in NewTeamMember"

--------------------------------------------------------------------------------
-- TeamMemberDeleteData

newtype TeamMemberDeleteData = TeamMemberDeleteData
  { _tmdAuthPassword :: Maybe PlainTextPassword
  }
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)

newTeamMemberDeleteData :: Maybe PlainTextPassword -> TeamMemberDeleteData
newTeamMemberDeleteData = TeamMemberDeleteData

-- FUTUREWORK: fix name of model?
modelTeamMemberDelete :: Doc.Model
modelTeamMemberDelete = Doc.defineModel "teamDeleteData" $ do
  Doc.description "Data for a team member deletion request in case of binding teams."
  Doc.property "password" Doc.string' $
    Doc.description "The account password to authorise the deletion."

instance FromJSON TeamMemberDeleteData where
  parseJSON = withObject "team-member-delete-data" $ \o ->
    TeamMemberDeleteData <$> (o .:? "password")

instance ToJSON TeamMemberDeleteData where
  toJSON tmd =
    object
      [ "password" .= _tmdAuthPassword tmd
      ]

makeLenses ''TeamMember
makeLenses ''TeamMemberList
makeLenses ''NewTeamMember
makeLenses ''TeamMemberDeleteData
