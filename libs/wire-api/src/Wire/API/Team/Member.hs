{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
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
  ( TeamMember,
    newTeamMember,
    userId,
    permissions,
    invitation,
    legalHoldStatus,
    teamMemberJson,
    TeamMemberList,
    ListType (..),
    newTeamMemberList,
    teamMembers,
    teamMemberListType,
    teamMemberListJson,
    NewTeamMember,
    newNewTeamMember,
    ntmNewTeamMember,
    TeamMemberDeleteData,
    tmdAuthPassword,
    newTeamMemberDeleteData,
    HardTruncationLimit,
    hardTruncationLimit,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Id (UserId)
import Data.Json.Util
import Data.LegalHold (UserLegalHoldStatus (..))
import Data.Misc (PlainTextPassword (..))
import Data.Proxy
import Data.String.Conversions (cs)
import GHC.TypeLits
import Imports
import Wire.API.Team.Permission (Permissions)

data TeamMember = TeamMember
  { _userId :: UserId,
    _permissions :: Permissions,
    _invitation :: Maybe (UserId, UTCTimeMillis),
    _legalHoldStatus :: UserLegalHoldStatus
  }
  deriving (Eq, Ord, Show, Generic)

data ListType
  = ListComplete
  | ListTruncated
  deriving (Eq, Show, Generic)

-- This replaces the previous `hasMore` but has no boolean blindness. At the API level
-- though we do want this to remain true/false
instance ToJSON ListType where
  toJSON ListComplete = Bool False
  toJSON ListTruncated = Bool True

instance FromJSON ListType where
  parseJSON (Bool False) = pure ListComplete
  parseJSON (Bool True) = pure ListTruncated
  parseJSON bad = fail $ "ListType: " <> cs (encode bad)

data TeamMemberList = TeamMemberList
  { _teamMembers :: [TeamMember],
    _teamMemberListType :: ListType
  }
  deriving (Generic)

type HardTruncationLimit = (2000 :: Nat)

hardTruncationLimit :: Integral a => a
hardTruncationLimit = fromIntegral $ natVal (Proxy @HardTruncationLimit)

newtype NewTeamMember = NewTeamMember
  { _ntmNewTeamMember :: TeamMember
  }

newtype TeamMemberDeleteData = TeamMemberDeleteData
  { _tmdAuthPassword :: Maybe PlainTextPassword
  }

newTeamMember ::
  UserId ->
  Permissions ->
  Maybe (UserId, UTCTimeMillis) ->
  TeamMember
newTeamMember uid perm invitation = TeamMember uid perm invitation UserLegalHoldDisabled

newTeamMemberList :: [TeamMember] -> ListType -> TeamMemberList
newTeamMemberList = TeamMemberList

newNewTeamMember :: TeamMember -> NewTeamMember
newNewTeamMember = NewTeamMember

newTeamMemberDeleteData :: Maybe PlainTextPassword -> TeamMemberDeleteData
newTeamMemberDeleteData = TeamMemberDeleteData

makeLenses ''TeamMember

makeLenses ''TeamMemberList

makeLenses ''NewTeamMember

makeLenses ''TeamMemberDeleteData

instance ToJSON TeamMember where
  toJSON = teamMemberJson (const True)

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
  TeamMember <$> o .: "user"
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

instance ToJSON TeamMemberList where
  toJSON = teamMemberListJson (const True)

-- | Show a list of team members using 'teamMemberJson'.
teamMemberListJson :: (TeamMember -> Bool) -> TeamMemberList -> Value
teamMemberListJson withPerms l =
  object
    [ "members" .= map (teamMemberJson withPerms) (_teamMembers l),
      "hasMore" .= _teamMemberListType l
    ]

instance FromJSON TeamMember where
  parseJSON = parseTeamMember

instance FromJSON TeamMemberList where
  parseJSON = withObject "team member list" $ \o ->
    TeamMemberList <$> o .: "members" <*> o .: "hasMore"

instance ToJSON NewTeamMember where
  toJSON t = object ["member" .= _ntmNewTeamMember t]

instance FromJSON NewTeamMember where
  parseJSON = withObject "add team member" $ \o ->
    NewTeamMember <$> o .: "member"

instance FromJSON TeamMemberDeleteData where
  parseJSON = withObject "team-member-delete-data" $ \o ->
    TeamMemberDeleteData <$> (o .:? "password")

instance ToJSON TeamMemberDeleteData where
  toJSON tmd =
    object
      [ "password" .= _tmdAuthPassword tmd
      ]
