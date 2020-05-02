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

module Wire.API.Team
  ( Team,
    TeamBinding (..),
    newTeam,
    teamId,
    teamCreator,
    teamName,
    teamIcon,
    teamIconKey,
    teamBinding,
    TeamCreationTime (..),
    tcTime,
    FeatureFlags (..),
    flagSSO,
    flagLegalHold,
    FeatureSSO (..),
    FeatureLegalHold (..),
    TeamList,
    newTeamList,
    teamListTeams,
    teamListHasMore,
    TeamMember,
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
    TeamConversation,
    newTeamConversation,
    conversationId,
    managedConversation,
    TeamConversationList,
    newTeamConversationList,
    teamConversations,
    Permissions,
    newPermissions,
    fullPermissions,
    noPermissions,
    serviceWhitelistPermissions,
    self,
    copy,
    Perm (..),
    permToInt,
    permsToInt,
    intToPerm,
    intToPerms,
    Role (..),
    defaultRole,
    rolePermissions,
    permissionsRole,
    BindingNewTeam (..),
    NonBindingNewTeam (..),
    NewTeam,
    newNewTeam,
    newTeamName,
    newTeamIcon,
    newTeamIconKey,
    newTeamMembers,
    NewTeamMember,
    newNewTeamMember,
    ntmNewTeamMember,
    TeamMemberDeleteData,
    tmdAuthPassword,
    newTeamMemberDeleteData,
    TeamDeleteData,
    tdAuthPassword,
    newTeamDeleteData,
    HardTruncationLimit,
    hardTruncationLimit,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Id (ConvId, TeamId, UserId)
import Data.Json.Util
import Data.Misc (PlainTextPassword (..))
import Data.Range
import Imports
import Wire.API.Team.Feature
import Wire.API.Team.Internal
import Wire.API.Team.Member
import Wire.API.Team.Permission

data TeamList = TeamList
  { _teamListTeams :: [Team],
    _teamListHasMore :: Bool
  }
  deriving (Show, Generic)

data TeamConversation = TeamConversation
  { _conversationId :: ConvId,
    _managedConversation :: Bool
  }

newtype TeamConversationList = TeamConversationList
  { _teamConversations :: [TeamConversation]
  }

newtype BindingNewTeam = BindingNewTeam (NewTeam ())
  deriving (Eq, Show, Generic)

-- | FUTUREWORK: this is dead code!  remove!
newtype NonBindingNewTeam = NonBindingNewTeam (NewTeam (Range 1 127 [TeamMember]))
  deriving (Eq, Show, Generic)

newtype TeamDeleteData = TeamDeleteData
  { _tdAuthPassword :: Maybe PlainTextPassword
  }

-- This is the cassandra timestamp of writetime(binding)
newtype TeamCreationTime = TeamCreationTime
  { _tcTime :: Int64
  }

newTeam :: TeamId -> UserId -> Text -> Text -> TeamBinding -> Team
newTeam tid uid nme ico bnd = Team tid uid nme ico Nothing bnd

newTeamList :: [Team] -> Bool -> TeamList
newTeamList = TeamList

newTeamConversation :: ConvId -> Bool -> TeamConversation
newTeamConversation = TeamConversation

newTeamConversationList :: [TeamConversation] -> TeamConversationList
newTeamConversationList = TeamConversationList

newNewTeam :: Range 1 256 Text -> Range 1 256 Text -> NewTeam a
newNewTeam nme ico = NewTeam nme ico Nothing Nothing

newTeamDeleteData :: Maybe PlainTextPassword -> TeamDeleteData
newTeamDeleteData = TeamDeleteData

makeLenses ''Team

makeLenses ''TeamList

makeLenses ''TeamConversation

makeLenses ''TeamConversationList

makeLenses ''NewTeam

makeLenses ''TeamDeleteData

makeLenses ''TeamCreationTime

instance ToJSON TeamList where
  toJSON t =
    object $
      "teams" .= _teamListTeams t
        # "has_more" .= _teamListHasMore t
        # []

instance FromJSON TeamList where
  parseJSON = withObject "teamlist" $ \o -> do
    TeamList <$> o .: "teams"
      <*> o .: "has_more"

instance ToJSON TeamConversation where
  toJSON t =
    object
      [ "conversation" .= _conversationId t,
        "managed" .= _managedConversation t
      ]

instance FromJSON TeamConversation where
  parseJSON = withObject "team conversation" $ \o ->
    TeamConversation <$> o .: "conversation" <*> o .: "managed"

instance ToJSON TeamConversationList where
  toJSON t = object ["conversations" .= _teamConversations t]

instance FromJSON TeamConversationList where
  parseJSON = withObject "team conversation list" $ \o -> do
    TeamConversationList <$> o .: "conversations"

newTeamJson :: NewTeam a -> [Pair]
newTeamJson (NewTeam n i ik _) =
  "name" .= fromRange n
    # "icon" .= fromRange i
    # "icon_key" .= (fromRange <$> ik)
    # []

instance ToJSON BindingNewTeam where
  toJSON (BindingNewTeam t) = object $ newTeamJson t

instance ToJSON NonBindingNewTeam where
  toJSON (NonBindingNewTeam t) =
    object $
      "members" .= (fromRange <$> _newTeamMembers t)
        # newTeamJson t

deriving instance FromJSON BindingNewTeam

deriving instance FromJSON NonBindingNewTeam

instance FromJSON TeamDeleteData where
  parseJSON = withObject "team-delete-data" $ \o ->
    TeamDeleteData <$> o .: "password"

instance ToJSON TeamDeleteData where
  toJSON tdd =
    object
      [ "password" .= _tdAuthPassword tdd
      ]
