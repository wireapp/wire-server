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

module Wire.API.Team
  ( -- * Team
    Team (..),
    newTeam,
    teamId,
    teamCreator,
    teamName,
    teamIcon,
    teamIconKey,
    teamBinding,
    TeamBinding (..),

    -- * TeamList
    TeamList (..),
    newTeamList,
    teamListTeams,
    teamListHasMore,

    -- * NewTeam
    BindingNewTeam (..),
    NonBindingNewTeam (..),
    NewTeam (..),
    newNewTeam,
    newTeamName,
    newTeamIcon,
    newTeamIconKey,
    newTeamMembers,

    -- * TeamUpdateData
    TeamUpdateData (..),
    newTeamUpdateData,
    nameUpdate,
    iconUpdate,
    iconKeyUpdate,

    -- * TeamDeleteData
    TeamDeleteData (..),
    newTeamDeleteData,
    tdAuthPassword,

    -- * Swagger
    modelTeam,
    modelTeamList,
    modelNewBindingTeam,
    modelNewNonBindingTeam,
    modelUpdateData,
    modelTeamDelete,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Id (TeamId, UserId)
import Data.Json.Util
import Data.Misc (PlainTextPassword (..))
import Data.Range
import qualified Data.Swagger.Build.Api as Doc
import Imports
import Wire.API.Team.Member (TeamMember, modelTeamMember)

--------------------------------------------------------------------------------
-- Team

data Team = Team
  { _teamId :: TeamId,
    _teamCreator :: UserId,
    _teamName :: Text,
    _teamIcon :: Text,
    _teamIconKey :: Maybe Text,
    _teamBinding :: TeamBinding
  }
  deriving (Eq, Show, Generic)

newTeam :: TeamId -> UserId -> Text -> Text -> TeamBinding -> Team
newTeam tid uid nme ico bnd = Team tid uid nme ico Nothing bnd

modelTeam :: Doc.Model
modelTeam = Doc.defineModel "Team" $ do
  Doc.description "Team information"
  Doc.property "id" Doc.bytes' $
    Doc.description "team ID"
  Doc.property "creator" Doc.bytes' $
    Doc.description "team creator's user ID"
  Doc.property "name" Doc.string' $
    Doc.description "team name"
  Doc.property "icon" Doc.string' $
    Doc.description "team icon (asset ID)"
  Doc.property "icon_key" Doc.string' $ do
    Doc.description "team icon asset key"
    Doc.optional
  Doc.property "binding" Doc.bool' $
    Doc.description "user binding team"

instance ToJSON Team where
  toJSON t =
    object $
      "id" .= _teamId t
        # "creator" .= _teamCreator t
        # "name" .= _teamName t
        # "icon" .= _teamIcon t
        # "icon_key" .= _teamIconKey t
        # "binding" .= _teamBinding t
        # []

instance FromJSON Team where
  parseJSON = withObject "team" $ \o -> do
    Team <$> o .: "id"
      <*> o .: "creator"
      <*> o .: "name"
      <*> o .: "icon"
      <*> o .:? "icon_key"
      <*> o .:? "binding" .!= NonBinding

data TeamBinding
  = Binding
  | NonBinding
  deriving (Eq, Show, Generic)

instance ToJSON TeamBinding where
  toJSON Binding = Bool True
  toJSON NonBinding = Bool False

instance FromJSON TeamBinding where
  parseJSON (Bool True) = pure Binding
  parseJSON (Bool False) = pure NonBinding
  parseJSON other = fail $ "Unknown binding type: " <> show other

--------------------------------------------------------------------------------
-- TeamList

data TeamList = TeamList
  { _teamListTeams :: [Team],
    _teamListHasMore :: Bool
  }
  deriving (Show, Generic)

newTeamList :: [Team] -> Bool -> TeamList
newTeamList = TeamList

modelTeamList :: Doc.Model
modelTeamList = Doc.defineModel "TeamList" $ do
  Doc.description "list of teams"
  Doc.property "teams" (Doc.unique $ Doc.array (Doc.ref modelTeam)) $
    Doc.description "the Doc.array of teams"
  Doc.property "has_more" Doc.bool' $
    Doc.description "if more teams are available"

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

--------------------------------------------------------------------------------
-- NewTeam

newtype BindingNewTeam = BindingNewTeam (NewTeam ())
  deriving (Eq, Show, Generic)

modelNewBindingTeam :: Doc.Model
modelNewBindingTeam = Doc.defineModel "NewBindingTeam" $ do
  Doc.description "Required data when creating new teams"
  Doc.property "name" Doc.string' $
    Doc.description "team name"
  Doc.property "icon" Doc.string' $
    Doc.description "team icon (asset ID)"
  Doc.property "icon_key" Doc.string' $ do
    Doc.description "team icon asset key"
    Doc.optional

instance ToJSON BindingNewTeam where
  toJSON (BindingNewTeam t) = object $ newTeamJson t

newTeamJson :: NewTeam a -> [Pair]
newTeamJson (NewTeam n i ik _) =
  "name" .= fromRange n
    # "icon" .= fromRange i
    # "icon_key" .= (fromRange <$> ik)
    # []

deriving instance FromJSON BindingNewTeam

-- | FUTUREWORK: this is dead code!  remove!
newtype NonBindingNewTeam = NonBindingNewTeam (NewTeam (Range 1 127 [TeamMember]))
  deriving (Eq, Show, Generic)

modelNewNonBindingTeam :: Doc.Model
modelNewNonBindingTeam = Doc.defineModel "newNonBindingTeam" $ do
  Doc.description "Required data when creating new regular teams"
  Doc.property "name" Doc.string' $
    Doc.description "team name"
  Doc.property "icon" Doc.string' $
    Doc.description "team icon (asset ID)"
  Doc.property "icon_key" Doc.string' $ do
    Doc.description "team icon asset key"
    Doc.optional
  Doc.property "members" (Doc.unique $ Doc.array (Doc.ref modelTeamMember)) $ do
    Doc.description "initial team member ids (between 1 and 127)"
    Doc.optional

instance ToJSON NonBindingNewTeam where
  toJSON (NonBindingNewTeam t) =
    object $
      "members" .= (fromRange <$> _newTeamMembers t)
        # newTeamJson t

deriving instance FromJSON NonBindingNewTeam

data NewTeam a = NewTeam
  { _newTeamName :: Range 1 256 Text,
    _newTeamIcon :: Range 1 256 Text,
    _newTeamIconKey :: Maybe (Range 1 256 Text),
    _newTeamMembers :: Maybe a
  }
  deriving (Eq, Show, Generic)

newNewTeam :: Range 1 256 Text -> Range 1 256 Text -> NewTeam a
newNewTeam nme ico = NewTeam nme ico Nothing Nothing

instance (FromJSON a) => FromJSON (NewTeam a) where
  parseJSON = withObject "new-team" $ \o -> do
    name <- o .: "name"
    icon <- o .: "icon"
    key <- o .:? "icon_key"
    mems <- o .:? "members"
    either fail pure $
      NewTeam <$> checkedEitherMsg "name" name
        <*> checkedEitherMsg "icon" icon
        <*> maybe (pure Nothing) (fmap Just . checkedEitherMsg "icon_key") key
        <*> pure mems

--------------------------------------------------------------------------------
-- TeamUpdateData

data TeamUpdateData = TeamUpdateData
  { _nameUpdate :: Maybe (Range 1 256 Text),
    _iconUpdate :: Maybe (Range 1 256 Text),
    _iconKeyUpdate :: Maybe (Range 1 256 Text)
  }
  deriving (Eq, Show, Generic)

modelUpdateData :: Doc.Model
modelUpdateData = Doc.defineModel "TeamUpdateData" $ do
  Doc.description "team update data"
  Doc.property "name" Doc.string' $ do
    Doc.description "new team name"
    Doc.optional
  Doc.property "icon" Doc.string' $ do
    Doc.description "new icon asset id"
    Doc.optional
  Doc.property "icon_key" Doc.string' $ do
    Doc.description "new icon asset key"
    Doc.optional

newTeamUpdateData :: TeamUpdateData
newTeamUpdateData = TeamUpdateData Nothing Nothing Nothing

instance ToJSON TeamUpdateData where
  toJSON u =
    object $
      "name" .= _nameUpdate u
        # "icon" .= _iconUpdate u
        # "icon_key" .= _iconKeyUpdate u
        # []

instance FromJSON TeamUpdateData where
  parseJSON = withObject "team update data" $ \o -> do
    name <- o .:? "name"
    icon <- o .:? "icon"
    icon_key <- o .:? "icon_key"
    when (isNothing name && isNothing icon && isNothing icon_key) $
      fail "TeamUpdateData: no update data specified"
    either fail pure $
      TeamUpdateData <$> maybe (pure Nothing) (fmap Just . checkedEitherMsg "name") name
        <*> maybe (pure Nothing) (fmap Just . checkedEitherMsg "icon") icon
        <*> maybe (pure Nothing) (fmap Just . checkedEitherMsg "icon_key") icon_key

--------------------------------------------------------------------------------
-- TeamDeleteData

newtype TeamDeleteData = TeamDeleteData
  { _tdAuthPassword :: Maybe PlainTextPassword
  }

newTeamDeleteData :: Maybe PlainTextPassword -> TeamDeleteData
newTeamDeleteData = TeamDeleteData

-- FUTUREWORK: fix name of model? (upper case)
modelTeamDelete :: Doc.Model
modelTeamDelete = Doc.defineModel "teamDeleteData" $ do
  Doc.description "Data for a team deletion request in case of binding teams."
  Doc.property "password" Doc.string' $
    Doc.description "The account password to authorise the deletion."

instance FromJSON TeamDeleteData where
  parseJSON = withObject "team-delete-data" $ \o ->
    TeamDeleteData <$> o .: "password"

instance ToJSON TeamDeleteData where
  toJSON tdd =
    object
      [ "password" .= _tdAuthPassword tdd
      ]

makeLenses ''Team
makeLenses ''TeamList
makeLenses ''NewTeam
makeLenses ''TeamUpdateData
makeLenses ''TeamDeleteData
