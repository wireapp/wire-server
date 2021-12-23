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
import Data.Aeson (FromJSON, ToJSON, Value (..))
import Data.Aeson.Types (Parser)
import Data.Id (TeamId, UserId)
import Data.Misc (PlainTextPassword (..))
import Data.Range
import Data.Schema
import Data.Singletons (sing)
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import Imports
import Test.QuickCheck.Gen (suchThat)
import Wire.API.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))
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
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Team)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema Team)

newTeam :: TeamId -> UserId -> Text -> Text -> TeamBinding -> Team
newTeam tid uid nme ico = Team tid uid nme ico Nothing

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

instance ToSchema Team where
  schema =
    object "Team" $
      Team
        <$> _teamId .= field "id" schema
        <*> _teamCreator .= field "creator" schema
        <*> _teamName .= field "name" schema
        <*> _teamIcon .= field "icon" schema
        <*> _teamIconKey .= maybe_ (optField "icon_key" schema)
        <*> _teamBinding .= (fromMaybe Binding <$> optField "binding" schema)

data TeamBinding
  = Binding
  | NonBinding
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform TeamBinding)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema TeamBinding)

instance ToSchema TeamBinding where
  schema =
    enum @Bool "TeamBinding" $
      mconcat [element True Binding, element False NonBinding]

--------------------------------------------------------------------------------
-- TeamList

data TeamList = TeamList
  { _teamListTeams :: [Team],
    _teamListHasMore :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform TeamList)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema TeamList)

newTeamList :: [Team] -> Bool -> TeamList
newTeamList = TeamList

modelTeamList :: Doc.Model
modelTeamList = Doc.defineModel "TeamList" $ do
  Doc.description "list of teams"
  Doc.property "teams" (Doc.unique $ Doc.array (Doc.ref modelTeam)) $
    Doc.description "the Doc.array of teams"
  Doc.property "has_more" Doc.bool' $
    Doc.description "if more teams are available"

instance ToSchema TeamList where
  schema =
    object "TeamList" $
      TeamList <$> _teamListTeams .= field "teams" (array schema)
        <*> _teamListHasMore .= field "has_more" schema

--------------------------------------------------------------------------------
-- NewTeam

newtype BindingNewTeam = BindingNewTeam (NewTeam ())
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema BindingNewTeam)

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

instance ToSchema BindingNewTeam where
  schema = object "BindingNewTeam" $ BindingNewTeam <$> unwrap .= ntUnitSchema
    where
      unwrap (BindingNewTeam nt) = nt
      ntUnitSchema =
        mkTeam
          <$> _newTeamName .= field "name" schema
          <*> _newTeamIcon .= field "icon" schema
          <*> _newTeamIconKey .= maybe_ (optField "icon_key" schema)
        where
          mkTeam n i ik = NewTeam n i ik (Just ())

-- FUTUREWORK: since new team members do not get serialized, we zero them here.
-- it may be worth looking into how this can be solved in the types.
instance Arbitrary BindingNewTeam where
  arbitrary =
    BindingNewTeam . zeroTeamMembers <$> arbitrary @(NewTeam ())
    where
      zeroTeamMembers tms = tms {_newTeamMembers = Nothing}

-- | FUTUREWORK: this is dead code!  remove!
newtype NonBindingNewTeam = NonBindingNewTeam (NewTeam (Range 1 127 [TeamMember]))
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema NonBindingNewTeam)

ntTeamMemberRangeSchema :: ObjectSchema SwaggerDoc (NewTeam (Range 1 127 [TeamMember]))
ntTeamMemberRangeSchema =
  NewTeam
    <$> _newTeamName .= field "name" schema
    <*> _newTeamIcon .= field "icon" schema
    <*> _newTeamIconKey .= maybe_ (optField "icon_key" schema)
    <*> (fmap fromRange . _newTeamMembers) .= maybe_ (optField "members" (rangedSchema sing sing (array schema)))

instance ToSchema NonBindingNewTeam where
  schema = object "NonBindingNewTeam" $ NonBindingNewTeam <$> unwrap .= ntTeamMemberRangeSchema
    where
      unwrap (NonBindingNewTeam nt) = nt

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

data NewTeam a = NewTeam
  { _newTeamName :: Range 1 256 Text,
    _newTeamIcon :: Range 1 256 Text,
    _newTeamIconKey :: Maybe (Range 1 256 Text),
    _newTeamMembers :: Maybe a
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform (NewTeam a))
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema (NewTeam a))

newNewTeam :: Range 1 256 Text -> Range 1 256 Text -> NewTeam a
newNewTeam nme ico = NewTeam nme ico Nothing Nothing

instance (ToSchema a) => ToSchema (NewTeam a) where
  schema =
    object "NewTeam" $
      NewTeam
        <$> _newTeamName .= field "name" schema
        <*> _newTeamIcon .= field "icon" schema
        <*> _newTeamIconKey .= maybe_ (optField "icon_key" schema)
        <*> _newTeamMembers .= maybe_ (optField "members" schema)

--------------------------------------------------------------------------------
-- TeamUpdateData

data TeamUpdateData = TeamUpdateData
  { _nameUpdate :: Maybe (Range 1 256 Text),
    _iconUpdate :: Maybe (Range 1 256 Text),
    _iconKeyUpdate :: Maybe (Range 1 256 Text)
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema TeamUpdateData)

instance Arbitrary TeamUpdateData where
  arbitrary = arb `suchThat` valid
    where
      arb = TeamUpdateData <$> arbitrary <*> arbitrary <*> arbitrary
      valid (TeamUpdateData Nothing Nothing Nothing) = False
      valid _ = True

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

validateTeamUpdateData :: TeamUpdateData -> Parser TeamUpdateData
validateTeamUpdateData u =
  when
    (isNothing (_nameUpdate u) && isNothing (_iconUpdate u) && isNothing (_iconKeyUpdate u))
    (fail "TeamUpdateData: no update data specified")
    $> u

instance ToSchema TeamUpdateData where
  schema =
    (`withParser` validateTeamUpdateData)
      . object "TeamUpdateData"
      $ TeamUpdateData
        <$> _nameUpdate .= maybe_ (optField "name" schema)
        <*> _iconUpdate .= maybe_ (optField "icon" schema)
        <*> _iconKeyUpdate .= maybe_ (optField "icon_key" schema)

--------------------------------------------------------------------------------
-- TeamDeleteData

newtype TeamDeleteData = TeamDeleteData
  { _tdAuthPassword :: Maybe PlainTextPassword
  }
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema TeamDeleteData)

newTeamDeleteData :: Maybe PlainTextPassword -> TeamDeleteData
newTeamDeleteData = TeamDeleteData

-- FUTUREWORK: fix name of model? (upper case)
modelTeamDelete :: Doc.Model
modelTeamDelete = Doc.defineModel "teamDeleteData" $ do
  Doc.description "Data for a team deletion request in case of binding teams."
  Doc.property "password" Doc.string' $
    Doc.description "The account password to authorise the deletion."

instance ToSchema TeamDeleteData where
  schema =
    object "TeamDeleteData" $
      TeamDeleteData <$> _tdAuthPassword .= optField "password" (maybeWithDefault Null schema)

makeLenses ''Team
makeLenses ''TeamList
makeLenses ''NewTeam
makeLenses ''TeamUpdateData
makeLenses ''TeamDeleteData
