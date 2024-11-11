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
    teamSplashScreen,
    TeamBinding (..),
    Icon (..),

    -- * TeamList
    TeamList (..),
    newTeamList,
    teamListTeams,
    teamListHasMore,

    -- * NewTeam
    NewTeam (..),
    newTeamObjectSchema,
    newNewTeam,

    -- * TeamUpdateData
    TeamUpdateData (..),
    newTeamUpdateData,
    newTeamDeleteDataWithCode,
    nameUpdate,
    iconUpdate,
    iconKeyUpdate,
    splashScreenUpdate,

    -- * TeamDeleteData
    TeamDeleteData (..),
    newTeamDeleteData,
    tdAuthPassword,
    tdVerificationCode,
  )
where

import Control.Lens (makeLenses, over, (?~))
import Data.Aeson (FromJSON, ToJSON, Value (..), toJSON)
import Data.Aeson.Types (Parser)
import Data.Attoparsec.ByteString qualified as Atto (Parser, string)
import Data.Attoparsec.Combinator (choice)
import Data.ByteString.Conversion
import Data.Code qualified as Code
import Data.Id (TeamId, UserId)
import Data.Misc (PlainTextPassword6)
import Data.OpenApi (HasDeprecated (deprecated))
import Data.OpenApi qualified as S
import Data.Range
import Data.Schema
import Data.Text.Encoding qualified as T
import Imports
import Test.QuickCheck.Gen (suchThat)
import Wire.API.Asset (AssetKey)
import Wire.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))

--------------------------------------------------------------------------------
-- Team

data Team = Team
  { _teamId :: TeamId,
    _teamCreator :: UserId,
    _teamName :: Text,
    _teamIcon :: Icon,
    _teamIconKey :: Maybe Text,
    _teamBinding :: TeamBinding,
    _teamSplashScreen :: Icon
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Team)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema Team)

newTeam :: TeamId -> UserId -> Text -> Icon -> TeamBinding -> Team
newTeam tid uid nme ico tb = Team tid uid nme ico Nothing tb DefaultIcon

instance ToSchema Team where
  schema =
    objectWithDocModifier "Team" desc $
      Team
        <$> _teamId .= field "id" schema
        <*> _teamCreator .= field "creator" schema
        <*> _teamName .= field "name" schema
        <*> _teamIcon .= field "icon" schema
        <*> _teamIconKey .= maybe_ (optField "icon_key" schema)
        <*> _teamBinding .= (fromMaybe Binding <$> optFieldWithDocModifier "binding" bindingDesc schema)
        <*> _teamSplashScreen .= (fromMaybe DefaultIcon <$> optField "splash_screen" schema)
    where
      desc = description ?~ "`binding` is deprecated, and should be ignored. The non-binding teams API is not used (and will not be supported from API version V4 onwards), and `binding` will always be `true`."
      bindingDesc v =
        v
          & description ?~ "Deprecated, please ignore."
          & deprecated ?~ True

-- | How a team "binds" its members (users)
--
-- A `Binding` team is the normal team which we see in the UI. A user is
-- on-boarded as part of the team. If the team gets deleted/suspended the user
-- gets deleted/suspended.
--
-- A `NonBinding` team is a concept only in the backend. It is a team someone
-- can create and someone who has an account on Wire can join that team. This
-- way, in theory, one person can join many teams. This concept never made it as
-- a concept of product, but got used a lot of writing integration tests. Newer
-- features don't really work well with this and sometimes we have to rewrite
-- parts of the tests to use `Binding` teams.
--
-- Please try to not use `NonBinding` teams in tests anymore. In future, we
-- would like it to be deleted, but it is hard to delete because it requires a
-- bunch of tests to be rewritten.
data TeamBinding
  = Binding
  | NonBinding
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform TeamBinding)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema TeamBinding)

instance ToSchema TeamBinding where
  schema =
    over doc (deprecated ?~ True) $
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

instance ToSchema TeamList where
  schema =
    object "TeamList" $
      TeamList
        <$> _teamListTeams .= field "teams" (array schema)
        <*> _teamListHasMore .= field "has_more" schema

--------------------------------------------------------------------------------
-- NewTeam

data NewTeam = NewTeam
  { newTeamName :: Range 1 256 Text,
    newTeamIcon :: Icon,
    newTeamIconKey :: Maybe (Range 1 256 Text)
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema NewTeam)
  deriving (Arbitrary) via (GenericUniform NewTeam)

newTeamObjectSchema :: ObjectSchema SwaggerDoc NewTeam
newTeamObjectSchema =
  NewTeam
    <$> newTeamName .= fieldWithDocModifier "name" (description ?~ "team name") schema
    <*> newTeamIcon .= field "icon" schema
    <*> newTeamIconKey .= maybe_ (optFieldWithDocModifier "icon_key" (description ?~ "The decryption key for the team icon S3 asset") schema)

instance ToSchema NewTeam where
  schema = object "NewTeam" newTeamObjectSchema

newNewTeam :: Range 1 256 Text -> Icon -> NewTeam
newNewTeam nme ico = NewTeam nme ico Nothing

--------------------------------------------------------------------------------
-- TeamUpdateData

data Icon = Icon AssetKey | DefaultIcon
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Icon)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema Icon

instance FromByteString Icon where
  parser =
    choice
      [ Icon <$> (parser :: Atto.Parser AssetKey),
        DefaultIcon <$ Atto.string "default"
      ]

instance ToByteString Icon where
  builder (Icon key) = builder key
  builder DefaultIcon = "default"

instance ToSchema Icon where
  schema =
    (T.decodeUtf8 . toByteString')
      .= parsedTextWithDoc desc "Icon" (runParser parser . T.encodeUtf8)
      & doc' . S.schema . S.example ?~ toJSON ("3-1-47de4580-ae51-4650-acbb-d10c028cb0ac" :: Text)
    where
      desc =
        "S3 asset key for an icon image with retention information. Allows special value 'default'."

data TeamUpdateData = TeamUpdateData
  { _nameUpdate :: Maybe (Range 1 256 Text),
    _iconUpdate :: Maybe Icon,
    _iconKeyUpdate :: Maybe (Range 1 256 Text),
    _splashScreenUpdate :: Maybe Icon
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema TeamUpdateData)

instance Arbitrary TeamUpdateData where
  arbitrary = arb `suchThat` valid
    where
      arb = TeamUpdateData <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      valid (TeamUpdateData Nothing Nothing Nothing Nothing) = False
      valid _ = True

newTeamUpdateData :: TeamUpdateData
newTeamUpdateData = TeamUpdateData Nothing Nothing Nothing Nothing

validateTeamUpdateData :: TeamUpdateData -> Parser TeamUpdateData
validateTeamUpdateData u =
  when
    (isNothing (_nameUpdate u) && isNothing (_iconUpdate u) && isNothing (_iconKeyUpdate u) && isNothing (_splashScreenUpdate u))
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
        <*> _splashScreenUpdate .= maybe_ (optField "splash_screen" schema)

--------------------------------------------------------------------------------
-- TeamDeleteData

data TeamDeleteData = TeamDeleteData
  { _tdAuthPassword :: Maybe PlainTextPassword6,
    _tdVerificationCode :: Maybe Code.Value
  }
  deriving stock (Eq, Show)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema TeamDeleteData)

instance Arbitrary TeamDeleteData where
  arbitrary = TeamDeleteData <$> arbitrary <*> arbitrary

newTeamDeleteData :: Maybe PlainTextPassword6 -> TeamDeleteData
newTeamDeleteData = flip TeamDeleteData Nothing

newTeamDeleteDataWithCode :: Maybe PlainTextPassword6 -> Maybe Code.Value -> TeamDeleteData
newTeamDeleteDataWithCode = TeamDeleteData

instance ToSchema TeamDeleteData where
  schema =
    object "TeamDeleteData" $
      TeamDeleteData
        <$> _tdAuthPassword .= optField "password" (maybeWithDefault Null schema)
        <*> _tdVerificationCode .= maybe_ (optField "verification_code" schema)

makeLenses ''Team
makeLenses ''TeamList
makeLenses ''TeamUpdateData
makeLenses ''TeamDeleteData
