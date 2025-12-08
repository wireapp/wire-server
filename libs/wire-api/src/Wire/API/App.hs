-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.App where

import Data.Aeson qualified as A
import Data.HashMap.Strict qualified as HM
import Data.Misc
import Data.OpenApi qualified as S
import Data.Range
import Data.Schema
import Imports
import Wire.API.User
import Wire.API.User.Auth
import Wire.Arbitrary as Arbitrary

data NewApp = NewApp
  { app :: GetApp,
    password :: PlainTextPassword6
  }
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema NewApp

data GetApp = GetApp
  { name :: Name,
    pict :: Pict,
    assets :: [Asset],
    accentId :: ColourId,
    meta :: A.Object,
    category :: Category,
    description :: Range 0 300 Text
  }
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema GetApp

data Category
  = Security
  | Collaboration
  | Productivity
  | Automation
  | Files
  | AI
  | Developer
  | Support
  | Finance
  | HR
  | Integration
  | Compliance
  | Other
  deriving (Eq, Ord, Show, Read, Generic)
  deriving (Arbitrary) via GenericUniform Category

instance A.FromJSON Category where parseJSON = schemaParseJSON

instance A.ToJSON Category where toJSON = schemaToJSON

categoryTextMapping :: [(Text, Category)]
categoryTextMapping =
  [ ("security", Security),
    ("collaboration", Collaboration),
    ("productivity", Productivity),
    ("automation", Automation),
    ("files", Files),
    ("ai", AI),
    ("developer", Developer),
    ("support", Support),
    ("finance", Finance),
    ("hr", HR),
    ("integration", Integration),
    ("compliance", Compliance),
    ("other", Other)
  ]

categoryMap :: HM.HashMap Text Category
categoryMap = HM.fromList categoryTextMapping

categoryFromText :: Text -> Maybe Category
categoryFromText text' = HM.lookup text' categoryMap

categoryToText :: Category -> Text
categoryToText = \case
  Security -> "security"
  Collaboration -> "collaboration"
  Productivity -> "productivity"
  Automation -> "automation"
  Files -> "files"
  AI -> "ai"
  Developer -> "developer"
  Support -> "support"
  Finance -> "finance"
  HR -> "hr"
  Integration -> "integration"
  Compliance -> "compliance"
  Other -> "other"

instance ToSchema Category where
  schema =
    enum @Text "Category" $
      mconcat $
        map (uncurry element) categoryTextMapping

instance ToSchema NewApp where
  schema =
    object "NewApp" $
      NewApp
        <$> (.app) .= field "app" schema
        <*> (.password) .= field "password" schema

instance ToSchema GetApp where
  schema =
    object "GetApp" $
      GetApp
        <$> (.name) .= field "name" schema
        <*> (.pict) .= (fromMaybe noPict <$> optField "picture" schema)
        <*> (.assets) .= (fromMaybe [] <$> optField "assets" (array schema))
        <*> (.accentId) .= (fromMaybe defaultAccentId <$> optField "accent_id" schema)
        <*> (.meta) .= field "metadata" jsonObject
        <*> (.category) .= field "category" schema
        <*> (.description) .= field "description" schema

data CreatedApp = CreatedApp
  { user :: User,
    cookie :: SomeUserToken
  }
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema CreatedApp

instance ToSchema CreatedApp where
  schema =
    object "CreatedApp" $
      CreatedApp
        <$> (.user) .= field "user" schema
        <*> (.cookie) .= field "cookie" schema

newtype RefreshAppCookieResponse = RefreshAppCookieResponse
  {cookie :: SomeUserToken}
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema RefreshAppCookieResponse

instance ToSchema RefreshAppCookieResponse where
  schema =
    object "RefreshAppCookieResponse" $
      RefreshAppCookieResponse <$> (.cookie) .= field "cookie" schema
