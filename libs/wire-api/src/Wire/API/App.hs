{-# LANGUAGE DeriveAnyClass #-}

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
import Data.OpenApi qualified as S
import Data.Schema
import Data.Text qualified as TS
import Imports
import Data.Range
import Wire.API.User
import Wire.API.User.Auth

data NewApp = NewApp
  { name :: Name,
    pict :: Pict,
    assets :: [Asset],
    accentId :: ColourId,
    meta :: A.Object,
    category :: Category,
    description :: Range 1 300 Text,
    author :: Range 1 256 Text
  }
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema NewApp

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
  deriving anyclass (A.FromJSON, A.ToJSON)

categoryFromText :: Text -> Either Text Category
categoryFromText t = case attempt TS.toTitle t of -- XXX: Use ToSchema to parse Category, as the mapping between the type and lower case strings is already expressed there?
  Left _ -> attempt TS.toUpper t
  Right a -> Right a
  where
    attempt f t' = case readEither $ TS.unpack $ f t' of
      Left err -> Left $ TS.pack err
      Right a -> Right a

categoryToText :: Category -> Text
categoryToText = TS.toLower . TS.pack . show

instance ToSchema Category where
  schema =
    enum @Text "Category" $
      mconcat
        [ element "security" Security,
          element "collaboration" Collaboration,
          element "productivity" Productivity,
          element "automation" Automation,
          element "files" Files,
          element "ai" AI,
          element "developer" Developer,
          element "support" Support,
          element "finance" Finance,
          element "hr" HR,
          element "integration" Integration,
          element "compliance" Compliance,
          element "other" Other
        ]

instance ToSchema NewApp where
  schema =
    object "NewApp" $
      NewApp
        <$> (.name) .= field "name" schema
        <*> (.pict) .= (fromMaybe noPict <$> optField "picture" schema)
        <*> (.assets) .= (fromMaybe [] <$> optField "assets" (array schema))
        <*> (.accentId) .= (fromMaybe defaultAccentId <$> optField "accent_id" schema)
        <*> (.meta) .= field "metadata" jsonObject
        <*> (.category) .= field "category" schema
        <*> (.description) .= field "description" schema
        <*> (.author) .= field "author" schema

defNewApp :: Name -> NewApp
defNewApp name =
  NewApp
    { name,
      pict = noPict,
      assets = [],
      accentId = defaultAccentId,
      meta = mempty,
      category = Other,
      description = unsafeRange "",
      author = unsafeRange ""
    }

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
