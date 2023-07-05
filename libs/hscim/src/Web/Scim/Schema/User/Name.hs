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

module Web.Scim.Schema.User.Name where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Web.Scim.Schema.Common

data Name = Name
  { formatted :: Maybe Text,
    familyName :: Maybe Text,
    givenName :: Maybe Text,
    middleName :: Maybe Text,
    honorificPrefix :: Maybe Text,
    honorificSuffix :: Maybe Text
  }
  deriving (Show, Eq, Generic)

emptyName :: Name
emptyName = Name Nothing Nothing Nothing Nothing Nothing Nothing

instance FromJSON Name where
  parseJSON = either (fail . show) (genericParseJSON parseOptions) . jsonLower

instance ToJSON Name where
  toJSON = genericToJSON serializeOptions
