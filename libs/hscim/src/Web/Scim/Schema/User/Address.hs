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

module Web.Scim.Schema.User.Address where

import Data.Aeson
import Data.Text hiding (dropWhile)
import GHC.Generics (Generic)
import Web.Scim.Schema.Common

data Address = Address
  { formatted :: Maybe Text,
    streetAddress :: Maybe Text,
    locality :: Maybe Text,
    region :: Maybe Text,
    postalCode :: Maybe Text,
    -- TODO: country is specified as ISO3166, but example uses "USA"
    country :: Maybe Text,
    typ :: Maybe Text,
    primary :: Maybe ScimBool
  }
  deriving (Show, Eq, Generic)

instance FromJSON Address where
  parseJSON = either (fail . show) (genericParseJSON parseOptions) . jsonLower

instance ToJSON Address where
  toJSON = genericToJSON serializeOptions
