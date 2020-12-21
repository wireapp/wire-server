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

module Web.Scim.Schema.User.Email where

import Data.Aeson
import Data.Text hiding (dropWhile)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Text.Email.Validate
import Web.Scim.Schema.Common

newtype EmailAddress2 = EmailAddress2
  {unEmailAddress :: EmailAddress}
  deriving (Show, Eq)

instance FromJSON EmailAddress2 where
  parseJSON = withText "Email" $ \e -> case emailAddress (encodeUtf8 e) of
    Nothing -> fail "Invalid email"
    Just some -> pure $ EmailAddress2 some

instance ToJSON EmailAddress2 where
  toJSON (EmailAddress2 e) = String $ decodeUtf8 . toByteString $ e

data Email = Email
  { typ :: Maybe Text,
    value :: EmailAddress2,
    primary :: Maybe ScimBool
  }
  deriving (Show, Eq, Generic)

instance FromJSON Email where
  parseJSON = genericParseJSON parseOptions . jsonLower

instance ToJSON Email where
  toJSON = genericToJSON serializeOptions
