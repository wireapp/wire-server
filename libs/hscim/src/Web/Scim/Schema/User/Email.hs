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

module Web.Scim.Schema.User.Email where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Text hiding (dropWhile)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import qualified Text.Email.Validate as Email
import Web.Scim.Schema.Common hiding (value)

newtype EmailAddress = EmailAddress
  {unEmailAddress :: Email.EmailAddress}
  deriving (Show, Eq)

instance FromJSON EmailAddress where
  parseJSON = withText "Email" $ \e -> case Email.emailAddress (encodeUtf8 e) of
    Nothing -> fail "Invalid email"
    Just some -> pure $ EmailAddress some

instance ToJSON EmailAddress where
  toJSON (EmailAddress e) = String $ decodeUtf8 . Email.toByteString $ e

data Email = Email
  { typ :: Maybe Text, -- Work, private, and so on
    value :: EmailAddress,
    primary :: Maybe ScimBool
  }
  deriving (Show, Eq, Generic)

instance FromJSON Email where
  parseJSON = either (fail . show) (genericParseJSON parseOptions) . jsonLower

instance ToJSON Email where
  toJSON = genericToJSON serializeOptions

emailToEmailAddress :: Email -> Email.EmailAddress
emailToEmailAddress = unEmailAddress . value

scimEmailsToEmailAddress :: [Email] -> Maybe Email.EmailAddress
scimEmailsToEmailAddress es = pickPrimary es <|> pickFirst es
  where
    pickFirst [] = Nothing
    pickFirst (e : _) = Just (unEmailAddress (value e))

    pickPrimary = pickFirst . Prelude.filter isPrimary

    isPrimary e = primary e == Just (ScimBool True)
