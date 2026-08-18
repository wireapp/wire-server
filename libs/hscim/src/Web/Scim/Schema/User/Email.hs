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

import Data.Aeson
import Data.Text hiding (dropWhile, show)
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

-- | Reduce a list of SCIM emails to the single address Wire stores.
--
-- Wire/brig holds at most one email per user, so the (possibly multi-valued)
-- SCIM @emails@ attribute must be reduced to one address. Selection rule:
-- the entry marked @primary@ (RFC 7643 §2.4: @primary@ value @true@ MUST
-- appear no more than once), else the first entry. Per RFC 7643 §2.4 an
-- absent @primary@ is assumed @false@; with none marked primary, Wire
-- deterministically picks the first entry (it must store exactly one email).
--
-- If more than one entry is marked @primary@ — a client-side protocol
-- violation — this returns 'Left' with a descriptive message so the caller
-- rejects the request instead of silently picking one.
scimEmailsToEmailAddress :: [Email] -> Either Text (Maybe Email.EmailAddress)
scimEmailsToEmailAddress es =
  case primaries of
    [] -> Right Nothing
    [primaryEmail] -> Right . Just . unEmailAddress $ value primaryEmail
    _ -> Left "More than one email is marked as primary; RFC 7643 §2.4 allows at most one."
  where
    primaries = Prelude.filter isPrimary es

    isPrimary e = primary e == Just (ScimBool True)
