{-# OPTIONS_GHC -Wno-orphans #-}

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

module Wire.API.User.EmailAddress
  ( fromEmail,
    emailAddress,
    emailAddressText,
    module Text.Email.Parser,
    emailToSAMLNameID,
    emailFromSAML,
    emailDomain,
  )
where

-----
-- This is where we declare orphan instances
-----

import Cassandra.CQL qualified as C
import Data.ByteString.Conversion hiding (toByteString)
import Data.Data (Proxy (..))
import Data.Domain
import Data.OpenApi hiding (Schema, ToSchema)
import Data.Schema
import Data.Text hiding (null)
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Deriving.Aeson
import Imports
import SAML2.WebSSO.Types qualified as SAML
import SAML2.WebSSO.Types.Email qualified as SAMLEmail
import Servant.API qualified as S
import Test.QuickCheck
import Text.Email.Parser
import Text.Email.Validate

--------------------------------------------------------------------------------
-- Email

instance ToByteString EmailAddress where
  builder = builder . fromEmail

instance FromByteString EmailAddress where
  parser = parser >>= maybe (fail "Invalid email") pure . emailAddress

deriving via (Schema EmailAddress) instance ToJSON EmailAddress

deriving via (Schema EmailAddress) instance FromJSON EmailAddress

instance ToParamSchema EmailAddress where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance ToSchema EmailAddress where
  schema =
    fromEmail
      .= parsedText
        "Email"
        ( maybe
            (Left "Invalid email. Expected '<local>@<domain>'.")
            pure
            . emailAddressText
        )

instance S.FromHttpApiData EmailAddress where
  parseUrlPiece = maybe (Left "Invalid email") Right . fromByteString . encodeUtf8

instance S.ToHttpApiData EmailAddress where
  toUrlPiece = decodeUtf8With lenientDecode . toByteString'

instance Arbitrary EmailAddress where
  -- By generating arbitrary Text and then encoding as bytestrings
  -- we avoid the risk of generating invalid UTF-8 bytes.
  arbitrary = arbitraryValidMail

-- loc <- fromString <$> listOf1 arbitraryMailString
-- dom <- fromString <$> listOf1 arbitraryMailString
-- pure $ unsafeEmailAddress loc dom

instance C.Cql EmailAddress where
  ctype = C.Tagged C.TextColumn

  fromCql (C.CqlText t) = case emailAddressText t of
    Just e -> pure e
    Nothing -> Left "fromCql: Invalid email"
  fromCql _ = Left "fromCql: email: CqlText expected"

  toCql = C.toCql . fromEmail

fromEmail :: EmailAddress -> Text
fromEmail = decodeUtf8 . toByteString

emailAddressText :: Text -> Maybe EmailAddress
emailAddressText = emailAddress . encodeUtf8

emailDomain :: EmailAddress -> Either String Domain
emailDomain = mkDomainFromBS . domainPart

-- | Generates any Unicode character (but not a surrogate)
arbitraryValidMail :: Gen EmailAddress
arbitraryValidMail = do
  loc <- arbitrary `suchThat` isValidLoc
  Domain dom <- arbitrary
  pure . fromJust $ emailAddress (fromString $ loc <> "@" <> T.unpack dom)
  where
    notAt :: String -> Bool
    notAt = notElem '@'

    notNull = not . null

    isValidLoc :: String -> Bool
    isValidLoc x =
      notNull x
        && notAt x
        && isValid (fromString (x <> "@mail.example"))

-- | FUTUREWORK(fisx): if saml2-web-sso exported the 'NameID' constructor, we could make this
-- function total without all that praying and hoping.
emailToSAMLNameID :: EmailAddress -> Either String SAML.NameID
emailToSAMLNameID = SAML.emailNameID . fromEmail

emailFromSAML :: SAMLEmail.Email -> EmailAddress
emailFromSAML = fromJust . emailAddressText . SAMLEmail.render
