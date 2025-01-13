{-# OPTIONS_GHC -Wno-orphans #-}

module Wire.API.User.EmailAddress
  ( fromEmail,
    emailAddress,
    emailAddressText,
    module Text.Email.Parser,
    emailToSAMLNameID,
    emailFromSAML,
  )
where

-----
-- This is where we declare orphan instances
-----

import Cassandra.CQL qualified as C
import Data.ByteString.Conversion hiding (toByteString)
import Data.Data (Proxy (..))
import Data.OpenApi hiding (Schema, ToSchema)
import Data.Schema
import Data.Text hiding (null)
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

-- | Generates any Unicode character (but not a surrogate)
arbitraryValidMail :: Gen EmailAddress
arbitraryValidMail = do
  loc <- arbitrary `suchThat` isValidLoc
  dom <- (addTld <$> arbitrary) `suchThat` isValidDom
  pure . fromJust $ emailAddress (fromString $ loc <> "@" <> dom)
  where
    notAt :: String -> Bool
    notAt = notElem '@'

    -- at some places dotless domains do not work, so we add a tld
    addTld :: String -> String
    addTld str = if '.' `notElem` str then str <> ".tld" else str

    notNull = not . null

    isValidLoc :: String -> Bool
    isValidLoc x =
      notNull x
        && notAt x
        && isValid (fromString (x <> "@mail.com"))

    isValidDom :: String -> Bool
    isValidDom x =
      notNull x
        && notAt x
        && isValid (fromString ("me@" <> x))

-- | FUTUREWORK(fisx): if saml2-web-sso exported the 'NameID' constructor, we could make this
-- function total without all that praying and hoping.
emailToSAMLNameID :: EmailAddress -> Either String SAML.NameID
emailToSAMLNameID = SAML.emailNameID . fromEmail

emailFromSAML :: SAMLEmail.Email -> EmailAddress
emailFromSAML = fromJust . emailAddressText . SAMLEmail.render
