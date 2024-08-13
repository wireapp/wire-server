{-# OPTIONS_GHC -Wno-orphans #-}

module Wire.API.User.EmailAddress
  ( fromEmail,
    parseEmail,
    module Text.Email.Parser,
  )
where

-----
-- This is where we declare orphan instances
-----

import Cassandra.CQL qualified as C
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Conversion
import Data.Data (Proxy (..))
import Data.OpenApi hiding (Schema, ToSchema)
import Data.Schema
import Data.Text
import Data.Text qualified as Text
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Deriving.Aeson
import Imports
import Servant.API qualified as S
import Text.Email.Parser
import Wire.Arbitrary (Arbitrary (arbitrary))

--------------------------------------------------------------------------------
-- Email

instance ToByteString EmailAddress where
  builder = builder . fromEmail

instance FromByteString EmailAddress where
  parser = parser >>= maybe (fail "Invalid email") pure . parseEmail

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
            . parseEmail
        )

instance S.FromHttpApiData EmailAddress where
  parseUrlPiece = maybe (Left "Invalid email") Right . fromByteString . encodeUtf8

instance S.ToHttpApiData EmailAddress where
  toUrlPiece = decodeUtf8With lenientDecode . toByteString'

instance Arbitrary EmailAddress where
  arbitrary = do
    loc <- BS.filter (/= '@') <$> arbitrary
    dom <- BS.filter (/= '@') <$> arbitrary
    pure $ unsafeEmailAddress loc dom

instance C.Cql EmailAddress where
  ctype = C.Tagged C.TextColumn

  fromCql (C.CqlText t) = case parseEmail t of
    Just e -> pure e
    Nothing -> Left "fromCql: Invalid email"
  fromCql _ = Left "fromCql: email: CqlText expected"

  toCql = C.toCql . fromEmail

fromEmail :: EmailAddress -> Text
fromEmail mail = decodeUtf8 $ (localPart mail) <> "@" <> (domainPart mail)

-- | Parses an email address of the form <local-part>@<domain>.
parseEmail :: Text -> Maybe EmailAddress
parseEmail = read . Text.unpack
