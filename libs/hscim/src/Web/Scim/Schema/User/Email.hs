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
    primary :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON Email where
  parseJSON = genericParseJSON parseOptions . jsonLower

instance ToJSON Email where
  toJSON = genericToJSON serializeOptions
