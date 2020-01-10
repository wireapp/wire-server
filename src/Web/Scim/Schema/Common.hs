
module Web.Scim.Schema.Common where

import Data.Text hiding (dropWhile)
import Data.Aeson
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HM
import Data.String (IsString)
import qualified Network.URI as Network
import qualified Data.HashMap.Lazy as HML


data WithId id a = WithId
  { id :: id
  , value :: a
  } deriving (Eq, Show)

instance (ToJSON id, ToJSON a) => ToJSON (WithId id a) where
  toJSON (WithId i v) = case toJSON v of
    (Object o) -> Object (HML.insert "id" (toJSON i) o)
    other      -> other

instance (FromJSON id, FromJSON a) => FromJSON (WithId id a) where
  parseJSON = withObject "WithId" $ \o ->
    WithId <$> o .: "id" <*> parseJSON (Object o)

newtype URI = URI { unURI :: Network.URI }
  deriving (Show, Eq)

instance FromJSON URI where
  parseJSON = withText "URI" $ \uri -> case Network.parseURI (unpack uri) of
    Nothing -> fail "Invalid URI"
    Just some -> pure $ URI some

instance ToJSON URI where
  toJSON (URI uri) = String $ pack $ show uri

toKeyword :: (IsString p, Eq p) => p -> p
toKeyword "typ" = "type"
toKeyword "ref" = "$ref"
toKeyword other = other

serializeOptions :: Options
serializeOptions = defaultOptions
  { omitNothingFields = True
  , fieldLabelModifier = toKeyword
  }

-- | Turn all keys in a JSON object to lowercase.
jsonLower :: Value -> Value
jsonLower (Object o) = Object . HM.fromList . fmap lowerPair . HM.toList $ o
  where lowerPair (key, val) = (fromKeyword . toLower $ key, val)
jsonLower x = x

fromKeyword :: (IsString p, Eq p) => p -> p
fromKeyword "type" = "typ"
fromKeyword "$ref" = "ref"
fromKeyword other = other

parseOptions :: Options
parseOptions = defaultOptions
  { fieldLabelModifier = fmap Char.toLower
  }

