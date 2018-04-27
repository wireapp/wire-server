
module Web.SCIM.Schema.Common where

import Data.Text hiding (dropWhile)
import Data.Aeson
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HM
import Data.String (IsString)
import qualified Network.URI as Network



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

data Unsettable a = Unset | Omitted | Some a
  deriving (Show, Eq)

instance Functor Unsettable where
  fmap f (Some a) = Some $ f a
  fmap _ Unset = Unset
  fmap _ Omitted = Omitted 

instance (FromJSON a) => FromJSON (Unsettable a) where
  parseJSON Null = pure Unset
  parseJSON v = do
    res <- parseJSON v
    case res of
      Nothing -> pure Omitted
      Just some -> pure $ Some some

toMaybe :: Unsettable a -> Maybe a
toMaybe Unset = Nothing
toMaybe Omitted = Nothing
toMaybe (Some a) = Just a

instance (ToJSON a) => ToJSON (Unsettable a) where
  toJSON Unset = Null
  toJSON v     = toJSON . toMaybe $ v
