module Web.Scim.Schema.Meta where

import Data.Aeson
import qualified Data.HashMap.Lazy as HML
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.Time.Clock
import GHC.Generics (Generic)
import Text.Read (readEither)
import Web.Scim.Schema.Common
import Web.Scim.Schema.ResourceType
import Prelude hiding (map)

data ETag = Weak Text | Strong Text
  deriving (Eq, Show)

instance ToJSON ETag where
  toJSON (Weak tag) = String $ "W/" <> pack (show tag)
  toJSON (Strong tag) = String $ pack (show tag)

instance FromJSON ETag where
  parseJSON = withText "ETag" $ \s ->
    case Text.stripPrefix "W/" s of
      Nothing -> Strong <$> unquote s
      Just s' -> Weak <$> unquote s'
    where
      unquote s = case readEither (unpack s) of
        Right x -> pure x
        Left e -> fail ("couldn't unquote the string: " <> e)

data Meta
  = Meta
      { resourceType :: ResourceType,
        created :: UTCTime,
        lastModified :: UTCTime,
        -- | Resource version: <https://tools.ietf.org/html/rfc7644#section-3.14>.
        --
        -- A version is an /opaque/ string that doesn't need to conform to any
        -- format (e.g. it does not have to be a monotonically increasing integer,
        -- contrary to what the word @version@ suggests).
        --
        -- For 'Weak' versions we have to guarantee that different resources will
        -- have different 'version's. For 'Strong' versions we also have to
        -- guarantee that same resources will have the same 'version'.
        version :: ETag,
        location :: URI
      }
  deriving (Eq, Show, Generic)

instance ToJSON Meta where
  toJSON = genericToJSON serializeOptions

instance FromJSON Meta where
  parseJSON = genericParseJSON parseOptions . jsonLower

data WithMeta a
  = WithMeta
      { meta :: Meta,
        thing :: a
      }
  deriving (Eq, Show, Generic)

instance (ToJSON a) => ToJSON (WithMeta a) where
  toJSON (WithMeta m v) = case toJSON v of
    (Object o) -> Object (HML.insert "meta" (toJSON m) o)
    other -> other

instance (FromJSON a) => FromJSON (WithMeta a) where
  parseJSON = withObject "WithMeta" $ \o ->
    WithMeta <$> o .: "meta" <*> parseJSON (Object o)
