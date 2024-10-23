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

module Web.Scim.Schema.Meta where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
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
  -- (if a strong tag contains a "W/" prefix by accident, it will be parsed as weak tag.  this
  -- is mildly confusing, but should do no harm.)
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

data Meta = Meta
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
  parseJSON = either (fail . show) (genericParseJSON parseOptions) . jsonLower

data WithMeta a = WithMeta
  { meta :: Meta,
    thing :: a
  }
  deriving (Eq, Show, Generic)

instance (ToJSON a) => ToJSON (WithMeta a) where
  toJSON (WithMeta m v) = case toJSON v of
    (Object o) -> Object (KeyMap.insert "meta" (toJSON m) o)
    other -> other

instance (FromJSON a) => FromJSON (WithMeta a) where
  parseJSON = withObject "WithMeta" $ \o ->
    WithMeta <$> o .: "meta" <*> parseJSON (Object o)
