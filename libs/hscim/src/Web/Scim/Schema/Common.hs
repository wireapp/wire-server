{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Web.Scim.Schema.Common where

import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.CaseInsensitive as CI
import Data.String.Conversions (cs)
import Data.Text (pack, unpack)
import qualified Network.URI as Network

data WithId id a = WithId
  { id :: id,
    value :: a
  }
  deriving (Eq, Show)

instance (ToJSON id, ToJSON a) => ToJSON (WithId id a) where
  toJSON (WithId i v) = case toJSON v of
    (Object o) -> Object (KeyMap.insert "id" (toJSON i) o)
    other -> other

instance (FromJSON id, FromJSON a) => FromJSON (WithId id a) where
  parseJSON = withObject "WithId" $ \o ->
    WithId <$> o .: "id" <*> parseJSON (Object o)

newtype URI = URI {unURI :: Network.URI}
  deriving (Show, Eq)

instance FromJSON URI where
  parseJSON = withText "URI" $ \uri -> case Network.parseURI (unpack uri) of
    Nothing -> fail "Invalid URI"
    Just some -> pure $ URI some

instance ToJSON URI where
  toJSON (URI uri) = String $ pack $ show uri

newtype ScimBool = ScimBool {unScimBool :: Bool}
  deriving stock (Eq, Show, Ord)
  deriving newtype (ToJSON)

instance FromJSON ScimBool where
  parseJSON (Bool bl) = pure (ScimBool bl)
  parseJSON (String str) =
    case CI.mk str of
      "true" -> pure (ScimBool True)
      "false" -> pure (ScimBool False)
      _ -> fail $ "Expected true, false, \"true\", or \"false\" (case insensitive), but got " <> cs str
  parseJSON bad = fail $ "Expected true, false, \"true\", or \"false\" (case insensitive), but got " <> show bad

toKeyword :: String -> String
toKeyword "typ" = "type"
toKeyword "ref" = "$ref"
toKeyword other = other

serializeOptions :: Options
serializeOptions =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = toKeyword
    }

parseOptions :: Options
parseOptions =
  defaultOptions
    { fieldLabelModifier = toKeyword . CI.foldCase
    }

-- | Turn all keys in a JSON object to lowercase recursively.  This is applied to the aeson
-- 'Value' to be parsed; 'parseOptions' is applied to the keys passed to '(.:)' etc.
--
-- NB: be careful to not mix 'Data.Text.{toLower,toCaseFold', 'Data.Char.toLower', and
-- 'Data.CaseInsensitive.foldCase'.  They're not all the same thing!
-- https://github.com/basvandijk/case-insensitive/issues/31
--
-- (FUTUREWORK: The "recursively" part is a bit of a waste and could be dropped, but we would
-- have to spend more effort in making sure it is always called manually in nested parsers.)
jsonLower :: Value -> Value
jsonLower (Object o) = Object . KeyMap.fromList . fmap lowerPair . KeyMap.toList $ o
  where
    lowerPair (key, val) = (lowerKey key, jsonLower val)
jsonLower (Array x) = Array (jsonLower <$> x)
jsonLower same@(String _) = same -- (only object attributes, not all texts in the value side of objects!)
jsonLower same@(Number _) = same
jsonLower same@(Bool _) = same
jsonLower same@Null = same

lowerKey :: Key.Key -> Key.Key
lowerKey = Key.fromText . CI.foldCase . Key.toText
