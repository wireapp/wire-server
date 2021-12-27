{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Properties
  ( PropertyKeysAndValues (..),
    PropertyKey (..),
    PropertyValue (..),

    -- * Swagger
    modelPropertyValue,
    modelPropertyDictionary,
  )
where

import Data.Aeson (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Value)
import qualified Data.Aeson as A
import Data.ByteString.Conversion
import Data.Hashable (Hashable)
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import Data.Text.Ascii
import Imports
import Wire.API.Arbitrary (Arbitrary)
import Data.Schema

newtype PropertyKeysAndValues = PropertyKeysAndValues { pkavKeysAndValues :: [(PropertyKey, PropertyValue)] }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Hashable)
  deriving S.ToSchema via Schema PropertyKeysAndValues

instance ToSchema PropertyKeysAndValues where
  -- TODO(sandy): HELP
  schema = object "PropertyKeysAndValues" $ undefined
    -- PropertyKeysAndValues
    --   <$> pkavKeysAndValues .= schema

modelPropertyDictionary :: Doc.Model
modelPropertyDictionary =
  Doc.defineModel "PropertyDictionary" $
    Doc.description "A JSON object with properties as attribute/value pairs."

instance ToJSON PropertyKeysAndValues where
  toJSON (PropertyKeysAndValues kvs) = A.object [toText k A..= v | (PropertyKey k, v) <- kvs]


newtype PropertyKey = PropertyKey
  {propertyKeyName :: AsciiPrintable}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromByteString, ToByteString, FromJSONKey, ToJSONKey, Hashable, Arbitrary)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema PropertyKey

instance ToSchema PropertyKey where
  schema = PropertyKey <$> propertyKeyName .= schema

instance S.ToParamSchema PropertyKey where
  -- TODO(sandy):
  toParamSchema _ = undefined

newtype PropertyValue = PropertyValue
  {propertyValueJson :: Value}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Hashable, Arbitrary)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema PropertyValue)

instance ToSchema PropertyValue where
  schema =
    PropertyValue
      <$> propertyValueJson .= named "PropertyValue" mempty

modelPropertyValue :: Doc.Model
modelPropertyValue =
  Doc.defineModel "PropertyValue" $
    Doc.description "A property value is any valid JSON value."
