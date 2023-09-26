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

module Wire.API.Properties
  ( PropertyKeysAndValues (..),
    PropertyKey (..),
    RawPropertyValue (..),
    PropertyValue (..),
  )
where

import Control.Lens ((?~))
import Data.Aeson (FromJSON (..), ToJSON (..), Value)
import Data.Aeson qualified as A
import Data.ByteString.Conversion
import Data.Hashable (Hashable)
import Data.OpenApi qualified as S
import Data.Text.Ascii
import Imports
import Servant
import Wire.Arbitrary (Arbitrary)

newtype PropertyKeysAndValues = PropertyKeysAndValues (Map PropertyKey PropertyValue)
  deriving newtype (ToJSON)

instance S.ToSchema PropertyKeysAndValues where
  declareNamedSchema _ =
    pure $
      S.NamedSchema (Just "PropertyKeysAndValues") $
        mempty & S.type_ ?~ S.OpenApiObject

newtype PropertyKey = PropertyKey
  {propertyKeyName :: AsciiPrintable}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype
    ( FromByteString,
      ToByteString,
      FromJSON,
      ToJSON,
      S.ToSchema,
      A.FromJSONKey,
      A.ToJSONKey,
      FromHttpApiData,
      Hashable,
      Arbitrary
    )

instance S.ToParamSchema PropertyKey where
  toParamSchema _ =
    mempty
      & S.type_ ?~ S.OpenApiString
      & S.format ?~ "printable"

-- | A raw, unparsed property value.
newtype RawPropertyValue = RawPropertyValue {rawPropertyBytes :: LByteString}

instance {-# OVERLAPPING #-} MimeUnrender JSON RawPropertyValue where
  mimeUnrender _ = pure . RawPropertyValue

instance {-# OVERLAPPING #-} MimeRender JSON RawPropertyValue where
  mimeRender _ = rawPropertyBytes

instance S.ToSchema RawPropertyValue where
  declareNamedSchema _ =
    pure . S.NamedSchema (Just "PropertyValue") $
      mempty & S.description ?~ "An arbitrary JSON value for a property"

-- | A property value together with its original serialisation.
data PropertyValue = PropertyValue
  { propertyRaw :: RawPropertyValue,
    propertyValue :: Value
  }

instance ToJSON PropertyValue where
  toJSON = propertyValue

instance Show PropertyValue where
  show = show . propertyValue
