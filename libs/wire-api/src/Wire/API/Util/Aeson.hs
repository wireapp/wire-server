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

module Wire.API.Util.Aeson
  ( customEncodingOptions,
    customEncodingOptionsDropChar,
    defaultOptsDropChar,
    CustomEncoded (..),
    CustomEncodedLensable (..),
  )
where

import Data.Aeson
import Data.Json.Util (toJSONFieldName)
import GHC.Generics (Rep)
import Imports hiding (All)

-- | Drops record field name prefixes (anything until the first upper-case char)
-- and turns the rest into snake_case.
--
-- For example, it converts @_recordFieldLabel@ into @field_label@.
customEncodingOptions :: Options
customEncodingOptions = toJSONFieldName

-- This is useful for structures that are also creating lenses.
-- If the field name doesn't have a leading underscore then the
-- default `makeLenses` call won't make any lenses.
customEncodingOptionsDropChar :: Char -> Options
customEncodingOptionsDropChar c =
  toJSONFieldName
    { fieldLabelModifier = fieldLabelModifier toJSONFieldName . dropWhile (c ==)
    }

-- Similar to customEncodingOptionsDropChar, but not doing snake_case
defaultOptsDropChar :: Char -> Options
defaultOptsDropChar c =
  defaultOptions
    { fieldLabelModifier = fieldLabelModifier defaultOptions . dropWhile (c ==)
    }

newtype CustomEncoded a = CustomEncoded {unCustomEncoded :: a}

instance (Generic a, GToJSON Zero (Rep a)) => ToJSON (CustomEncoded a) where
  toJSON = genericToJSON @a customEncodingOptions . unCustomEncoded

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (CustomEncoded a) where
  parseJSON = fmap CustomEncoded . genericParseJSON @a customEncodingOptions

-- Similar to CustomEncoded except that it will first strip off leading '_' characters.
-- This is important for records with field names that would otherwise be keywords, like type or data
-- It is also useful if the record has lenses being generated.
newtype CustomEncodedLensable a = CustomEncodedLensable {unCustomEncodedLensable :: a}

instance (Generic a, GToJSON Zero (Rep a)) => ToJSON (CustomEncodedLensable a) where
  toJSON = genericToJSON @a (customEncodingOptionsDropChar '_') . unCustomEncodedLensable

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (CustomEncodedLensable a) where
  parseJSON = fmap CustomEncodedLensable . genericParseJSON @a (customEncodingOptionsDropChar '_')
