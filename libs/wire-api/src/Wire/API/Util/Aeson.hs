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
    CustomEncoded (..),
  )
where

import Data.Aeson
import Data.Char qualified as Char
import GHC.Generics (Rep)
import Imports hiding (All)

-- | Drops record field name prefixes (anything until the first upper-case char)
-- and turns the rest into snake_case.
--
-- For example, it converts @_recordFieldLabel@ into @field_label@.
customEncodingOptions :: Options
customEncodingOptions =
  defaultOptions
    { fieldLabelModifier = camelTo2 '_' . dropWhile (not . Char.isUpper)
    }

newtype CustomEncoded a = CustomEncoded {unCustomEncoded :: a}

instance (Generic a, GToJSON Zero (Rep a)) => ToJSON (CustomEncoded a) where
  toJSON = genericToJSON @a customEncodingOptions . unCustomEncoded

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (CustomEncoded a) where
  parseJSON = fmap CustomEncoded . genericParseJSON @a customEncodingOptions
