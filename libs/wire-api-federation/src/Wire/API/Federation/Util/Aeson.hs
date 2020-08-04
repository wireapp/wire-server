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

module Wire.API.Federation.Util.Aeson
  ( customEncodingOptions,
    CustomEncoded (..),
  )
where

import Data.Aeson
import qualified Data.Char as Char
import GHC.Generics (Rep)
import Imports

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
