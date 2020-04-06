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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Brig.Types.Properties where

import Data.Aeson
import Data.ByteString.Conversion
import Data.Hashable (Hashable)
import Data.Text.Ascii
import Imports

newtype PropertyKey
  = PropertyKey
      {propertyKeyName :: AsciiPrintable}
  deriving
    ( Eq,
      Ord,
      Show,
      FromByteString,
      ToByteString,
      FromJSON,
      ToJSON,
      FromJSONKey,
      ToJSONKey,
      Generic,
      Hashable
    )

newtype PropertyValue
  = PropertyValue
      {propertyValueJson :: Value}
  deriving (Eq, Show, FromJSON, ToJSON, Generic, Hashable)

newtype PropertyKeysAndValues = PropertyKeysAndValues [(PropertyKey, PropertyValue)]
  deriving (Eq, Show, Generic, Hashable)

instance ToJSON PropertyKeysAndValues where
  toJSON (PropertyKeysAndValues kvs) = object [toText k .= v | (PropertyKey k, v) <- kvs]
