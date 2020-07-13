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

-- | Scim attribute names. these are case-insensitive
module Web.Scim.AttrName where

import Data.Aeson.Types (FromJSONKey, ToJSONKey)
import Data.Attoparsec.ByteString.Char8
import Data.Hashable
import Data.String (IsString, fromString)
import Data.Text (Text, cons, toCaseFold)
import Data.Text.Encoding (decodeUtf8)
import Prelude hiding (takeWhile)

-- | An attribute (e.g. username).
--
-- ATTRNAME  = ALPHA *(nameChar)
-- NOTE: We use the FromJSONKey instance of Text. The default instances parses
-- a list of key values instead of a map
newtype AttrName
  = AttrName Text
  deriving (Show, FromJSONKey, ToJSONKey)

instance Eq AttrName where
  AttrName a == AttrName b = toCaseFold a == toCaseFold b

instance Ord AttrName where
  compare (AttrName a) (AttrName b) = compare (toCaseFold a) (toCaseFold b)

instance Hashable AttrName where
  hashWithSalt x (AttrName a) = hashWithSalt x (toCaseFold a)

instance IsString AttrName where
  fromString = AttrName . fromString

-- | Attribute name parser.
pAttrName :: Parser AttrName
pAttrName =
  (\c str -> AttrName (cons c (decodeUtf8 str)))
    <$> letter_ascii
    <*> takeWhile (\x -> isDigit x || isAlpha_ascii x || x == '-' || x == '_')

-- | Attribute name renderer.
rAttrName :: AttrName -> Text
rAttrName (AttrName x) = x
