{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Scim attribute names. these are case-insensitive
module Web.Scim.AttrName where

import Prelude hiding (takeWhile)
import Data.Text (Text, toCaseFold, cons)
import Data.Text.Encoding (decodeUtf8)
import Data.String (IsString, fromString)
import Data.Aeson.Types (ToJSONKey, FromJSONKey)
import Data.Attoparsec.ByteString.Char8
import Data.Hashable

-- | An attribute (e.g. username).
--
-- ATTRNAME  = ALPHA *(nameChar)
-- NOTE: We use the FromJSONKey instance of Text. The default instances parses
-- a list of key values instead of a map
newtype AttrName
  = AttrName Text deriving (Show, FromJSONKey, ToJSONKey)

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
