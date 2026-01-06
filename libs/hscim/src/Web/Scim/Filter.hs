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

-- | A query might specify a filter that should be applied to the results
-- before returning them. This module implements a very limited subset of
-- the specification: <https://tools.ietf.org/html/rfc7644#section-3.4.2.2>.
--
-- Supported:
--
-- * All comparison operators (@eq@, @le@, etc)
-- * The @userName@ attribute
--
-- Not supported:
--
-- * The @pr@ operator
-- * Boolean operators
-- * Combined filters
-- * Fully qualified attribute names (schema prefixes, attribute paths)
module Web.Scim.Filter
  ( -- * Filter type
    Filter (..),
    parseFilter,
    renderFilter,

    -- * Constructing filters
    CompValue (..),
    CompareOp (..),
    AttrPath (..),
    ValuePath (..),
    SubAttr (..),
    pAttrPath,
    pValuePath,
    pSubAttr,
    pFilter,
    rAttrPath,
    rCompareOp,
    rValuePath,
    rSubAttr,
    compareStr,
    topLevelAttrPath,
  )
where

import Control.Applicative (optional)
import Data.Aeson as Aeson
import Data.Aeson.Parser as Aeson
import Data.Aeson.Text as Aeson
import Data.Attoparsec.ByteString.Char8
import Data.Scientific
import Data.String
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as LT
import Imports
import Lens.Micro
import Web.HttpApiData
import Web.Scim.AttrName
import Web.Scim.Schema.Schema (Schema (User20), getSchemaUri, pSchema)
import Prelude hiding (takeWhile)

----------------------------------------------------------------------------
-- Types

-- NB: when extending these types, don't forget to update Test.FilterSpec

-- | A value type. Attributes are compared against literal values.
data CompValue
  = ValNull
  | ValBool Bool
  | ValNumber Scientific
  | ValString Text
  deriving (Eq, Ord, Show)

-- | A comparison operator.
data CompareOp
  = -- | Equal
    OpEq
  | -- | Not equal
    OpNe
  | -- | Contains
    OpCo
  | -- | Starts with
    OpSw
  | -- | Ends with
    OpEw
  | -- | Greater than
    OpGt
  | -- | Greater than or equal to
    OpGe
  | -- | Less than
    OpLt
  | -- | Less than or equal to
    OpLe
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A filter.
--
-- Our representation of filters is lax and doesn't attempt to ensure
-- validity on the type level. If a filter does something silly (e.g. tries
-- to compare a username with a boolean), it will be caught during filtering
-- and an appropriate error message will be thrown (see 'filterUser').
--
-- TODO(arianvp): Implement the following grammar fully if we want to support
-- more complex filters
--
-- FILTER    = attrExp / logExp / valuePath / *1"not" "(" FILTER ")"
-- PATH = attrPath / valuePath [subAttr]
--
-- FUTUREWORK(fisx): Currently we don't support matching on lists in paths
-- as we currently don't support filtering on arbitrary attributes yet
-- e.g.
-- @
-- "path":"members[value eq
--            \"2819c223-7f76-453a-919d-413861904646\"].displayName"
-- @
-- is not supported. The code here should actually read something like this:
-- @
-- data Filter = FilterAttrCompare (Either AttrPath ValuePath) CompareOp CompValue
-- @
--
-- FUTUREWORK(fisx): does it make sense to have a type-level argument to
-- AttrPath, ValuePath(?), Filter containing the allowed schemas?
-- it's certainly information that should be known at compile time...
--
-- https://datatracker.ietf.org/doc/html/rfc7644#section-3.4.2.2
data Filter
  = -- | Compare the attribute value with a literal
    FilterAttrCompare AttrPath CompareOp CompValue
  deriving (Eq, Show)

-- | valuePath = attrPath "[" valFilter "]"
--
-- A `ValuePath` without a `Filter` is morally an `AttrPath`.
--
-- Cases covered:
-- - '.roles'
-- - '.bla.foo'
-- - '.email["type" eq "work"]'
--
-- TODO(arianvp): This is a slight simplification at the moment as we
-- don't support the complete Filter grammar. This should be a
-- valFilter, not a FILTER.
-- https://datatracker.ietf.org/doc/html/rfc7644#section-3.4.2.2
data ValuePath = ValuePath AttrPath (Maybe Filter)
  deriving (Eq, Show)

-- | subAttr   = "." ATTRNAME
newtype SubAttr = SubAttr AttrName
  deriving (Eq, Show, IsString)

-- | attrPath  = [URI ":"] ATTRNAME *1subAtt
data AttrPath = AttrPath (Maybe Schema) AttrName (Maybe SubAttr)
  deriving (Eq, Show)

-- | Smart constructor that refers to a toplevel field with default schema
topLevelAttrPath :: Text -> AttrPath
topLevelAttrPath x = AttrPath Nothing (AttrName x) Nothing

----------------------------------------------------------------------------
-- Parsing

-- | Parse a filter. Spaces surrounding the filter will be stripped.
--
-- If parsing fails, returns a 'Left' with an error description.
--
-- Note: this parser is written with Attoparsec because I don't know how to
-- lift an Attoparsec parser (from Aeson) to Megaparsec
parseFilter :: [Schema] -> Text -> Either Text Filter
parseFilter supportedSchemas =
  over _Left T.pack
    . parseOnly (skipSpace *> pFilter supportedSchemas <* skipSpace <* endOfInput)
    . encodeUtf8

-- |
-- @
-- ATTRNAME  = ALPHA *(nameChar)
-- attrPath  = [URI ":"] ATTRNAME *1subAtt
-- @
pAttrPath :: [Schema] -> Parser AttrPath
pAttrPath supportedSchemas = do
  schema <- optional (pSchema supportedSchemas <* char ':')
  AttrPath schema <$> pAttrName <*> optional pSubAttr

-- | subAttr   = "." ATTRNAME
pSubAttr :: Parser SubAttr
pSubAttr = char '.' *> (SubAttr <$> pAttrName)

-- | valuePath = attrPath "[" valFilter "]"
pValuePath :: [Schema] -> Parser ValuePath
pValuePath supportedSchemas =
  ValuePath <$> pAttrPath supportedSchemas <*> (Just <$> (char '[' *> pFilter supportedSchemas <* char ']'))

-- | Value literal parser.
pCompValue :: Parser CompValue
pCompValue =
  choice
    [ ValNull <$ string "null",
      ValBool True <$ stringCI "true",
      ValBool False <$ stringCI "false",
      ValNumber <$> Aeson.scientific,
      ValString <$> Aeson.jstring
    ]

-- | Comparison operator parser.
pCompareOp :: Parser CompareOp
pCompareOp =
  choice
    [ OpEq <$ stringCI "eq",
      OpNe <$ stringCI "ne",
      OpCo <$ stringCI "co",
      OpSw <$ stringCI "sw",
      OpEw <$ stringCI "ew",
      OpGt <$ stringCI "gt",
      OpGe <$ stringCI "ge",
      OpLt <$ stringCI "lt",
      OpLe <$ stringCI "le"
    ]

-- | Filter parser.
pFilter :: [Schema] -> Parser Filter
pFilter supportedSchemas =
  FilterAttrCompare
    <$> pAttrPath supportedSchemas
    <*> (skipSpace1 *> pCompareOp)
    <*> (skipSpace1 *> pCompValue)

-- | Utility parser for skipping one or more spaces.
skipSpace1 :: Parser ()
skipSpace1 = space *> skipSpace

----------------------------------------------------------------------------
-- Rendering

-- | Render a filter according to the SCIM spec.
renderFilter :: Filter -> Text
renderFilter filter_ = case filter_ of
  FilterAttrCompare attr op val ->
    rAttrPath attr <> " " <> rCompareOp op <> " " <> rCompValue val

rAttrPath :: AttrPath -> Text
rAttrPath (AttrPath schema attr subAttr) =
  maybe "" ((<> ":") . getSchemaUri) schema
    <> rAttrName attr
    <> maybe "" rSubAttr subAttr

rSubAttr :: SubAttr -> Text
rSubAttr (SubAttr x) = "." <> rAttrName x

rValuePath :: ValuePath -> Text
rValuePath (ValuePath attrPath Nothing) = rAttrPath attrPath
rValuePath (ValuePath attrPath (Just filter')) = rAttrPath attrPath <> "[" <> renderFilter filter' <> "]"

-- | Value literal renderer.
rCompValue :: CompValue -> Text
rCompValue = \case
  ValNull -> "null"
  ValBool True -> "true"
  ValBool False -> "false"
  ValNumber n -> LT.toStrict $ Aeson.encodeToLazyText (Aeson.Number n)
  ValString s -> LT.toStrict $ Aeson.encodeToLazyText (Aeson.String s)

-- | Comparison operator renderer.
rCompareOp :: CompareOp -> Text
rCompareOp = \case
  OpEq -> "eq"
  OpNe -> "ne"
  OpCo -> "co"
  OpSw -> "sw"
  OpEw -> "ew"
  OpGt -> "gt"
  OpGe -> "ge"
  OpLt -> "lt"
  OpLe -> "le"

-- | Execute a comparison operator.
compareStr :: CompareOp -> Text -> Text -> Bool
compareStr = \case
  OpEq -> (==) -- equal
  OpNe -> (/=) -- not equal
  OpCo -> flip T.isInfixOf -- A contains B
  OpSw -> flip T.isPrefixOf -- A starts with B
  OpEw -> flip T.isSuffixOf -- A ends with B
  OpGt -> (>) -- greater than
  OpGe -> (>=) -- greater than or equal to
  OpLt -> (<) -- less than
  OpLe -> (<=) -- less than or equal to

----------------------------------------------------------------------------
-- Instances

-- | We currently only support filtering on core user schema
instance FromHttpApiData Filter where
  parseUrlPiece = parseFilter [User20]

instance ToHttpApiData Filter where
  toUrlPiece = renderFilter

instance ToJSON AttrPath where
  toJSON = toJSON . rAttrPath

instance FromJSON AttrPath where
  parseJSON val = parseJSON @Text val >>= either fail pure . parseOnly (pAttrPath []) . encodeUtf8

instance ToJSON ValuePath where
  toJSON = todo

instance FromJSON ValuePath where
  parseJSON = todo
