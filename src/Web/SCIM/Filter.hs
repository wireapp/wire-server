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

module Web.SCIM.Filter
  ( Filter(..)
  , parseFilter
  , filterUser

  -- * Constructing filters
  , CompValue(..)
  , CompareOp(..)
  , Attribute(..)
  ) where

import Data.Scientific
import Data.Text
import Data.Text.Encoding
import Data.Attoparsec.ByteString.Char8
import Data.Aeson.Parser as Aeson
import Lens.Micro
import Web.HttpApiData

import Web.SCIM.Schema.User

----------------------------------------------------------------------------
-- Types

-- | A value type. Attributes are compared against literal values.
data CompValue
  = ValNull
  | ValBool Bool
  | ValNumber Scientific
  | ValString Text
  deriving (Eq, Ord, Show)

-- | A comparison operator.
data CompareOp
  = OpEq            -- ^ Equal
  | OpNe            -- ^ Not equal
  | OpCo            -- ^ Contains
  | OpSw            -- ^ Starts with
  | OpEw            -- ^ Ends with
  | OpGt            -- ^ Greater than
  | OpGe            -- ^ Greater than or equal to
  | OpLt            -- ^ Less than
  | OpLe            -- ^ Less than or equal to
  deriving (Eq, Ord, Show)

-- | An attribute (e.g. username).
--
-- Only usernames are supported as attributes. Paths are not supported.
data Attribute
  = AttrUserName
  deriving (Eq, Ord, Show)

-- | A filter.
--
-- Our representation of filters is lax and doesn't attempt to ensure
-- validity on the type level. If a filter does something silly (e.g. tries
-- to compare a username with a boolean), it will be caught during filtering
-- and an appropriate error message will be thrown (see 'filterUser').
data Filter
  -- | Compare the attribute value with a literal
  = FilterAttrCompare Attribute CompareOp CompValue

----------------------------------------------------------------------------
-- Parsing

-- Note: this parser is written with Attoparsec because I don't know how to
-- lift an Attoparsec parser (from Aeson) to Megaparsec

-- | Parse a filter. Spaces surrounding the filter will be stripped.
--
-- If parsing fails, returns a 'Left' with an error description.
parseFilter :: Text -> Either Text Filter
parseFilter =
  over _Left pack .
  parseOnly (skipSpace *> pFilter <* skipSpace <* endOfInput) .
  encodeUtf8

-- parser pieces

-- | Value literal parser.
pCompValue :: Parser CompValue
pCompValue = choice
  [ ValNull       <$  string "null"
  , ValBool True  <$  string "true"
  , ValBool False <$  string "false"
  , ValNumber     <$> Aeson.scientific
  , ValString     <$> Aeson.jstring
  ]

-- | Comparison operator parser.
pCompareOp :: Parser CompareOp
pCompareOp = choice
  [ OpEq <$ stringCI "eq"
  , OpNe <$ stringCI "ne"
  , OpCo <$ stringCI "co"
  , OpSw <$ stringCI "sw"
  , OpEw <$ stringCI "ew"
  , OpGt <$ stringCI "gt"
  , OpGe <$ stringCI "ge"
  , OpLt <$ stringCI "lt"
  , OpLe <$ stringCI "le"
  ]

-- | Attribute name parser.
pAttribute :: Parser Attribute
pAttribute = choice
  [ AttrUserName <$ stringCI "username"
  ]

-- | Filter parser.
pFilter :: Parser Filter
pFilter = choice
  [ FilterAttrCompare
      <$> pAttribute
      <*> (skipSpace1 *> pCompareOp)
      <*> (skipSpace1 *> pCompValue)
  ]

-- | Utility parser for skipping one or more spaces.
skipSpace1 :: Parser ()
skipSpace1 = space *> skipSpace

----------------------------------------------------------------------------
-- Applying

-- | Check whether a user satisfies the filter.
--
-- Returns 'Left' if the filter is constructed incorrectly (e.g. tries to
-- compare a username with a boolean).
filterUser :: Filter -> User -> Either Text Bool
filterUser filter_ user = case filter_ of
  FilterAttrCompare AttrUserName op (ValString str) ->
    -- Comparing usernames has to be case-insensitive; look at the
    -- 'caseExact' parameter of the user schema
    Right (compareStr op (toCaseFold (userName user)) (toCaseFold str))
  FilterAttrCompare AttrUserName _ _ ->
    Left "usernames can only be compared with strings"

-- | Execute a comparison operator.
compareStr :: CompareOp -> Text -> Text -> Bool
compareStr = \case
  OpEq -> (==)                -- equal
  OpNe -> (/=)                -- not equal
  OpCo -> flip isInfixOf      -- A contains B
  OpSw -> flip isPrefixOf     -- A starts with B
  OpEw -> flip isSuffixOf     -- A ends with B
  OpGt -> (>)                 -- greater than
  OpGe -> (>=)                -- greater than or equal to
  OpLt -> (<)                 -- less than
  OpLe -> (<=)                -- less than or equal to

----------------------------------------------------------------------------
-- Instances

instance FromHttpApiData Filter where
  parseUrlPiece = parseFilter
