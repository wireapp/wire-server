{-# LANGUAGE RankNTypes #-}

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

-- | Type-directed ToSchema instances. Heavily inspired by
-- [deriving-aeson](https://hackage.haskell.org/package/deriving-aeson).
module Deriving.Swagger where

import Data.Char qualified as Char
import Data.Kind (Constraint)
import Data.List.Extra (stripSuffix)
import Data.OpenApi.Internal.Schema (GToSchema)
import Data.OpenApi.Internal.TypeShape
import Data.OpenApi.Schema
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic (Rep))
import GHC.TypeLits (ErrorMessage (Text), KnownSymbol, Symbol, TypeError, symbolVal)
import Imports

-- * How to use this library

-- $usage
-- This library provides types which can be used with @deriving via@ like this:
--
-- @
-- data Person = Person { personName :: Text
--                      , personAge :: Int
--                      , personBirthCity :: Text
--                      }
--  deriving (Generic)
--  deriving (ToSchema) via (CustomSwagger '[FieldLabelModifier (StripPrefix "person", CamelToSnake)] Person)
-- @
--
-- This will produce swagger schema with fields @name@, @age@ and @birth_city@.
--
-- Similarly constructor tags of sum types can also be modified like this:
--
-- @
-- data AssetSize = AssetComplete | AssetPreview
--   deriving (Generic)
--   deriving (ToSchema) via (CustomSwagger '[ConstructorTagModifier (StripPrefix \"Asset\", CamelToSnake)] AssetSize)
-- @
--
-- This will produce a swagger schema with enums limited to @complete@ and
-- @preview@
--
-- Sometimes life isn't as easy and some exceptions need to be specified to
-- neatly laid out rules. For example, if in the first example we wanted to map
-- @personBirthCity@ to @city_of_birth@, we can specify this exception using
-- @'LabelMappings'@ like this:
--
-- @
--   deriving via (ToSchema)
--   via (CustomSwagger '[ FieldLabelModifier ( StripPrefix "person"
--                                            , CamelToSnake
--                                            , LabelMappings '["birth_city" ':-> "city_of_birth"]
--                                            )
--                                            Person
--                       ]
--       )
-- @
--
-- Note that here we map the camel-cased version @birth_city@ to @city_of_birth@
-- because @'CamelToSnake'@ is used before @'LabelMappings'@.

-- * Types

-- | A newtype wrapper which gives ToSchema instances with modified options.
-- 't' has to have an instance of the 'SwaggerOptions' class.
newtype CustomSwagger t a = CustomSwagger {unCustomSwagger :: a}
  deriving (Generic, Typeable)

class SwaggerOptions xs where
  swaggerOptions :: SchemaOptions

instance SwaggerOptions '[] where
  swaggerOptions = defaultSchemaOptions

instance (StringModifier f, SwaggerOptions xs) => SwaggerOptions (FieldLabelModifier f ': xs) where
  swaggerOptions = (swaggerOptions @xs) {fieldLabelModifier = getStringModifier @f}

instance (StringModifier f, SwaggerOptions xs) => SwaggerOptions (ConstructorTagModifier f ': xs) where
  swaggerOptions = (swaggerOptions @xs) {constructorTagModifier = getStringModifier @f}

instance (SwaggerOptions t, Generic a, Typeable a, GToSchema (Rep a), Typeable (CustomSwagger t a), TypeHasSimpleShape a "genericDeclareNamedSchemaUnrestricted") => ToSchema (CustomSwagger t a) where
  declareNamedSchema _ = genericDeclareNamedSchema (swaggerOptions @t) (Proxy @a)

-- ** Specify __what__ to modify

-- | Modifies field labels only. Most likely 't' should be anything that
-- implments 'StringModifier'
data FieldLabelModifier t

-- | Modifies constructor tags only. Most likely 't' should be anything that
-- implements 'StringModifier'.
data ConstructorTagModifier t

-- ** Specify __how__ to modify

-- | Typeclass to specify the modifications
class StringModifier t where
  getStringModifier :: String -> String

-- | Left-to-right (@'flip' '.'@) composition
instance (StringModifier a, StringModifier b) => StringModifier (a, b) where
  getStringModifier = getStringModifier @b . getStringModifier @a

-- | Left-to-right (@'flip' '.'@) composition
instance (StringModifier a, StringModifier b, StringModifier c) => StringModifier (a, b, c) where
  getStringModifier = getStringModifier @c . getStringModifier @b . getStringModifier @a

-- | Left-to-right (@'flip' '.'@) composition
instance (StringModifier a, StringModifier b, StringModifier c, StringModifier d) => StringModifier (a, b, c, d) where
  getStringModifier = getStringModifier @d . getStringModifier @c . getStringModifier @b . getStringModifier @a

-- | Strips the given prefix, has no effect if the prefix doesn't exist
data StripPrefix t

instance (KnownSymbol prefix) => StringModifier (StripPrefix prefix) where
  getStringModifier = fromMaybe <*> stripPrefix (symbolVal (Proxy @prefix))

-- | Strips the given suffix, has no effect if the suffix doesn't exist
data StripSuffix t

instance (KnownSymbol suffix) => StringModifier (StripSuffix suffix) where
  getStringModifier = fromMaybe <*> stripSuffix (symbolVal (Proxy @suffix))

data CamelTo (separator :: Symbol)

instance (KnownSymbol separator, NonEmptyString separator) => StringModifier (CamelTo separator) where
  getStringModifier =
    case symbolVal (Proxy @separator) of
      (char : _) -> camelTo2 char
      _ -> error "Impossible case for 'NonEmptyString'"

type family NonEmptyString (xs :: Symbol) :: Constraint where
  NonEmptyString "" = TypeError ('Text "Empty string separator provided for camelTo separator")
  NonEmptyString _ = ()

-- | Copied from Data.Aeson.Types.Internal
camelTo2 :: Char -> String -> String
camelTo2 c = map toLower . go2 . go1
  where
    go1 "" = ""
    go1 (x : u : l : xs) | isUpper u && isLower l = x : c : u : l : go1 xs
    go1 (x : xs) = x : go1 xs
    go2 "" = ""
    go2 (l : u : xs) | isLower l && isUpper u = l : c : u : go2 xs
    go2 (x : xs) = x : go2 xs

type CamelToSnake = CamelTo "_"

type CamelToKebab = CamelTo "-"

data LowerCase

instance StringModifier LowerCase where
  getStringModifier = map Char.toLower

data LabelMapping a b = a :-> b

data LabelMappings (lmap :: [LabelMapping Symbol Symbol])

instance StringModifier (LabelMappings '[]) where
  getStringModifier = id

instance (KnownSymbol orig, KnownSymbol new, StringModifier (LabelMappings xs)) => StringModifier (LabelMappings ((orig ':-> new) ': xs)) where
  getStringModifier input =
    if input == symbolVal (Proxy @orig)
      then symbolVal (Proxy @new)
      else getStringModifier @(LabelMappings xs) input
