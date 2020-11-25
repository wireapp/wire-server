module Deriving.Swagger where

import Data.Kind (Constraint)
import Data.Proxy (Proxy (..))
import Data.Swagger (SchemaOptions, ToSchema (..), constructorTagModifier, defaultSchemaOptions, fieldLabelModifier, genericDeclareNamedSchema)
import Data.Swagger.Internal.Schema (GToSchema)
import Data.Swagger.Internal.TypeShape (TypeHasSimpleShape)
import GHC.Generics (Generic (Rep))
import GHC.TypeLits (ErrorMessage (Text), KnownSymbol, Symbol, TypeError, symbolVal)
import Imports

newtype CustomSwagger t a = CustomSwagger {unCustomSwagger :: a}

data StripPrefix t

data ConstructorTagModifier t

data FieldLabelModifier t

data CamelTo (separator :: Symbol)

type CamelToSnake = CamelTo "_"

type CamelToKebab = CamelTo "-"

data LabelMapping a b = a :-> b

data LabelMappings (lmap :: [LabelMapping Symbol Symbol])

class StringModifier t where
  getStringModifier :: String -> String

instance KnownSymbol prefix => StringModifier (StripPrefix prefix) where
  getStringModifier = fromMaybe <*> stripPrefix (symbolVal (Proxy @prefix))

instance (StringModifier a, StringModifier b) => StringModifier (a, b) where
  getStringModifier = getStringModifier @b . getStringModifier @a

instance (StringModifier a, StringModifier b, StringModifier c) => StringModifier (a, b, c) where
  getStringModifier = getStringModifier @c . getStringModifier @b . getStringModifier @a

instance (StringModifier a, StringModifier b, StringModifier c, StringModifier d) => StringModifier (a, b, c, d) where
  getStringModifier = getStringModifier @d . getStringModifier @c . getStringModifier @b . getStringModifier @a

instance (KnownSymbol separator, NonEmptyString separator) => StringModifier (CamelTo separator) where
  getStringModifier =
    case symbolVal (Proxy @separator) of
      (char : _) -> camelTo2 char
      _ -> error "Impossible case for 'NonEmptyString'"

instance StringModifier (LabelMappings '[]) where
  getStringModifier = id

instance (KnownSymbol orig, KnownSymbol new, StringModifier (LabelMappings xs)) => StringModifier (LabelMappings ((orig ':-> new) ': xs)) where
  getStringModifier input =
    if input == symbolVal (Proxy @orig)
      then symbolVal (Proxy @new)
      else getStringModifier @(LabelMappings xs) input

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

class SwaggerOptions xs where
  swaggerOptions :: SchemaOptions

instance SwaggerOptions '[] where
  swaggerOptions = defaultSchemaOptions

instance (StringModifier f, SwaggerOptions xs) => SwaggerOptions (FieldLabelModifier f ': xs) where
  swaggerOptions = (swaggerOptions @xs) {fieldLabelModifier = getStringModifier @f}

instance (StringModifier f, SwaggerOptions xs) => SwaggerOptions (ConstructorTagModifier f ': xs) where
  swaggerOptions = (swaggerOptions @xs) {constructorTagModifier = getStringModifier @f}

instance
  ( SwaggerOptions t,
    Generic a,
    GToSchema (Rep a),
    TypeHasSimpleShape a "genericDeclareNamedSchemaUnrestricted"
  ) =>
  ToSchema (CustomSwagger t a)
  where
  declareNamedSchema _ = genericDeclareNamedSchema (swaggerOptions @t) (Proxy @a)
