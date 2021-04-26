{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Swagger.Typed
  ( Schema,
    NamedSchema,
    ToTypedSchema (..),
    TypedSchema (TypedSchema),
    field,
    field',
    named,
    unnamed,
    array,
    enum,
    handle,
    coerce,
    schema,
    untypedSchema,
  )
where

import Control.Applicative
import Control.Lens (at, view, (.~), (?~))
import Data.Aeson (Value)
import Data.Proxy
import qualified Data.Swagger as S
import qualified Data.Swagger.Declare as S
import qualified Data.Swagger.Internal.Schema as S
import Imports

type Declare = S.Declare (S.Definitions S.Schema)

-- | A typed Swagger schema
--
-- /Note/: the 'Applicative' and 'Alternative' instances of 'Schema'
-- have the same implementation, but different types. For example, the
-- binary operation of 'Applicative' is:
-- @
-- (<*>) :: f (a -> b) -> f a -> f b
-- @
-- while that of 'Alternative' is:
-- @
-- (<|>) :: f a -> f a -> f a.
-- @
-- The simple reason for this distinction is that the types will line
-- up with the 'Applicative' instance in some cases, and with the
-- 'Alternative' instance in others, so one needs both.
--
-- The deeper reason is that they are conceptually different. A schema like:
-- @
-- (,) <$> schemaA <*> schemaB
-- @
-- means that you will can have fields from @A@ and @B@ in the
-- resulting object, while
-- @
-- schemaA <|> schemaB
-- @
-- is only allowed to have fields from @A@ /or/ from @B@.
--
-- Unfortunately, there does not seem to be a way to express the
-- latter constraint in the resulting schema, and that's why the
-- implementations are the same. It seems this should be possible in
-- openapi3, thought.
--
-- The difference between 'Applicative' and 'Alternative' also becomes
-- important when we think about using a typed schema specification to
-- derive FromJSON instances. Then they would respectively implement
-- sequencing and alternation of the corresponding parsers.
newtype Schema (a :: *) = Schema (Declare S.Schema)
  deriving (Functor)

-- FUTUREWORK: store enough information in 'Schema' to synthesise
-- ToJSON and FromJSON instances

data NamedSchema a = NamedSchema
  { _nsSchema :: Declare S.NamedSchema,
    _nsDescription :: Maybe Text
  }
  deriving (Functor)

appendMaybe :: Maybe a -> Maybe a -> Maybe a
appendMaybe Nothing x = x
appendMaybe x _ = x

appendNS :: S.NamedSchema -> S.NamedSchema -> S.NamedSchema
appendNS (S.NamedSchema n1 s1) (S.NamedSchema n2 s2) =
  S.NamedSchema (appendMaybe n1 n2) (s1 <> s2)

instance Applicative NamedSchema where
  pure _ = NamedSchema (pure (S.NamedSchema Nothing mempty)) Nothing
  NamedSchema decl1 descr1 <*> NamedSchema decl2 descr2 =
    NamedSchema (appendNS <$> decl1 <*> decl2) (appendMaybe descr1 descr2)

instance Alternative NamedSchema where
  empty = NamedSchema (pure (S.NamedSchema Nothing mempty)) Nothing
  NamedSchema decl1 descr1 <|> NamedSchema decl2 descr2 =
    NamedSchema (appendNS <$> decl1 <*> decl2) (appendMaybe descr1 descr2)

runNamedSchema :: NamedSchema a -> Declare S.NamedSchema
runNamedSchema (NamedSchema decl descr) = do
  s <- decl
  pure $ s & (S.schema . S.description) .~ descr

instance Applicative Schema where
  pure _ = Schema (pure mempty)
  Schema a <*> Schema b = Schema ((<>) <$> a <*> b)

instance Alternative Schema where
  empty = Schema (pure mempty)
  Schema a <|> Schema b = Schema ((<>) <$> a <*> b)

class HasSchemaRef f where
  schemaRef :: f a -> Declare (S.Referenced S.Schema)

instance HasSchemaRef Schema where
  schemaRef (Schema decl) = S.Inline <$> decl

instance HasSchemaRef NamedSchema where
  schemaRef tns = do
    ns <- runNamedSchema tns
    case ns of
      S.NamedSchema (Just n) s -> do
        S.declare [(n, s)]
        pure (S.Ref (S.Reference n))
      S.NamedSchema Nothing s ->
        pure (S.Inline s)

-- | Just like 'field', but with a trivial "modifier".
field' :: HasSchemaRef f => Text -> f a -> Schema a
field' name = field name id

-- | A schema for an object with a single field.
--
-- Multiple fields can be combined into a single object using the
-- 'Applicative' instance of 'Schema'. Note that 'field' accepts both
-- a 'NamedSchema' and 'Schema' for the inner field schema.
--
-- The second argument is an arbitrary "modifier" function from
-- 'S.Schema' to itself, to be applied to the field schema. The most
-- common use for this is to set some values, such as its
-- description. For example, using lenses:
-- @
--     field "name" (description ?~ "The name of this object") schema
-- @
-- creates a field called "name" with the given description, with a
-- type that is instance of 'TypedSchema'.
field :: HasSchemaRef f => Text -> (S.Schema -> S.Schema) -> f a -> Schema a
field name f s = Schema $ do
  ref <- schemaRef s
  let ref' = fmap f ref
  pure $
    mempty
      & S.type_ ?~ S.SwaggerObject
      & S.properties . at name ?~ ref'

-- | A schema for an array of items.
--
-- Just like 'field', it can take both 'NamedSchema' and 'Schema'
-- definitions.
array :: HasSchemaRef f => f a -> Schema [a]
array s = Schema $ do
  ref <- schemaRef s
  pure $
    mempty
      & S.type_ ?~ S.SwaggerArray
      & S.items ?~ S.SwaggerItemsObject ref

-- | A schema for an enumeration type.
--
-- This takes as argument a list of pairs @(value, schema)@ where
-- @value@ is one of the possible JSON values of the enumeration, and
-- @schema@ is the corresponding Swagger schema. At the moment, the
-- schemas are ignored, but they could be used to derive a FromJSON
-- instance.
enum :: [(Value, Schema a)] -> Schema a
enum ss =
  Schema $
    pure $
      mempty
        & S.type_ ?~ S.SwaggerString
        & S.enum_ ?~ map fst ss

-- | Used for schemas that can potentially fail.
handle :: Schema (Either Text a) -> Schema a
handle = coerce

-- | Coerce schema to a different type.
--
-- This is safe as far as the Swagger schema generation is concerned,
-- but it makes it impossible to derive a safe 'FromJSON' instance
-- from a typed schema definition.
coerce :: Schema a -> Schema b
coerce (Schema s) = Schema s

-- | Turn a 'Schema' into a 'NamedSchema' with the given name.
named :: Text -> Schema a -> NamedSchema a
named name (Schema s) = NamedSchema (S.NamedSchema (Just name) <$> s) Nothing

-- | Turn a 'NamedSchema' into a 'Schema'.
--
-- This is mostly useful to attach descriptions to field, since fields
-- containing referenced schemas cannot have a description. For example:
-- @
--     field "elements" (description ?~ "the elements of this object") (unnamed schema)
-- @
unnamed :: NamedSchema a -> Schema a
unnamed (NamedSchema s _) = Schema (view S.schema <$> s)

-- | Typed schema for a type that is instance of 'ToTypedSchema'
--
-- Reuse the typed schema definition which is part of the
-- 'ToTypedSchema' instance of a type. Using 'schema', one can split a
-- complicated shema definition into manageable parts by defining
-- instances of 'ToTypedSchema' for the various types involved.
schema :: ToTypedSchema a => NamedSchema a
schema = toTypedSchema Proxy

-- | Derive a typed schema from an untyped schema definition
--
-- This can be used for types that have a 'ToSchema' instance but lack
-- a 'ToTypedSchema' instance.
untypedSchema :: forall a. S.ToSchema a => NamedSchema a
untypedSchema = NamedSchema (pure (S.toNamedSchema (Proxy :: Proxy a))) Nothing

-- | A type with a canonical typed schema definition.
class ToTypedSchema a where
  toTypedSchema :: Proxy a -> NamedSchema a

-- | Wrapper to turn 'ToTypedSchema' into 'ToSchema'.
--
-- This is mostly for use in a 'deriving via' clause. For example:
-- @
--     data MyType = ...
--       deriving ToSchema via TypedSchema MyType
--
--     instance ToTypedSchema MyType where
--       toTypedSchema _ = ...
-- @
newtype TypedSchema a = TypedSchema a

instance ToTypedSchema a => S.ToSchema (TypedSchema a) where
  declareNamedSchema _ = runNamedSchema (toTypedSchema Proxy :: NamedSchema a)

-- Lens instances

instance S.HasDescription (NamedSchema a) (Maybe Text) where
  description f s = fmap (\x -> s {_nsDescription = x}) . f . _nsDescription $ s
