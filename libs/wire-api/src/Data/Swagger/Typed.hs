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

field' :: HasSchemaRef f => Text -> f a -> Schema a
field' name = field name id

field :: HasSchemaRef f => Text -> (S.Schema -> S.Schema) -> f a -> Schema a
field name f s = Schema $ do
  ref <- schemaRef s
  let ref' = fmap f ref
  pure $
    mempty
      & S.type_ ?~ S.SwaggerObject
      & S.properties . at name ?~ ref'

array :: HasSchemaRef f => f a -> Schema [a]
array s = Schema $ do
  ref <- schemaRef s
  pure $
    mempty
      & S.type_ ?~ S.SwaggerArray
      & S.items ?~ S.SwaggerItemsObject ref

enum :: [(Value, Schema a)] -> Schema a
enum ss =
  Schema $
    pure $
      mempty
        & S.type_ ?~ S.SwaggerString
        & S.enum_ ?~ map fst ss

named :: Text -> Schema a -> NamedSchema a
named name (Schema s) = NamedSchema (S.NamedSchema (Just name) <$> s) Nothing

unnamed :: NamedSchema a -> Schema a
unnamed (NamedSchema s _) = Schema (view S.schema <$> s)

schema :: ToTypedSchema a => NamedSchema a
schema = toTypedSchema Proxy

untypedSchema :: forall a. S.ToSchema a => NamedSchema a
untypedSchema = NamedSchema (pure (S.toNamedSchema (Proxy :: Proxy a))) Nothing

class ToTypedSchema a where
  toTypedSchema :: Proxy a -> NamedSchema a

-- to use with deriving via
newtype TypedSchema a = TypedSchema a

instance ToTypedSchema a => S.ToSchema (TypedSchema a) where
  declareNamedSchema _ = runNamedSchema (toTypedSchema Proxy :: NamedSchema a)

-- Lens instances

instance S.HasDescription (NamedSchema a) (Maybe Text) where
  description f s = fmap (\x -> s {_nsDescription = x}) . f . _nsDescription $ s
