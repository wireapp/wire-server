{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}

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

-- | Combinator library for defining bidirectional JSON encodings with
-- associated Swagger schemas.
--
-- Documentation on the organisation of this library and a tutorial
-- can be found in @README.md@.
module Data.Schema
  ( SchemaP,
    ValueSchema,
    ValueSchemaP,
    ObjectSchema,
    ObjectSchemaP,
    ToSchema (..),
    Schema (..),
    mkSchema,
    schemaDoc,
    schemaIn,
    schemaOut,
    HasDoc (..),
    doc',
    HasSchemaRef (..),
    HasObject (..),
    HasField (..),
    withParser,
    SwaggerDoc,
    swaggerDoc,
    NamedSwaggerDoc,
    WithDeclare,
    declareSwaggerSchema,
    getName,
    object,
    objectWithDocModifier,
    objectOver,
    jsonObject,
    jsonValue,
    FieldFunctor (..),
    field,
    fieldWithDocModifier,
    fieldOver,
    optField,
    optFieldWithDocModifier,
    fieldF,
    fieldOverF,
    fieldWithDocModifierF,
    array,
    set,
    nonEmptyArray,
    map_,
    mapWithKeys,
    enum,
    maybe_,
    maybeWithDefault,
    bind,
    dispatch,
    text,
    parsedText,
    null_,
    nullable,
    element,
    tag,
    unnamed,
    named,
    (.=),
    schemaToSwagger,
    schemaToJSON,
    schemaParseJSON,
    genericToSchema,
    S.description, -- re-export
  )
where

import Control.Applicative
import Control.Comonad
import Control.Lens hiding (element, enum, set, (.=))
import Control.Lens qualified as Lens
import Control.Monad.Trans.Cont
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types qualified as A
import Data.Bifunctor.Joker
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Monoid hiding (Product)
import Data.OpenApi qualified as S
import Data.OpenApi.Declare qualified as S
import Data.Profunctor (Star (..))
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as V
import Imports hiding (Product)
import Numeric.Natural

type Declare = S.Declare (S.Definitions S.Schema)

newtype SchemaIn v a b = SchemaIn (v -> A.Parser b)
  deriving (Functor)
  deriving (Applicative, Alternative) via (ReaderT v A.Parser)
  deriving (Profunctor, Choice) via Joker (ReaderT v A.Parser)

instance Semigroup (SchemaIn v a b) where
  (<>) = (<|>)

instance Monoid (SchemaIn v a b) where
  mempty = empty

newtype SchemaOut v a b = SchemaOut (a -> Maybe v)
  deriving (Functor)
  deriving (Applicative) via (ReaderT a (Const (Ap Maybe v)))
  deriving (Profunctor) via Star (Const (Maybe v))
  deriving (Choice) via Star (Const (Alt Maybe v))

-- /Note/: deriving Choice via Star (Const (Maybe v)) would also
-- type-check, but it would use the wrong Monoid structure of Maybe v:
-- here we want the monoid structure corresponding to the Alternative
-- instance of Maybe, instead of the one coming from a Semigroup
-- structure of v.

-- The following instance is correct because `Ap Maybe v` is a
-- near-semiring when v is a monoid
instance (Monoid v) => Alternative (SchemaOut v a) where
  empty = mempty
  (<|>) = (<>)

instance Semigroup (SchemaOut v a b) where
  SchemaOut x1 <> SchemaOut x2 = SchemaOut $ \a ->
    x1 a <|> x2 a

instance Monoid (SchemaOut v a b) where
  mempty = SchemaOut (pure empty)

-- | A near-semiring (aka seminearring).
--
-- This is used for schema documentation types, to support different behaviours
-- for composing schemas sequentially vs alternatively.
class (Monoid m) => NearSemiRing m where
  zero :: m
  add :: m -> m -> m

newtype SchemaDoc doc a b = SchemaDoc {getDoc :: doc}
  deriving (Functor, Semigroup, Monoid, NearSemiRing)
  deriving (Applicative) via (Const doc)
  deriving (Profunctor, Choice) via Joker (Const doc)

instance (NearSemiRing doc) => Alternative (SchemaDoc doc a) where
  empty = zero
  (<|>) = add

class HasDoc a a' doc doc' | a a' -> doc doc' where
  doc :: Lens a a' doc doc'

instance HasDoc (SchemaDoc doc a b) (SchemaDoc doc' a b) doc doc' where
  doc = lens getDoc $ \s d -> s {getDoc = d}

-- | A combined JSON encoder-decoder with associated documentation.
--
-- A value of type 'SchemaP d v w a b', which we will refer to as a
-- /schema/, contains both a JSON parser and a JSON serialiser,
-- together with documentation-like metadata, such as a JSON or
-- Swagger schema.
--
-- The type variables are as follows:
--
--   [@d@] documentation type, usually a 'Monoid'.
--   [@v@] type of JSON values being parsed (e.g. 'A.Value').
--   [@w@] type of JSON values being serialised (e.g. 'A.Value').
--   [@a@] input type
--   [@b@] output type
--
-- Input and output types deserve some more explanation. We can think
-- of a value @sch@ of type 'SchemaP d v w a b' as a kind of special
-- "function" from @a@ to @b@, but where @a@ and @b@ might potentially
-- live in different "languages". The parser portion of @sch@ takes a
-- JSON-encoded value of type @a@ and produces a value of type @b@,
-- while the serialiser portion of @sch@ takes a haskell value of type
-- @a@ and produces a JSON-encoding of something of type @b@.
--
-- In terms of composability, this way of representing schemas (based
-- on input and output types) is superior to the perhaps more natural
-- approach of using "bidirectional functions" or isomorphisms (based
-- on a single type parameter).
--
-- Although schemas cannot be composed as functions (i.e. they do not
-- form a 'Category'), they still admit a number of important and
-- useful instances, such as 'Profunctor' (and specifically 'Choice'),
-- which makes it possible to use prism quite effectively to build
-- schema values.
--
-- Using type variables to represent JSON types might seem like
-- excessive generality, but it is useful to represent "intermediate"
-- schemas arising when building complex ones. For example, a schema
-- which is able to work with fields of a JSON object (see 'field')
-- should not output full-blown objects, but only lists of pairs, so
-- that they can be combined correctly via the usual 'Monoid'
-- structure of lists when using the 'Applicative' interface of
-- 'SchemaP d v w a b'.
--
-- The idea of using the profunctor structure of 'SchemaP' is taken
-- from the [codec](https://github.com/chpatrick/codec) library.
data SchemaP doc v w a b
  = SchemaP
      (SchemaDoc doc a b)
      (SchemaIn v a b)
      (SchemaOut w a b)
  deriving (Functor)

-- | Build a schema from documentation, parser and serialiser
mkSchema :: doc -> (v -> A.Parser b) -> (a -> Maybe w) -> SchemaP doc v w a b
mkSchema d i o = SchemaP (SchemaDoc d) (SchemaIn i) (SchemaOut o)

instance (Monoid doc, Monoid v') => Applicative (SchemaP doc v v' a) where
  pure x = SchemaP (pure x) (pure x) (pure x)
  SchemaP d1 i1 o1 <*> SchemaP d2 i2 o2 =
    SchemaP (d1 <*> d2) (i1 <*> i2) (o1 <*> o2)

instance (NearSemiRing doc, Monoid v') => Alternative (SchemaP doc v v' a) where
  empty = SchemaP empty empty empty
  SchemaP d1 i1 o1 <|> SchemaP d2 i2 o2 =
    SchemaP (d1 <|> d2) (i1 <|> i2) (o1 <|> o2)

-- /Note/: this is a more general instance than the 'Alternative' one,
-- since it works for arbitrary v'
instance (Semigroup doc) => Semigroup (SchemaP doc v v' a b) where
  SchemaP d1 i1 o1 <> SchemaP d2 i2 o2 =
    SchemaP (d1 <> d2) (i1 <> i2) (o1 <> o2)

instance (Monoid doc) => Monoid (SchemaP doc v v' a b) where
  mempty = SchemaP mempty mempty mempty

instance Profunctor (SchemaP doc v v') where
  dimap f g (SchemaP d i o) =
    SchemaP (dimap f g d) (dimap f g i) (dimap f g o)

instance Choice (SchemaP doc v v') where
  left' (SchemaP d i o) = SchemaP (left' d) (left' i) (left' o)
  right' (SchemaP d i o) = SchemaP (right' d) (right' i) (right' o)

instance HasDoc (SchemaP doc v v' a b) (SchemaP doc' v v' a b) doc doc' where
  doc = lens schemaDoc $ \(SchemaP d i o) d' -> SchemaP (Lens.set doc d' d) i o

doc' :: Lens' (SchemaP doc v w a b) doc
doc' = doc

withParser :: SchemaP doc v w a b -> (b -> A.Parser b') -> SchemaP doc v w a b'
withParser (SchemaP (SchemaDoc d) (SchemaIn p) (SchemaOut o)) q =
  SchemaP (SchemaDoc d) (SchemaIn (p >=> q)) (SchemaOut o)

type ObjectSchemaP doc = SchemaP doc A.Object [A.Pair]

type ObjectSchema doc a = ObjectSchemaP doc a a

type ValueSchemaP doc = SchemaP doc A.Value A.Value

type ValueSchema doc a = ValueSchemaP doc a a

schemaDoc :: SchemaP ss v m a b -> ss
schemaDoc (SchemaP (SchemaDoc d) _ _) = d

schemaIn :: SchemaP doc v v' a b -> v -> A.Parser b
schemaIn (SchemaP _ (SchemaIn i) _) = i

schemaOut :: SchemaP ss v m a b -> a -> Maybe m
schemaOut (SchemaP _ _ (SchemaOut o)) = o

class (Functor f) => FieldFunctor doc f where
  parseFieldF :: (A.Value -> A.Parser a) -> A.Object -> Text -> A.Parser (f a)
  mkDocF :: doc -> doc

instance FieldFunctor doc Identity where
  parseFieldF f obj key = Identity <$> A.explicitParseField f obj (Key.fromText key)
  mkDocF = id

instance (HasOpt doc) => FieldFunctor doc Maybe where
  parseFieldF f obj key = A.explicitParseFieldMaybe f obj (Key.fromText key)
  mkDocF = mkOpt

-- | A schema for a one-field JSON object.
field ::
  forall doc' doc a b.
  (HasField doc' doc) =>
  Text ->
  SchemaP doc' A.Value A.Value a b ->
  SchemaP doc A.Object [A.Pair] a b
field = fieldOver id

-- | A schema for a JSON object with a single optional field.
optField ::
  forall doc doc' a b.
  (HasOpt doc, HasField doc' doc) =>
  Text ->
  SchemaP doc' A.Value A.Value a b ->
  SchemaP doc A.Object [A.Pair] a (Maybe b)
optField = fieldF

-- | Generalization of 'optField' with 'FieldFunctor'.
fieldF ::
  forall doc' doc f a b.
  (HasField doc' doc, FieldFunctor doc f) =>
  Text ->
  SchemaP doc' A.Value A.Value a b ->
  SchemaP doc A.Object [A.Pair] a (f b)
fieldF = fieldOverF id

newtype Positive x y a = Positive {runPositive :: (a -> x) -> y}
  deriving (Functor)

-- | A version of 'field' for more general input values.
--
-- This can be used when the input type 'v' of the parser is not exactly a
-- 'A.Object', but it contains one. The first argument is a lens that can
-- extract the 'A.Object' contained in 'v'.
--
-- See 'bind' for use cases.
fieldOverF ::
  forall f doc' doc v v' a b.
  (HasField doc' doc, FieldFunctor doc f) =>
  Lens v v' A.Object A.Value ->
  Text ->
  SchemaP doc' v' A.Value a b ->
  SchemaP doc v [A.Pair] a (f b)
fieldOverF l name sch = SchemaP (SchemaDoc s) (SchemaIn r) (SchemaOut w)
  where
    parseField :: A.Object -> Positive (A.Parser b) (A.Parser (f b)) A.Value
    parseField obj = Positive $ \k -> parseFieldF @doc k obj name

    r :: v -> A.Parser (f b)
    r obj = runPositive (l parseField obj) (schemaIn sch)

    w x = do
      v <- schemaOut sch x
      pure [Key.fromText name A..= v]

    s = mkDocF @doc @f (mkField name (schemaDoc sch))

-- | Like 'fieldOverF', but specialised to the identity functor.
fieldOver ::
  forall doc' doc v v' a b.
  (HasField doc' doc) =>
  Lens v v' A.Object A.Value ->
  Text ->
  SchemaP doc' v' A.Value a b ->
  SchemaP doc v [A.Pair] a b
fieldOver l name = fmap runIdentity . fieldOverF l name

-- | Like 'field', but apply an arbitrary function to the
-- documentation of the field.
fieldWithDocModifier ::
  forall doc' doc a b.
  (HasField doc' doc) =>
  Text ->
  (doc' -> doc') ->
  SchemaP doc' A.Value A.Value a b ->
  SchemaP doc A.Object [A.Pair] a b
fieldWithDocModifier name modify sch = field @doc' @doc name (over doc modify sch)

-- | Like 'optField', but apply an arbitrary function to the
-- documentation of the field.
optFieldWithDocModifier ::
  forall doc doc' a b.
  (HasOpt doc, HasField doc' doc) =>
  Text ->
  (doc' -> doc') ->
  SchemaP doc' A.Value A.Value a b ->
  SchemaP doc A.Object [A.Pair] a (Maybe b)
optFieldWithDocModifier name modify sch = optField @doc @doc' name (over doc modify sch)

-- | Like 'fieldF', but apply an arbitrary function to the
-- documentation of the field.
fieldWithDocModifierF ::
  forall doc' doc f a b.
  (HasField doc' doc, FieldFunctor doc f) =>
  Text ->
  (doc' -> doc') ->
  SchemaP doc' A.Value A.Value a b ->
  SchemaP doc A.Object [A.Pair] a (f b)
fieldWithDocModifierF name modify sch = fieldF @doc' @doc name (over doc modify sch)

-- | Change the input type of a schema.
(.=) :: (Profunctor p) => (a -> a') -> p a' b -> p a b
(.=) = lmap

-- | Change the input and output types of a schema via a prism.
tag :: Prism b b' a a' -> SchemaP ss v m a a' -> SchemaP ss v m b b'
tag f = rmap runIdentity . f . rmap Identity

-- | A schema for a JSON object.
--
-- This can be used to convert a combination of schemas obtained using
-- 'field' into a single schema for a JSON object.
object ::
  (HasObject doc doc') =>
  Text ->
  SchemaP doc A.Object [A.Pair] a b ->
  SchemaP doc' A.Value A.Value a b
object = objectOver id

-- | A version of 'object' for more general input values.
--
-- Just like 'fieldOver', but for 'object'.
objectOver ::
  (HasObject doc doc') =>
  Lens v v' A.Value A.Object ->
  Text ->
  SchemaP doc v' [A.Pair] a b ->
  SchemaP doc' v A.Value a b
objectOver l name sch = SchemaP (SchemaDoc s) (SchemaIn r) (SchemaOut w)
  where
    parseObject val = ContT $ \k -> A.withObject (T.unpack name) k val
    r v = runContT (l parseObject v) (schemaIn sch)
    w x = A.object <$> schemaOut sch x
    s = mkObject name (schemaDoc sch)

-- | Like 'object', but apply an arbitrary function to the
-- documentation of the resulting object.
objectWithDocModifier ::
  (HasObject doc doc') =>
  Text ->
  (doc' -> doc') ->
  ObjectSchema doc a ->
  ValueSchema doc' a
objectWithDocModifier name modify sch = over doc modify (object name sch)

-- | Turn a named schema into an unnamed one.
--
-- This is mostly useful when using a schema as a field of a bigger
-- schema. If the inner schema is unnamed, it gets "inlined" in the
-- larger scheme definition, and otherwise it gets "referenced". This
-- combinator makes it possible to choose one of the two options.
unnamed :: (HasObject doc doc') => SchemaP doc' v m a b -> SchemaP doc v m a b
unnamed = over doc unmkObject

-- | Attach a name to a schema.
--
-- This only affects the documentation portion of a schema, and not
-- the parsing or serialisation.
named :: (HasObject doc doc') => Text -> SchemaP doc v m a b -> SchemaP doc' v m a b
named name = over doc (mkObject name)

-- | A schema for a JSON array.
array ::
  (HasArray ndoc doc, HasName ndoc) =>
  ValueSchema ndoc a ->
  ValueSchema doc [a]
array sch = SchemaP (SchemaDoc s) (SchemaIn r) (SchemaOut w)
  where
    name = maybe "array" ("array of " <>) (getName (schemaDoc sch))
    r = A.withArray (T.unpack name) $ \arr -> mapM (schemaIn sch) $ V.toList arr
    s = mkArray (schemaDoc sch)
    w x = A.Array . V.fromList <$> mapM (schemaOut sch) x

set ::
  (HasArray ndoc doc, HasName ndoc, Ord a) =>
  ValueSchema ndoc a ->
  ValueSchema doc (Set a)
set sch = SchemaP (SchemaDoc s) (SchemaIn r) (SchemaOut w)
  where
    name = maybe "set" ("set of " <>) (getName (schemaDoc sch))
    r = A.withArray (T.unpack name) $ \arr ->
      fmap Set.fromList . mapM (schemaIn sch) $ V.toList arr
    s = mkArray (schemaDoc sch)
    w x = A.Array . V.fromList <$> mapM (schemaOut sch) (Set.toList x)

nonEmptyArray ::
  (HasArray ndoc doc, HasName ndoc, HasMinItems doc (Maybe Integer)) =>
  ValueSchema ndoc a ->
  ValueSchema doc (NonEmpty a)
nonEmptyArray sch = setMinItems 1 $ NonEmpty.toList .= array sch `withParser` check
  where
    check =
      maybe (fail "Unexpected empty array found while parsing a NonEmpty") pure
        . NonEmpty.nonEmpty

-- | A schema for a JSON object with arbitrary keys of type 'k'. The type of
-- keys must have instances for 'A.FromJSONKey' and 'A.ToJSONKey'.
--
-- Use 'mapWithKeys' for key types that do not have such instances.
map_ ::
  forall ndoc doc k a.
  (HasMap ndoc doc, Ord k, A.FromJSONKey k, A.ToJSONKey k) =>
  ValueSchema ndoc a ->
  ValueSchema doc (Map k a)
map_ sch = mkSchema d i o
  where
    d = mkMap (schemaDoc sch)
    i :: A.Value -> A.Parser (Map k a)
    i = A.parseJSON >=> traverse (schemaIn sch)
    o = fmap A.toJSON . traverse (schemaOut sch)

-- | A schema for a JSON object with arbitrary keys of type 'k', where 'k' can
-- be converted to and from 'Text'.
mapWithKeys ::
  forall ndoc doc k a.
  (HasMap ndoc doc, Ord k) =>
  (k -> Text) ->
  (Text -> k) ->
  ValueSchema ndoc a ->
  ValueSchema doc (Map k a)
mapWithKeys keyToText textToKey sch =
  Map.mapKeys textToKey
    <$> Map.mapKeys keyToText .= map_ sch

-- Putting this in `where` clause causes compile error, maybe a bug in GHC?
setMinItems :: (HasMinItems doc (Maybe Integer)) => Integer -> ValueSchema doc a -> ValueSchema doc a
setMinItems m = doc . minItems ?~ m

-- | Ad-hoc class for types corresponding to JSON primitive types.
class (A.ToJSON a) => With a where
  with :: String -> (a -> A.Parser b) -> A.Value -> A.Parser b

instance With Text where
  with = A.withText

instance With Integer where
  with _ = (A.parseJSON >=>)

instance With Natural where
  with _ = (A.parseJSON >=>)

instance With Bool where
  with = A.withBool

-- | A schema for a single value of an enumeration.
element ::
  forall a b.
  (A.ToJSON a, Eq a, Eq b) =>
  a ->
  b ->
  SchemaP [A.Value] a (Alt Maybe a) b b
element label value = SchemaP (SchemaDoc d) (SchemaIn i) (SchemaOut o)
  where
    d = [A.toJSON label]
    i l = value <$ guard (label == l)
    o v = Alt (Just label) <$ guard (value == v)

-- | A schema for a JSON enumeration.
--
-- This is used to convert a combination of schemas obtained using
-- 'element' into a single schema for a JSON string.
enum ::
  forall v doc a b.
  (With v, HasEnum v doc) =>
  Text ->
  SchemaP [A.Value] v (Alt Maybe v) a b ->
  SchemaP doc A.Value A.Value a b
enum name sch = SchemaP (SchemaDoc d) (SchemaIn i) (SchemaOut o)
  where
    d = mkEnum @v name (schemaDoc sch)
    i x =
      with (T.unpack name) (schemaIn sch) x
        <|> fail ("Unexpected value for enum " <> T.unpack name)
    o = fmap A.toJSON . (getAlt <=< schemaOut sch)

-- | A schema for 'Maybe' that omits a field on serialisation.
--
-- This is most commonly used for optional fields, and it will cause the field
-- to be omitted from the output of the serialiser.
maybe_ :: (Monoid w) => SchemaP d v w a b -> SchemaP d v w (Maybe a) b
maybe_ = maybeWithDefault mempty

-- | A schema for 'Maybe', producing the given default value on serialisation.
maybeWithDefault :: w -> SchemaP d v w a b -> SchemaP d v w (Maybe a) b
maybeWithDefault w0 (SchemaP (SchemaDoc d) (SchemaIn i) (SchemaOut o)) =
  SchemaP (SchemaDoc d) (SchemaIn i) (SchemaOut (maybe (pure w0) o))

-- | A schema depending on a parsed value.
--
-- Even though 'SchemaP' does not expose a monadic interface, it is possible to
-- make the parser of a schema depend on the values parsed by a previous
-- schema.
--
-- For example, a schema for an object containing a "type" field which
-- determines how the rest of the object is parsed. To construct the schema to
-- use as the second argument of 'bind', one can use 'dispatch'.
bind ::
  (Monoid d, Monoid w) =>
  SchemaP d v w a b ->
  SchemaP d (v, b) w a c ->
  SchemaP d v w a (b, c)
bind sch1 sch2 = mkSchema d i o
  where
    d = schemaDoc sch1 <> schemaDoc sch2
    i v = do
      b <- schemaIn sch1 v
      c <- schemaIn sch2 (v, b)
      pure (b, c)
    o a = (<>) <$> schemaOut sch1 a <*> schemaOut sch2 a

-- | A union of schemas over a finite type of "tags".
--
-- Normally used together with 'bind' to construct schemas that depend on some
-- "tag" value.
dispatch ::
  (Bounded t, Enum t, Monoid d) =>
  (t -> SchemaP d v w a b) ->
  SchemaP d (v, t) w a b
dispatch sch = mkSchema d i o
  where
    allSch = foldMap sch (enumFromTo minBound maxBound)
    d = schemaDoc allSch
    o = schemaOut allSch
    i (v, t) = schemaIn (sch t) v

-- | A schema for a textual value.
text :: Text -> ValueSchema NamedSwaggerDoc Text
text name =
  named name $
    mkSchema
      (pure d)
      (A.withText (T.unpack name) pure)
      (pure . A.String)
  where
    d = mempty & S.type_ ?~ S.OpenApiString

-- | A schema for a textual value with possible failure.
parsedText ::
  Text ->
  (Text -> Either String a) ->
  SchemaP NamedSwaggerDoc A.Value A.Value Text a
parsedText name parser = text name `withParser` (either fail pure . parser)

-- | A schema for an arbitrary JSON object.
jsonObject :: ValueSchema SwaggerDoc A.Object
jsonObject =
  unnamed . object "Object" $
    mkSchema mempty pure (pure . (^.. ifolded . withIndex))

-- | A schema for an arbitrary JSON value.
jsonValue :: ValueSchema SwaggerDoc A.Value
jsonValue = mkSchema mempty pure Just

-- | A schema for a null value.
null_ :: (Monoid d) => ValueSchemaP d () ()
null_ = mkSchema mempty i o
  where
    i x = guard (x == A.Null)
    o _ = pure A.Null

-- | A schema for a nullable value.
--
-- The parser accepts a JSON null as a valid value, and converts it to
-- 'Nothing'. Any non-null value is parsed using the underlying schema.
--
-- The serialiser behaves similarly, but in the other direction.
nullable ::
  (Monoid d) =>
  ValueSchema d a ->
  ValueSchema d (Maybe a)
nullable s =
  mconcat
    [ tag _Nothing null_,
      tag _Just s
    ]

data WithDeclare s = WithDeclare (Declare ()) s
  deriving (Functor)

instance Comonad WithDeclare where
  extract (WithDeclare _ s) = s
  duplicate w@(WithDeclare d _) = WithDeclare d w

declared :: Lens (WithDeclare s) (WithDeclare t) s t
declared = lens (\(WithDeclare _ s) -> s) $ \(WithDeclare decl _) s' ->
  WithDeclare decl s'

instance Applicative WithDeclare where
  pure = WithDeclare (pure ())
  WithDeclare d1 s1 <*> WithDeclare d2 s2 =
    WithDeclare (d1 >> d2) (s1 s2)

instance (Semigroup s) => Semigroup (WithDeclare s) where
  WithDeclare d1 s1 <> WithDeclare d2 s2 =
    WithDeclare (d1 >> d2) (s1 <> s2)

instance (Monoid s) => Monoid (WithDeclare s) where
  mempty = WithDeclare (pure ()) mempty

instance (NearSemiRing s) => NearSemiRing (WithDeclare s) where
  zero = WithDeclare (pure ()) zero
  add (WithDeclare d1 s1) (WithDeclare d2 s2) =
    WithDeclare (d1 >> d2) (add s1 s2)

runDeclare :: WithDeclare s -> Declare s
runDeclare (WithDeclare m s) = s <$ m

unrunDeclare :: Declare s -> WithDeclare s
unrunDeclare decl = case S.runDeclare decl mempty of
  (defns, s) -> (`WithDeclare` s) $ do
    S.declare defns

type SwaggerDoc = WithDeclare S.Schema

type NamedSwaggerDoc = WithDeclare S.NamedSchema

-- addition of schemas is used by the alternative instance, and it works like
-- multiplication (i.e. the Monoid instance), except that it intersects required
-- fields instead of concatenating them
instance NearSemiRing S.Schema where
  zero = mempty
  add x y = (x <> y) & S.required .~ intersect (x ^. S.required) (y ^. S.required)

-- This class abstracts over SwaggerDoc and NamedSwaggerDoc
class HasSchemaRef doc where
  schemaRef :: doc -> WithDeclare (S.Referenced S.Schema)

instance HasSchemaRef SwaggerDoc where
  schemaRef = fmap S.Inline

instance HasSchemaRef NamedSwaggerDoc where
  schemaRef (WithDeclare decl (S.NamedSchema mn s)) =
    (`WithDeclare` mkRef s mn) $ do
      decl
      case mn of
        Just n -> S.declare [(n, s)]
        Nothing -> pure ()
    where
      mkRef _ (Just n) = S.Ref (S.Reference n)
      mkRef x Nothing = S.Inline x

class HasName doc where
  getName :: doc -> Maybe Text

instance HasName SwaggerDoc where
  getName = const Nothing

instance HasName NamedSwaggerDoc where
  getName = S._namedSchemaName . extract

class (Monoid doc) => HasField ndoc doc | ndoc -> doc where
  mkField :: Text -> ndoc -> doc

class (Monoid doc) => HasObject doc ndoc | doc -> ndoc, ndoc -> doc where
  mkObject :: Text -> doc -> ndoc
  unmkObject :: ndoc -> doc

class (Monoid doc) => HasArray ndoc doc | ndoc -> doc where
  mkArray :: ndoc -> doc

class (Monoid doc) => HasMap ndoc doc | ndoc -> doc where
  mkMap :: ndoc -> doc

class HasOpt doc where
  mkOpt :: doc -> doc

class HasEnum a doc where
  mkEnum :: Text -> [A.Value] -> doc

instance (HasSchemaRef doc) => HasField doc SwaggerDoc where
  mkField name = fmap f . schemaRef
    where
      f ref =
        mempty
          & S.type_ ?~ S.OpenApiObject
          & S.properties . at name ?~ ref
          & S.required .~ [name]

instance HasObject SwaggerDoc NamedSwaggerDoc where
  mkObject name decl = S.NamedSchema (Just name) <$> decl
  unmkObject (WithDeclare d (S.NamedSchema Nothing s)) = WithDeclare d s
  unmkObject (WithDeclare d (S.NamedSchema (Just n) s)) =
    WithDeclare (d *> S.declare [(n, s)]) s

instance (HasSchemaRef ndoc) => HasArray ndoc SwaggerDoc where
  mkArray = fmap f . schemaRef
    where
      f :: S.Referenced S.Schema -> S.Schema
      f ref =
        mempty
          & S.type_ ?~ S.OpenApiArray
          & S.items ?~ S.OpenApiItemsObject ref

instance (HasSchemaRef ndoc) => HasMap ndoc SwaggerDoc where
  mkMap = fmap f . schemaRef
    where
      f :: S.Referenced S.Schema -> S.Schema
      f ref =
        mempty
          & S.type_ ?~ S.OpenApiObject
          & S.additionalProperties ?~ S.AdditionalPropertiesSchema ref

class HasMinItems s a where
  minItems :: Lens' s a

instance HasMinItems SwaggerDoc (Maybe Integer) where
  minItems = declared . S.minItems

instance HasEnum Text NamedSwaggerDoc where
  mkEnum = mkSwaggerEnum S.OpenApiString

instance HasEnum Integer NamedSwaggerDoc where
  mkEnum = mkSwaggerEnum S.OpenApiInteger

instance HasEnum Natural NamedSwaggerDoc where
  mkEnum = mkSwaggerEnum S.OpenApiInteger

instance HasEnum Bool NamedSwaggerDoc where
  mkEnum = mkSwaggerEnum S.OpenApiBoolean

mkSwaggerEnum ::
  S.OpenApiType ->
  Text ->
  [A.Value] ->
  NamedSwaggerDoc
mkSwaggerEnum ty name labels =
  pure . S.NamedSchema (Just name) $
    mempty
      & S.type_ ?~ ty
      & S.enum_ ?~ labels

instance HasOpt SwaggerDoc where
  mkOpt = (S.schema . S.required) .~ []

instance HasOpt NamedSwaggerDoc where
  mkOpt = (S.schema . S.required) .~ []

-- | A type with a canonical typed schema definition.
--
-- Using ToSchema, one can split a complicated shema definition
-- into manageable parts by defining instances for the various types
-- involved, and using the 'schema' method to reuse the
-- previously-defined schema definitions for component types.
class ToSchema a where
  schema :: ValueSchema NamedSwaggerDoc a

-- Newtype wrappers for deriving via

newtype Schema a = Schema {getSchema :: a}
  deriving (Generic)

schemaToSwagger :: forall a. (ToSchema a) => Proxy a -> Declare S.NamedSchema
schemaToSwagger _ = runDeclare (schemaDoc (schema @a))

instance (Typeable a, ToSchema a) => S.ToSchema (Schema a) where
  declareNamedSchema _ = schemaToSwagger (Proxy @a)

-- | JSON serialiser for an instance of 'ToSchema'.
schemaToJSON :: forall a. (ToSchema a) => a -> A.Value
schemaToJSON = fromMaybe A.Null . schemaOut (schema @a)

instance (ToSchema a) => A.ToJSON (Schema a) where
  toJSON = schemaToJSON . getSchema

-- | JSON parser for an instance of 'ToSchema'.
schemaParseJSON :: forall a. (ToSchema a) => A.Value -> A.Parser a
schemaParseJSON = schemaIn schema

instance (ToSchema a) => A.FromJSON (Schema a) where
  parseJSON = fmap Schema . schemaParseJSON

instance ToSchema Text where schema = genericToSchema

instance ToSchema TL.Text where schema = genericToSchema

instance ToSchema Int where schema = genericToSchema

instance ToSchema Int32 where schema = genericToSchema

instance ToSchema Int64 where schema = genericToSchema

instance ToSchema Integer where schema = genericToSchema

instance ToSchema Word where schema = genericToSchema

instance ToSchema Word8 where schema = genericToSchema

instance ToSchema Word16 where schema = genericToSchema

instance ToSchema Word32 where schema = genericToSchema

instance ToSchema Word64 where schema = genericToSchema

instance ToSchema Char where schema = genericToSchema

instance ToSchema String where schema = genericToSchema

instance ToSchema Bool where schema = genericToSchema

instance ToSchema Natural where schema = genericToSchema

declareSwaggerSchema :: SchemaP (WithDeclare d) v w a b -> Declare d
declareSwaggerSchema = runDeclare . schemaDoc

swaggerDoc :: forall a. (S.ToSchema a) => NamedSwaggerDoc
swaggerDoc = unrunDeclare (S.declareNamedSchema (Proxy @a))

genericToSchema :: forall a. (S.ToSchema a, A.ToJSON a, A.FromJSON a) => ValueSchema NamedSwaggerDoc a
genericToSchema =
  SchemaP
    (SchemaDoc (swaggerDoc @a))
    (SchemaIn r)
    (SchemaOut w)
  where
    r = A.parseJSON
    w = Just . A.toJSON

-- Swagger lenses

instance S.HasSchema SwaggerDoc S.Schema where
  schema = declared

instance S.HasSchema NamedSwaggerDoc S.Schema where
  schema = declared . S.schema

instance (S.HasSchema d S.Schema) => S.HasSchema (SchemaP d v w a b) S.Schema where
  schema = doc . S.schema

instance S.HasDescription NamedSwaggerDoc (Maybe Text) where
  description = declared . S.schema . S.description

instance S.HasDeprecated NamedSwaggerDoc (Maybe Bool) where
  deprecated = declared . S.schema . S.deprecated

instance {-# OVERLAPPABLE #-} (S.HasDescription s a) => S.HasDescription (WithDeclare s) a where
  description = declared . S.description

instance {-# OVERLAPPABLE #-} (S.HasDeprecated s a) => S.HasDeprecated (WithDeclare s) a where
  deprecated = declared . S.deprecated

instance {-# OVERLAPPABLE #-} (S.HasExample s a) => S.HasExample (WithDeclare s) a where
  example = declared . S.example
