{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData #-}

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

module Data.Qualified
  ( -- * Qualified
    QTag (..),
    Qualified (..),
    qToPair,
    QualifiedWithTag,
    tUnqualified,
    tDomain,
    tUntagged,
    tSplit,
    qTagUnsafe,
    Remote,
    pattern Remote,
    pattern Local,
    toRemoteUnsafe,
    Local,
    toLocalUnsafe,
    qualifyAs,
    foldQualified,
    partitionQualified,
    partitionQualifiedAndTag,
    indexQualified,
    bucketQualified,
    bucketRemote,
    isLocal,
    deprecatedSchema,
    qualifiedSchema,
    qualifiedObjectSchema,
  )
where

import Control.Lens (over, (?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Bifunctor (first)
import Data.Domain (Domain)
import Data.Handle (Handle (..))
import Data.Id
import Data.Map qualified as Map
import Data.OpenApi (deprecated)
import Data.OpenApi qualified as S
import Data.Schema
import Imports hiding (local)
import Test.QuickCheck (Arbitrary (arbitrary))

----------------------------------------------------------------------
-- QUALIFIED

data Qualified a = Qualified
  { qUnqualified :: a,
    qDomain :: Domain
  }
  deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

qToPair :: Qualified a -> (Domain, a)
qToPair (Qualified x dom) = (dom, x)

data QTag = QLocal | QRemote
  deriving (Eq, Show)

-- | A type to differentiate between generally 'Qualified' values, and "tagged" values,
-- for which it is known whether they are coming from a remote or local backend.
-- Use 'foldQualified', 'partitionQualified' or 'qualifyLocal' to get tagged values and use
-- 'tUntagged' to convert from a tagged value back to a plain 'Qualified' one.
newtype QualifiedWithTag (t :: QTag) a = QualifiedWithTag {tUntagged :: Qualified a}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)
  deriving newtype (Arbitrary)

qTagUnsafe :: forall t a. Qualified a -> QualifiedWithTag t a
qTagUnsafe = QualifiedWithTag

tUnqualified :: QualifiedWithTag t a -> a
tUnqualified = qUnqualified . tUntagged

tDomain :: QualifiedWithTag t a -> Domain
tDomain = qDomain . tUntagged

-- | perform 'qUnqualified' and 'tDomain' at once. Useful in ViewPatterns.
tSplit :: QualifiedWithTag t a -> (Domain, a)
tSplit (tUntagged -> q) = (q.qDomain, q.qUnqualified)

-- | A type representing a 'Qualified' value where the domain is guaranteed to
-- be remote.
type Remote = QualifiedWithTag 'QRemote

-- | Convert a 'Domain' and an @a@ to a 'Remote' value. This is only safe if we
-- already know that the domain is remote.
toRemoteUnsafe :: Domain -> a -> Remote a
toRemoteUnsafe d a = qTagUnsafe $ Qualified a d

-- | A type representing a 'Qualified' value where the domain is guaranteed to
-- be local.
type Local = QualifiedWithTag 'QLocal

-- | Convert a 'Domain' and an @a@ to a 'Local' value. This is only safe if we
-- already know that the domain is local.
toLocalUnsafe :: Domain -> a -> Local a
toLocalUnsafe d a = qTagUnsafe $ Qualified a d

-- | Convert an unqualified value to a qualified one, with the same tag as the
-- given tagged qualified value.
qualifyAs :: QualifiedWithTag t x -> a -> QualifiedWithTag t a
qualifyAs = ($>)

foldQualified :: Local x -> (Local a -> b) -> (Remote a -> b) -> Qualified a -> b
foldQualified loc f g q
  | tDomain loc == qDomain q =
      f (qTagUnsafe q)
  | otherwise =
      g (qTagUnsafe q)

pattern Local :: forall a x. forall. Local a -> (Local x, Qualified a)
pattern Local loc <- ((\(loc, q) -> (tDomain loc == qDomain q, qUnqualified q <$ loc)) -> (True, loc))

pattern Remote :: forall a x. forall. Remote a -> (Local x, Qualified a)
pattern Remote loc <- ((\(loc, q) -> (tDomain loc == qDomain q, qTagUnsafe q)) -> (False, loc))

{-# COMPLETE Local, Remote #-}

-- pattern Remote :: Remote a -> (Local x, Qualified a)

-- Partition a collection of qualified values into locals and remotes.
--
-- Note that the local values are returned as unqualified values, as a (probably
-- insignificant) optimisation. Use 'partitionQualifiedAndTag' to get them as
-- 'Local' values.
partitionQualified :: (Foldable f) => Local x -> f (Qualified a) -> ([a], [Remote a])
partitionQualified loc =
  foldMap $
    foldQualified loc (\l -> ([tUnqualified l], mempty)) (\r -> (mempty, [r]))

partitionQualifiedAndTag :: (Foldable f) => Local x -> f (Qualified a) -> ([Local a], [Remote a])
partitionQualifiedAndTag loc =
  first (map (qualifyAs loc))
    . partitionQualified loc

-- | Index a list of qualified values by domain.
indexQualified :: (Foldable f) => f (Qualified a) -> Map Domain [a]
indexQualified = foldr add mempty
  where
    add :: Qualified a -> Map Domain [a] -> Map Domain [a]
    add (Qualified x domain) = Map.insertWith (<>) domain [x]

-- | Bucket a list of qualified values by domain.
bucketQualified :: (Foldable f) => f (Qualified a) -> [Qualified [a]]
bucketQualified = map (\(d, a) -> Qualified a d) . Map.assocs . indexQualified

bucketRemote :: (Functor f, Foldable f) => f (Remote a) -> [Remote [a]]
bucketRemote =
  map (uncurry toRemoteUnsafe)
    . Map.assocs
    . indexQualified
    . fmap tUntagged

isLocal :: Local x -> Qualified a -> Bool
isLocal loc = foldQualified loc (const True) (const False)

----------------------------------------------------------------------

deprecatedSchema :: (S.HasDeprecated doc (Maybe Bool), S.HasDescription doc (Maybe Text)) => Text -> ValueSchema doc a -> ValueSchema doc a
deprecatedSchema new =
  over doc $
    (description ?~ ("Deprecated, use " <> new))
      . (deprecated ?~ True)

qualifiedSchema ::
  (HasSchemaRef doc) =>
  Text ->
  Text ->
  ValueSchema doc a ->
  ValueSchema NamedSwaggerDoc (Qualified a)
qualifiedSchema name fieldName sch =
  object ("Qualified_" <> name) $
    qualifiedObjectSchema fieldName sch

qualifiedObjectSchema ::
  (HasSchemaRef d) =>
  Text ->
  ValueSchema d a ->
  ObjectSchema SwaggerDoc (Qualified a)
qualifiedObjectSchema fieldName sch =
  flip Qualified
    <$> qDomain .= field "domain" schema
    <*> qUnqualified .= field fieldName sch

instance (KnownIdTag t) => ToSchema (Qualified (Id t)) where
  schema = qualifiedSchema (idTagName (idTagValue @t) <> "Id") "id" schema

instance ToSchema (Qualified Handle) where
  schema = qualifiedSchema "Handle" "handle" schema

instance (KnownIdTag t) => ToJSON (Qualified (Id t)) where
  toJSON = schemaToJSON

instance (KnownIdTag t) => FromJSON (Qualified (Id t)) where
  parseJSON = schemaParseJSON

instance (Typeable t, KnownIdTag t) => S.ToSchema (Qualified (Id t)) where
  declareNamedSchema = schemaToSwagger

instance ToJSON (Qualified Handle) where
  toJSON = schemaToJSON

instance FromJSON (Qualified Handle) where
  parseJSON = schemaParseJSON

instance S.ToSchema (Qualified Handle) where
  declareNamedSchema = schemaToSwagger

----------------------------------------------------------------------
-- ARBITRARY

instance (Arbitrary a) => Arbitrary (Qualified a) where
  arbitrary = Qualified <$> arbitrary <*> arbitrary
