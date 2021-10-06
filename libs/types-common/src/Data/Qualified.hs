{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData #-}

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

module Data.Qualified
  ( -- * Qualified
    Qualified (..),
    QualifiedWithTag,
    qUntagged,
    qTagUnsafe,
    Remote,
    toRemoteUnsafe,
    Local,
    toLocalUnsafe,
    lUnqualified,
    lDomain,
    qualifyAs,
    foldQualified,
    renderQualifiedId,
    partitionRemoteOrLocalIds,
    partitionRemoteOrLocalIds',
    indexQualified,
    deprecatedSchema,
    partitionRemote,
  )
where

import Control.Lens ((?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Bifunctor (first)
import Data.Domain (Domain, domainText)
import Data.Handle (Handle (..))
import Data.Id (Id (toUUID))
import qualified Data.Map as Map
import Data.Schema
import Data.String.Conversions (cs)
import qualified Data.Swagger as S
import qualified Data.UUID as UUID
import Imports hiding (local)
import Test.QuickCheck (Arbitrary (arbitrary))

----------------------------------------------------------------------
-- QUALIFIED

data Qualified a = Qualified
  { qUnqualified :: a,
    qDomain :: Domain
  }
  deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

data QTag = QLocal | QRemote
  deriving (Eq, Show)

-- | A type to differentiate between generally 'Qualified' values, and "tagged" values,
-- for which it is known whether they are coming from a remote or local backend.
-- Use 'foldQualified' or 'partitionRemoteOrLocalIds\'' to get tagged values and use
-- 'qUntagged' to convert from a tagged value back to a plain 'Qualified' one.
newtype QualifiedWithTag (t :: QTag) a = QualifiedWithTag {qUntagged :: Qualified a}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)
  deriving newtype (Arbitrary)

qTagUnsafe :: forall t a. Qualified a -> QualifiedWithTag t a
qTagUnsafe = QualifiedWithTag

-- | A type representing a 'Qualified' value where the domain is guaranteed to
-- be remote.
type Remote = QualifiedWithTag 'QRemote

-- | Convert a 'Qualified' value to a 'Remote' value. This is only safe if we
-- already know that the domain of the value is remote.
toRemoteUnsafe :: Qualified a -> Remote a
toRemoteUnsafe = qTagUnsafe

-- | A type representing a 'Qualified' value where the domain is guaranteed to
-- be local.
type Local = QualifiedWithTag 'QLocal

-- | Convert a 'Qualified' value to a 'Local' value. This is only safe if we
-- already know that the domain of the value is local.
toLocalUnsafe :: Qualified a -> Local a
toLocalUnsafe = qTagUnsafe

lUnqualified :: Local a -> a
lUnqualified = qUnqualified . qUntagged

lDomain :: Local a -> Domain
lDomain = qDomain . qUntagged

-- | Convert an unqualified value to a qualified one, with the same tag as the
-- given tagged qualified value.
qualifyAs :: QualifiedWithTag t x -> a -> QualifiedWithTag t a
qualifyAs = ($>)

foldQualified :: Local x -> (Local a -> b) -> (Remote a -> b) -> Qualified a -> b
foldQualified loc f g q
  | lDomain loc == qDomain q =
    f (toLocalUnsafe q)
  | otherwise =
    g (toRemoteUnsafe q)

-- | FUTUREWORK: Maybe delete this, it is only used in printing federation not
-- implemented errors
renderQualified :: (a -> Text) -> Qualified a -> Text
renderQualified renderLocal (Qualified localPart domain) =
  renderLocal localPart <> "@" <> domainText domain

-- FUTUREWORK: we probably want to use the primed function everywhere. Refactor these two functions to only have one.
partitionRemoteOrLocalIds :: Foldable f => Domain -> f (Qualified a) -> ([Qualified a], [a])
partitionRemoteOrLocalIds localDomain = foldMap $ \qualifiedId ->
  if qDomain qualifiedId == localDomain
    then (mempty, [qUnqualified qualifiedId])
    else ([qualifiedId], mempty)

partitionRemoteOrLocalIds' :: Foldable f => Domain -> f (Qualified a) -> ([Remote a], [a])
partitionRemoteOrLocalIds' localDomain xs = first (fmap toRemoteUnsafe) $ partitionRemoteOrLocalIds localDomain xs

-- | Index a list of qualified values by domain.
indexQualified :: Foldable f => f (Qualified a) -> Map Domain [a]
indexQualified = foldr add mempty
  where
    add :: Qualified a -> Map Domain [a] -> Map Domain [a]
    add (Qualified x domain) = Map.insertWith (<>) domain [x]

partitionRemote :: (Functor f, Foldable f) => f (Remote a) -> [(Domain, [a])]
partitionRemote remotes = Map.assocs $ indexQualified (qUntagged <$> remotes)

----------------------------------------------------------------------

renderQualifiedId :: Qualified (Id a) -> Text
renderQualifiedId = renderQualified (cs . UUID.toString . toUUID)

deprecatedSchema :: S.HasDescription doc (Maybe Text) => Text -> ValueSchema doc a -> ValueSchema doc a
deprecatedSchema new = doc . description ?~ ("Deprecated, use " <> new)

qualifiedSchema ::
  Text ->
  Text ->
  ValueSchema NamedSwaggerDoc a ->
  ValueSchema NamedSwaggerDoc (Qualified a)
qualifiedSchema name fieldName sch =
  object ("Qualified_" <> name) $
    Qualified
      <$> qUnqualified .= field fieldName sch
      <*> qDomain .= field "domain" schema

instance ToSchema (Qualified (Id a)) where
  schema = qualifiedSchema "UserId" "id" schema

instance ToSchema (Qualified Handle) where
  schema = qualifiedSchema "Handle" "handle" schema

instance ToJSON (Qualified (Id a)) where
  toJSON = schemaToJSON

instance FromJSON (Qualified (Id a)) where
  parseJSON = schemaParseJSON

instance S.ToSchema (Qualified (Id a)) where
  declareNamedSchema = schemaToSwagger

instance ToJSON (Qualified Handle) where
  toJSON = schemaToJSON

instance FromJSON (Qualified Handle) where
  parseJSON = schemaParseJSON

instance S.ToSchema (Qualified Handle) where
  declareNamedSchema = schemaToSwagger

----------------------------------------------------------------------
-- ARBITRARY

instance Arbitrary a => Arbitrary (Qualified a) where
  arbitrary = Qualified <$> arbitrary <*> arbitrary
