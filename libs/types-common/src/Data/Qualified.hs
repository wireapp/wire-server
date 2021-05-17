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
  ( -- * Optionally qualified
    OptionallyQualified (..),
    unqualified,
    qualified,
    eitherQualifiedOrNot,

    -- * Qualified
    Qualified (..),
    Remote,
    toRemote,
    renderQualifiedId,
    partitionRemoteOrLocalIds,
    partitionRemoteOrLocalIds',
    partitionQualified,
    deprecatedSchema,
  )
where

import Control.Applicative (optional)
import Control.Lens ((?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import Data.Bifunctor (first)
import Data.ByteString.Conversion (FromByteString (parser))
import Data.Domain (Domain, domainText)
import Data.Handle (Handle (..))
import Data.Id (Id (toUUID))
import qualified Data.Map as Map
import Data.Schema
import Data.String.Conversions (cs)
import qualified Data.Swagger as S
import Data.Tagged
import qualified Data.UUID as UUID
import Imports hiding (local)
import Test.QuickCheck (Arbitrary (arbitrary))

----------------------------------------------------------------------
-- OPTIONALLY QUALIFIED -- FUTUREWORK: remove optionally qualified, not used

data OptionallyQualified a = OptionallyQualified
  { oqUnqualified :: a,
    oqDomain :: Maybe Domain
  }
  deriving (Eq, Show)

unqualified :: a -> OptionallyQualified a
unqualified x = OptionallyQualified x Nothing

qualified :: Qualified a -> OptionallyQualified a
qualified (Qualified x domain) = OptionallyQualified x (Just domain)

eitherQualifiedOrNot :: OptionallyQualified a -> Either a (Qualified a)
eitherQualifiedOrNot = \case
  OptionallyQualified x Nothing -> Left x
  OptionallyQualified x (Just domain) -> Right (Qualified x domain)

optionallyQualifiedParser :: Atto.Parser a -> Atto.Parser (OptionallyQualified a)
optionallyQualifiedParser localParser =
  OptionallyQualified
    <$> localParser
    <*> optional (Atto.char '@' *> parser @Domain)

-- | we could have an
-- @instance FromByteString a => FromByteString (OptionallyQualified a)@,
-- but we only need this for specific things and don't want to just allow parsing things like
-- @OptionallyQualified HttpsUrl@.
instance FromByteString (OptionallyQualified (Id a)) where
  parser = optionallyQualifiedParser (parser @(Id a))

instance FromByteString (OptionallyQualified Handle) where
  parser = optionallyQualifiedParser (parser @Handle)

----------------------------------------------------------------------
-- QUALIFIED

data Qualified a = Qualified
  { qUnqualified :: a,
    qDomain :: Domain
  }
  deriving stock (Eq, Ord, Show, Generic, Functor)

-- | A type to differentiate between generally Qualified values, and values
-- where it is known if they are coming from a Remote backend or not.
-- Use 'toRemote' or 'partitionRemoteOrLocalIds\'' to get Remote values and use
-- 'unTagged' to convert from a Remote value back to a plain Qualified one.
type Remote a = Tagged "remote" (Qualified a)

-- | Convert a Qualified something to a Remote something.
toRemote :: Qualified a -> Remote a
toRemote = Tagged

-- | FUTUREWORK: Maybe delete this, it is only used in printing federation not
-- implemented errors
renderQualified :: (a -> Text) -> Qualified a -> Text
renderQualified renderLocal (Qualified localPart domain) =
  renderLocal localPart <> "@" <> domainText domain

partitionRemoteOrLocalIds :: Foldable f => Domain -> f (Qualified a) -> ([Qualified a], [a])
partitionRemoteOrLocalIds localDomain = foldMap $ \qualifiedId ->
  if qDomain qualifiedId == localDomain
    then (mempty, [qUnqualified qualifiedId])
    else ([qualifiedId], mempty)

partitionRemoteOrLocalIds' :: Foldable f => Domain -> f (Qualified a) -> ([Remote a], [a])
partitionRemoteOrLocalIds' localDomain xs = first (fmap toRemote) $ partitionRemoteOrLocalIds localDomain xs

-- | Index a list of qualified values by domain
partitionQualified :: [Qualified a] -> Map Domain [a]
partitionQualified = foldr add mempty
  where
    add :: Qualified a -> Map Domain [a] -> Map Domain [a]
    add (Qualified x domain) = Map.insertWith (<>) domain [x]

----------------------------------------------------------------------

renderQualifiedId :: Qualified (Id a) -> Text
renderQualifiedId = renderQualified (cs . UUID.toString . toUUID)

deprecatedSchema :: Text -> ValueSchema NamedSwaggerDoc a -> ValueSchema SwaggerDoc a
deprecatedSchema new = (doc . description ?~ ("Deprecated, use " <> new)) . unnamed

qualifiedSchema ::
  Text ->
  Text ->
  ValueSchema NamedSwaggerDoc a ->
  ValueSchema NamedSwaggerDoc (Qualified a)
qualifiedSchema name fieldName sch =
  object ("Qualified " <> name) $
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

instance Arbitrary a => Arbitrary (OptionallyQualified a) where
  arbitrary = OptionallyQualified <$> arbitrary <*> arbitrary
