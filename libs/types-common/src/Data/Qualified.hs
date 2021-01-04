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
    renderQualifiedId,
    mkQualifiedId,
    renderQualifiedHandle,
    mkQualifiedHandle,
    partitionRemoteOrLocalIds,
    deprecatedUnqualifiedSchemaRef,
  )
where

import Control.Applicative (optional)
import Control.Lens (view, (.~), (?~))
import Data.Aeson (FromJSON, ToJSON, withObject, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import Data.Bifunctor (first)
import Data.ByteString.Conversion (FromByteString (parser))
import Data.Domain (Domain, domainText)
import Data.Handle (Handle (..))
import Data.Id (Id (toUUID))
import Data.Proxy (Proxy (..))
import Data.String.Conversions (cs)
import Data.Swagger
import Data.Swagger.Declare (Declare, DeclareT)
import qualified Data.Text.Encoding as Text.E
import qualified Data.UUID as UUID
import Imports hiding (local)
import Servant.API (FromHttpApiData (parseUrlPiece))
import Test.QuickCheck (Arbitrary (arbitrary))

----------------------------------------------------------------------
-- OPTIONALLY QUALIFIED

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
  deriving stock (Eq, Ord, Show, Generic)

renderQualified :: (a -> Text) -> Qualified a -> Text
renderQualified renderLocal (Qualified localPart domain) =
  renderLocal localPart <> "@" <> domainText domain

-- FUTUREWORK: do we want a different way to serialize these than with an '@' ? A '/' was talked about also.
--
-- renderQualified :: (a -> Text) -> Qualified a -> Text
-- renderQualified renderLocal (Qualified localPart domain) =
-- domainText domain <> "/" <> renderLocal localPart
--
-- qualifiedParser :: Atto.Parser a -> Atto.Parser (Qualified a)
--   domain <- parser @Domain
--   _ <- Atto.char '/'
--   local <- localParser
--   pure $ Qualified local domain

qualifiedParser :: Atto.Parser a -> Atto.Parser (Qualified a)
qualifiedParser localParser = do
  Qualified <$> localParser <*> (Atto.char '@' *> parser @Domain)

partitionRemoteOrLocalIds :: Foldable f => Domain -> f (Qualified a) -> ([Qualified a], [a])
partitionRemoteOrLocalIds localDomain = foldMap $ \qualifiedId ->
  if qDomain qualifiedId == localDomain
    then (mempty, [qUnqualified qualifiedId])
    else ([qualifiedId], mempty)

----------------------------------------------------------------------

renderQualifiedId :: Qualified (Id a) -> Text
renderQualifiedId = renderQualified (cs . UUID.toString . toUUID)

mkQualifiedId :: Text -> Either String (Qualified (Id a))
mkQualifiedId = Atto.parseOnly (parser <* Atto.endOfInput) . Text.E.encodeUtf8

deprecatedUnqualifiedSchemaRef :: ToSchema a => Proxy a -> Text -> Declare (Definitions Schema) (Referenced Schema)
deprecatedUnqualifiedSchemaRef p newField =
  Inline
    . (description ?~ ("Deprecated, use " <> newField))
    . view schema
    <$> declareNamedSchema p

instance ToSchema (Qualified (Id a)) where
  declareNamedSchema _ =
    declareQualifiedSchema "Qualified Id" "id" =<< declareSchemaRef (Proxy @(Id a))

instance ToJSON (Qualified (Id a)) where
  toJSON qu =
    Aeson.object
      [ "id" .= qUnqualified qu,
        "domain" .= qDomain qu
      ]

instance FromJSON (Qualified (Id a)) where
  parseJSON = withObject "QualifiedUserId" $ \o ->
    Qualified <$> o .: "id" <*> o .: "domain"

instance FromHttpApiData (Qualified (Id a)) where
  parseUrlPiece = first cs . mkQualifiedId

instance FromByteString (Qualified (Id a)) where
  parser = qualifiedParser parser

declareQualifiedSchema :: Text -> Text -> Referenced Schema -> DeclareT (Definitions Schema) Identity NamedSchema
declareQualifiedSchema qualifiedSchemaName unqualifiedFieldName unqualifiedSchemaRef = do
  domainSchema <- declareSchemaRef (Proxy @Domain)
  return $
    NamedSchema (Just qualifiedSchemaName) $
      mempty
        & type_ ?~ SwaggerObject
        & properties
          .~ [ (unqualifiedFieldName, unqualifiedSchemaRef),
               ("domain", domainSchema)
             ]

----------------------------------------------------------------------

renderQualifiedHandle :: Qualified Handle -> Text
renderQualifiedHandle = renderQualified fromHandle

mkQualifiedHandle :: Text -> Either String (Qualified Handle)
mkQualifiedHandle = Atto.parseOnly (parser <* Atto.endOfInput) . Text.E.encodeUtf8

instance FromHttpApiData (Qualified Handle) where
  parseUrlPiece = first cs . mkQualifiedHandle

-- TODO: Delete this?
instance FromByteString (Qualified Handle) where
  parser = qualifiedParser parser

instance ToSchema (Qualified Handle) where
  declareNamedSchema _ =
    declareQualifiedSchema "Qualified Handle" "handle" =<< declareSchemaRef (Proxy @Handle)

instance ToJSON (Qualified Handle) where
  toJSON qh =
    Aeson.object
      [ "handle" .= qUnqualified qh,
        "domain" .= qDomain qh
      ]

instance FromJSON (Qualified Handle) where
  parseJSON = withObject "Qualified Handle" $ \o ->
    Qualified <$> o .: "handle" <*> o .: "domain"

----------------------------------------------------------------------
-- ARBITRARY

instance Arbitrary a => Arbitrary (Qualified a) where
  arbitrary = Qualified <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (OptionallyQualified a) where
  arbitrary = OptionallyQualified <$> arbitrary <*> arbitrary
