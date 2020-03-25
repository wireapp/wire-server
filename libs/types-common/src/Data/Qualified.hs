{-# LANGUAGE StrictData #-}

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
  )
where

import Control.Applicative (optional)
import Data.Aeson (FromJSON, ToJSON, withText)
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import Data.Bifunctor (first)
import Data.ByteString.Conversion (FromByteString (parser))
import Data.Domain (Domain, domainText)
import Data.Handle (Handle (..))
import Data.Id (Id (toUUID))
import Data.String.Conversions (cs)
import qualified Data.Text.Encoding as Text.E
import qualified Data.UUID as UUID
import Imports hiding (local)
import Servant.API (FromHttpApiData (parseUrlPiece))
import Test.QuickCheck (Arbitrary (arbitrary))

----------------------------------------------------------------------
-- OPTIONALLY QUALIFIED

data OptionallyQualified a
  = OptionallyQualified
      { _oqLocalPart :: a,
        _oqDomain :: Maybe Domain
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

data Qualified a
  = Qualified
      { _qLocalPart :: a,
        _qDomain :: Domain
      }
  deriving (Eq, Show, Generic)

renderQualified :: (a -> Text) -> Qualified a -> Text
renderQualified renderLocal (Qualified localPart domain) =
  renderLocal localPart <> "@" <> domainText domain

qualifiedParser :: Atto.Parser a -> Atto.Parser (Qualified a)
qualifiedParser localParser =
  Qualified <$> localParser <*> (Atto.char '@' *> parser @Domain)

----------------------------------------------------------------------

renderQualifiedId :: Qualified (Id a) -> Text
renderQualifiedId = renderQualified (cs . UUID.toString . toUUID)

mkQualifiedId :: Text -> Either String (Qualified (Id a))
mkQualifiedId = Atto.parseOnly (parser <* Atto.endOfInput) . Text.E.encodeUtf8

instance ToJSON (Qualified (Id a)) where
  toJSON = Aeson.String . renderQualifiedId

instance FromJSON (Qualified (Id a)) where
  parseJSON = withText "QualifiedUserId" $ either fail pure . mkQualifiedId

instance FromHttpApiData (Qualified (Id a)) where
  parseUrlPiece = first cs . mkQualifiedId

instance FromByteString (Qualified (Id a)) where
  parser = qualifiedParser parser

----------------------------------------------------------------------

renderQualifiedHandle :: Qualified Handle -> Text
renderQualifiedHandle = renderQualified fromHandle

mkQualifiedHandle :: Text -> Either String (Qualified Handle)
mkQualifiedHandle = Atto.parseOnly (parser <* Atto.endOfInput) . Text.E.encodeUtf8

instance ToJSON (Qualified Handle) where
  toJSON = Aeson.String . renderQualifiedHandle

instance FromJSON (Qualified Handle) where
  parseJSON = withText "QualifiedHandle" $ either fail pure . mkQualifiedHandle

instance FromHttpApiData (Qualified Handle) where
  parseUrlPiece = first cs . mkQualifiedHandle

instance FromByteString (Qualified Handle) where
  parser = qualifiedParser parser

----------------------------------------------------------------------
-- ARBITRARY

instance Arbitrary a => Arbitrary (Qualified a) where
  arbitrary = Qualified <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (OptionallyQualified a) where
  arbitrary = OptionallyQualified <$> arbitrary <*> arbitrary
