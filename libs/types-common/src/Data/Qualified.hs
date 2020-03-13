{-# LANGUAGE StrictData #-}

module Data.Qualified where

import Data.Aeson (FromJSON, ToJSON, withText)
import qualified Data.Aeson as Aeson
import Data.Attoparsec.ByteString (takeByteString)
import Data.Bifunctor (first)
import qualified Data.ByteString.Conversion as BS.C
import Data.ByteString.Conversion (FromByteString (parser))
import Data.Domain (Domain, domainText, mkDomain)
import Data.Handle (Handle (..))
import Data.Id (Id (toUUID))
import Data.String.Conversions (cs)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.E
import qualified Data.UUID as UUID
import Imports hiding (local)
import Servant.API (FromHttpApiData (parseUrlPiece))
import Test.QuickCheck (Arbitrary (arbitrary))

----------------------------------------------------------------------
-- OPTIONALLY QUALIFIED

type OptionallyQualified a = Either a (Qualified a)

unqualified :: a -> OptionallyQualified a
unqualified = Left

qualified :: Qualified a -> OptionallyQualified a
qualified = Right

{-
TODO: do it properly
Either is a hack, conceptually we want this:

data OptionallyQualified a
  = OptionallyQualified
      { _oqLocalPart :: a,
        _oqDomain :: Maybe Domain
      }
-}

-- OptionallyQualified (Id a)
instance FromByteString (Either (Id a) (Qualified (Id a))) where
  parser = asum [Left <$> parser, Right <$> parser]

-- OptionallyQualified Handle
instance FromByteString (Either Handle (Qualified Handle)) where
  parser = asum [Left <$> parser, Right <$> parser]

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

-- | The string to parse must contain exactly one @"@"@ to separate local part from domain.
mkQualified :: (Text -> Either String a) -> Text -> Either String (Qualified a)
mkQualified mkLocal txt =
  -- FUTUREWORK: this should be done in a less hacky way
  case Text.split (== '@') txt of
    [local, domain] -> do
      _qDomain <- mkDomain domain
      _qLocalPart <- mkLocal local
      pure Qualified {_qLocalPart, _qDomain}
    [_one] ->
      Left "not a qualified identifier: no '@'"
    _more ->
      Left "not a qualified identifier: multiple '@'s"

renderQualifiedId :: Qualified (Id a) -> Text
renderQualifiedId = renderQualified (cs . UUID.toString . toUUID)

mkQualifiedId :: Text -> Either String (Qualified (Id a))
mkQualifiedId = mkQualified (first cs . BS.C.runParser BS.C.parser . cs)

instance ToJSON (Qualified (Id a)) where
  toJSON = Aeson.String . renderQualifiedId

instance FromJSON (Qualified (Id a)) where
  parseJSON = withText "QualifiedUserId" $ either fail pure . mkQualifiedId

instance FromHttpApiData (Qualified (Id a)) where
  parseUrlPiece = first cs . mkQualifiedId

instance FromByteString (Qualified (Id a)) where
  parser = do
    bs <- takeByteString
    case makeFromByteString bs of
      Left err -> fail err
      Right qi -> pure qi
    where
      makeFromByteString = mkQualifiedId <=< first show . Text.E.decodeUtf8'

renderQualifiedHandle :: Qualified Handle -> Text
renderQualifiedHandle = renderQualified fromHandle

mkQualifiedHandle :: Text -> Either String (Qualified Handle)
mkQualifiedHandle = mkQualified (BS.C.runParser BS.C.parser . cs)

instance ToJSON (Qualified Handle) where
  toJSON = Aeson.String . renderQualifiedHandle

instance FromJSON (Qualified Handle) where
  parseJSON = withText "QualifiedHandle" $ either fail pure . mkQualifiedHandle

instance FromHttpApiData (Qualified Handle) where
  parseUrlPiece = first cs . mkQualifiedHandle

instance FromByteString (Qualified Handle) where
  parser = do
    bs <- takeByteString
    case makeFromByteString bs of
      Left err -> fail err
      Right qi -> pure qi
    where
      makeFromByteString = mkQualifiedHandle <=< first show . Text.E.decodeUtf8'

----------------------------------------------------------------------
-- ARBITRARY

instance Arbitrary a => Arbitrary (Qualified a) where
  arbitrary = Qualified <$> arbitrary <*> arbitrary
