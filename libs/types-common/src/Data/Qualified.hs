{-# LANGUAGE StrictData #-}

module Data.Qualified where

import Data.Aeson (FromJSON, ToJSON, withText)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString.Conversion as BS.C
import Data.Domain (Domain, domainText, mkDomain)
import Data.Handle (Handle (..))
import Data.Id (Id (toUUID))
import Data.String.Conversions (cs)
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import Imports hiding (local)
import Servant.API (FromHttpApiData (parseUrlPiece))
import Test.QuickCheck (Arbitrary (arbitrary))

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
  case Text.split (== '@') txt of
    [local, domain] -> do
      _qDomain <- mkDomain domain
      _qLocalPart <- mkLocal local
      pure Qualified {_qLocalPart, _qDomain}
    [_one] ->
      Left "not a qualified identifier: no '@'"
    _more ->
      Left "not a qualified identifier: multiple '@'s"

instance ToJSON (Qualified (Id a)) where
  toJSON = Aeson.String . renderQualified (cs . UUID.toString . toUUID)

instance FromJSON (Qualified (Id a)) where
  parseJSON =
    withText "QualifiedUserId" $
      either fail pure
        . mkQualified (first cs . BS.C.runParser BS.C.parser . cs)
        . cs

instance FromHttpApiData (Qualified (Id a)) where
  parseUrlPiece = first cs . mkQualified (BS.C.runParser BS.C.parser . cs)

instance ToJSON (Qualified Handle) where
  toJSON = Aeson.String . renderQualified fromHandle

instance FromJSON (Qualified Handle) where
  parseJSON =
    withText "QualifiedHandle" $
      either fail pure
        . mkQualified (BS.C.runParser BS.C.parser . cs)
        . cs

instance FromHttpApiData (Qualified Handle) where
  parseUrlPiece = first cs . mkQualified (BS.C.runParser BS.C.parser . cs)

----------------------------------------------------------------------
-- ARBITRARY

instance Arbitrary a => Arbitrary (Qualified a) where
  arbitrary = Qualified <$> arbitrary <*> arbitrary
