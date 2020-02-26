{-# LANGUAGE StrictData #-}

module Data.Qualified where

import Data.Aeson (FromJSON, ToJSON, withText)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString.Conversion as BS.C
import Data.Domain (Domain, domainText)
import Data.Handle (Handle (..))
import Data.Id (Id (toUUID), UserId)
import Data.String.Conversions (cs)
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import Imports
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

mkQualified :: (ByteString -> Either Text a) -> ByteString -> Either Text (Qualified a)
mkQualified = undefined -- TODO: implement a parser

instance ToJSON (Qualified UserId) where
  toJSON = Aeson.String . renderQualified (cs . UUID.toString . toUUID)

instance FromJSON (Qualified UserId) where
  parseJSON =
    withText "QualifiedUserId" $
      either (fail . Text.unpack) pure
        . mkQualified (first cs . BS.C.runParser BS.C.parser)
        . cs

instance FromHttpApiData (Qualified UserId) where
  parseUrlPiece _raw = do
    error "TODO"

instance ToJSON (Qualified Handle) where
  toJSON = Aeson.String . renderQualified fromHandle

instance FromJSON (Qualified Handle) where
  parseJSON =
    withText "QualifiedHandle" $
      either (fail . Text.unpack) pure
        . mkQualified (first cs . BS.C.runParser BS.C.parser)
        . cs

instance FromHttpApiData (Qualified Handle) where
  parseUrlPiece _raw = do
    error "TODO"

----------------------------------------------------------------------
-- ARBITRARY

instance Arbitrary a => Arbitrary (Qualified a) where
  arbitrary = Qualified <$> arbitrary <*> arbitrary
