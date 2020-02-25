module Federator.Types.FullyQualified where

import Data.Aeson (FromJSON, ToJSON, withText)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (bimap, first)
import qualified Data.ByteString.Conversion as BS.C
import Data.Handle (Handle (..))
import Data.Id (Id (toUUID), UserId)
import Data.String.Conversions (cs)
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import Galley.Types (EmailDomain, emailDomainText, mkEmailDomain)
import Imports
import Servant.API (FromHttpApiData (parseUrlPiece))
import Test.QuickCheck (Arbitrary (arbitrary), elements)

data FullyQualified a
  = FullyQualified
      { _fqLocalPart :: a,
        _fqDomain :: Domain
      }
  deriving (Eq, Show, Generic)

renderFullyQualified :: (a -> Text) -> FullyQualified a -> Text
renderFullyQualified renderLocal (FullyQualified localPart domain) =
  renderLocal localPart <> "@" <> emailDomainText (unDomain domain)

mkFullyQualified :: (ByteString -> Either Text a) -> ByteString -> Either Text (FullyQualified a)
mkFullyQualified = undefined -- TODO: implement a parser

instance ToJSON (FullyQualified UserId) where
  toJSON = Aeson.String . renderFullyQualified (cs . UUID.toString . toUUID)

instance FromJSON (FullyQualified UserId) where
  parseJSON =
    withText "FullyQualifiedUserId" $
      either (fail . Text.unpack) pure
        . mkFullyQualified (first cs . BS.C.runParser BS.C.parser)
        . cs

instance FromHttpApiData (FullyQualified UserId) where
  parseUrlPiece _raw = do
    error "TODO"

instance ToJSON (FullyQualified Handle) where
  toJSON = Aeson.String . renderFullyQualified fromHandle

instance FromJSON (FullyQualified Handle) where
  parseJSON =
    withText "FullyQualifiedHandle" $
      either (fail . Text.unpack) pure
        . mkFullyQualified (first cs . BS.C.runParser BS.C.parser)
        . cs

instance FromHttpApiData (FullyQualified Handle) where
  parseUrlPiece _raw = do
    error "TODO"

--

newtype Domain
  = Domain {unDomain :: EmailDomain}
  deriving stock (Eq, Show, Generic)

renderDomain :: Domain -> Text
renderDomain = emailDomainText . unDomain

mkDomain :: ByteString -> Either Text Domain
mkDomain = bimap cs Domain . mkEmailDomain

instance ToJSON Domain where
  toJSON = Aeson.String . renderDomain

instance FromJSON Domain where
  parseJSON = withText "Domain" $ either (fail . Text.unpack) pure . mkDomain . cs

----------------------------------------------------------------------
-- ARBITRARY

instance Arbitrary a => Arbitrary (FullyQualified a) where
  arbitrary = FullyQualified <$> arbitrary <*> arbitrary

instance Arbitrary Domain where
  arbitrary =
    either (error "arbitrary @Domain") id . mkDomain
      <$> elements
        [ "example.com",
          "beispiel.com"
          -- unicode domains are not supported, sadly:
          -- "例.com",
          -- "مثال.com",
          -- "dæmi.com"
        ]
