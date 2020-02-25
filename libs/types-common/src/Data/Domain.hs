module Data.Domain where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.ByteString as Atto
import Data.Bifunctor (bimap)
import Data.ByteString.Conversion
import Data.String.Conversions (cs)
import qualified Data.Text.Encoding as Text.E
import Imports
import Test.QuickCheck (Arbitrary (arbitrary), elements)
import qualified Text.Email.Validate as Email

-- | FUTUREWORK: move this type upstream into the email-validate package?
-- or become independent of email validation.
newtype Domain
  = Domain {_domainText :: Text}
  deriving (Eq, Generic, Show)

domainText :: Domain -> Text
domainText = _domainText

mkDomain :: ByteString -> Either String Domain
mkDomain = bimap show Domain . Text.E.decodeUtf8' <=< validateDomain
  where
    -- this is a slightly hacky way of validating a domain,
    -- but Text.Email.Validate doesn't expose the parser for the domain.
    validateDomain = fmap Email.domainPart . Email.validate . ("local-part@" <>)

instance FromByteString Domain where
  parser = do
    bs <- Atto.takeByteString
    case mkDomain bs of
      Left err -> fail ("Failed parsing ByteString as Domain: " <> err)
      Right domain -> pure domain

instance ToJSON Domain where
  toJSON = Aeson.String . domainText

instance FromJSON Domain where
  parseJSON = Aeson.withText "Domain" $ either fail pure . mkDomain . cs

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
