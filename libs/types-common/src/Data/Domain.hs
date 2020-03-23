module Data.Domain where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.Char8
import Data.ByteString.Conversion
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.E
import Imports
import Test.QuickCheck (Arbitrary (arbitrary))
import qualified Test.QuickCheck as QC

-- | FUTUREWORK: move this type upstream into the email-validate package?
-- or become independent of email validation.
newtype Domain
  = Domain {_domainText :: Text}
  deriving (Eq, Generic, Show)

domainText :: Domain -> Text
domainText = _domainText

mkDomain :: Text -> Either String Domain
mkDomain = Atto.parseOnly domainParser . Text.E.encodeUtf8

instance FromByteString Domain where
  parser = domainParser

domainParser :: Atto.Parser Domain
domainParser = do
  parts <- domainLabel `Atto.sepBy1` Atto.char '.'
  let bs = BS.intercalate "." parts
  when (BS.length bs > 253) $
    fail "Invalid domain: too long"
  case Text.E.decodeUtf8' bs of
    Left err -> fail $ "Invalid UTF-8 in Domain: " <> show err
    Right txt -> pure $ Domain txt
  where
    domainLabel :: Atto.Parser ByteString
    domainLabel = do
      match <- matching (Atto.satisfy alphaNum *> Atto.skipWhile alphaNumHyphen)
      when (BS.length match > 63 || BS.Char8.last match == '-') $
        fail "Invalid domain label"
      pure match
    matching = fmap fst . Atto.match
    -- TODO: we don't accept capital letters here.
    -- we probably should, but need to normalize them?
    alphaNum = Atto.inClass "a-z0-9"
    alphaNumHyphen = Atto.inClass "a-z0-9-"

instance ToJSON Domain where
  toJSON = Aeson.String . domainText

instance FromJSON Domain where
  parseJSON = Aeson.withText "Domain" $ either fail pure . mkDomain

instance Arbitrary Domain where
  arbitrary =
    either (error . ("arbitrary @Domain: " <>)) id . mkDomain . Text.intercalate "."
      <$> count 1 4 arbitraryDomainLabel
    where
      arbitraryDomainLabel = do
        a <- alphaNum
        b <- Text.pack <$> count 0 61 alphaNumHypen
        c <- alphaNum
        pure (a `Text.cons` b `Text.snoc` c)
      -- unicode domains are not supported, sadly:
      -- "例.com",
      -- "مثال.com",
      -- "dæmi.com"
      alphaNum =
        QC.elements $ ['a' .. 'z'] <> ['0' .. '9']
      alphaNumHypen =
        QC.elements $ ['a' .. 'z'] <> ['0' .. '9'] <> "-"
      count x y gen = do
        n <- QC.choose (x, y)
        replicateM n gen
