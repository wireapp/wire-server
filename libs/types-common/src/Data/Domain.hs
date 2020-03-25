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

-- | Following [RFC-1035](https://www.ietf.org/rfc/rfc1035.txt), Section 2.3.1,
-- except for:
-- * not allowing a space @" "@
-- * accepting digits as the first letter of labels (except for the last label)
-- * not only must labels be 63 characters or less, the whole domain be 255 characters or less
--
-- <domain> ::= <label> | <domain> "." <label>
-- <label> ::= <let-dig> [ [ <ldh-str> ] <let-dig> ]
-- <ldh-str> ::= <let-dig-hyp> | <let-dig-hyp> <ldh-str>
-- <let-dig-hyp> ::= <let-dig> | "-"
-- <let-dig> ::= <letter> | <digit>
-- <letter> ::= any one of the 52 alphabetic characters A through Z in
-- upper case and a through z in lower case
-- <digit> ::= any one of the ten digits 0 through 9
--
-- All letters will be lowercased when parsed.
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
    Right txt -> pure . Domain $ Text.toCaseFold txt
  where
    domainLabel :: Atto.Parser ByteString
    domainLabel = do
      match <- matching (Atto.satisfy alphaNum *> Atto.skipWhile alphaNumHyphen)
      when (BS.length match > 63 || BS.Char8.last match == '-') $
        fail "Invalid domain label"
      pure match
    matching = fmap fst . Atto.match
    alphaNum = Atto.inClass "A-Za-z0-9"
    alphaNumHyphen = Atto.inClass "A-Za-z0-9-"

instance ToJSON Domain where
  toJSON = Aeson.String . domainText

instance FromJSON Domain where
  parseJSON = Aeson.withText "Domain" $ either fail pure . mkDomain

instance Arbitrary Domain where
  arbitrary =
    either (error . ("arbitrary @Domain: " <>)) id . mkDomain . getDomainText <$> arbitrary

-- | only for QuickCheck
newtype DomainText
  = DomainText {getDomainText :: Text}
  deriving (Eq, Show)

instance Arbitrary DomainText where
  arbitrary = DomainText <$> domain
    where
      -- <domain> ::= <label> | <domain> "." <label>
      domain =
        QC.oneof
          [ label,
            conc [domain, pure ".", label]
          ]
          `QC.suchThat` ((<= 255) . Text.length)
      -- <label> ::= <let-dig> [ [ <ldh-str> ] <let-dig> ]
      label =
        conc [letDig, opt (conc [opt ldhStr, letDig])]
          `QC.suchThat` ((<= 63) . Text.length)
      -- <ldh-str> ::= <let-dig-hyp> | <let-dig-hyp> <ldh-str>
      ldhStr =
        QC.frequency
          [ (1, letDigHyp),
            (3, conc [letDigHyp, ldhStr]) -- to get longer labels
          ]
          `QC.suchThat` ((<= 61) . Text.length)
      -- <let-dig-hyp> ::= <let-dig> | "-"
      letDigHyp =
        QC.frequency
          [ (1, pure "-"),
            (5, letDig) -- fewer hyphens
          ]
      -- <let-dig> ::= <letter> | <digit>
      -- <letter> ::= any one of the 52 alphabetic characters A through Z in
      -- upper case and a through z in lower case
      -- <digit> ::= any one of the ten digits 0 through 9
      letDig =
        Text.singleton <$> QC.elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'])
      -- helpers
      conc :: [QC.Gen Text] -> QC.Gen Text
      conc = fmap Text.concat . sequenceA
      opt :: QC.Gen Text -> QC.Gen Text
      opt x =
        QC.frequency
          [ (1, pure ""),
            (5, x) -- to get longer labels
          ]
