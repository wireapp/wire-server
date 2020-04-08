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

module Data.Domain where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.Aeson as Aeson
import Data.Attoparsec.ByteString ((<?>))
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.Char8
import Data.ByteString.Conversion
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.E
import Imports hiding (isAlphaNum)
import Test.QuickCheck (Arbitrary (arbitrary))
import qualified Test.QuickCheck as QC
import Util.Attoparsec (takeUpToWhile)

-- | A Fully Qualified Domain Name (FQDN).
--
-- Following [RFC-1035](https://www.ietf.org/rfc/rfc1035.txt), Section 2.3.1,
-- except for:
-- * not allowing a space @" "@
-- * accepting digits as first letter of labels (except for the last label, not allowing IPs)
-- * requiring at least two labels
-- * not only must labels be 63 chars or less, the whole domain must be 253 chars or less
--
-- <domain> ::= <label> "." <tld> | <label> "." <domain>
-- <tld> ::= <letter> [ [ <ldh-str> ] <let-dig> ]
-- <label> ::= <let-dig> [ [ <ldh-str> ] <let-dig> ]
-- <ldh-str> ::= <let-dig-hyp> | <let-dig-hyp> <ldh-str>
-- <let-dig-hyp> ::= <let-dig> | "-"
-- <let-dig> ::= <letter> | <digit>
-- <letter> ::= any one of the 52 alphabetic characters A through Z in
-- upper case and a through z in lower case
-- <digit> ::= any one of the ten digits 0 through 9
--
-- The domain will be normalized to lowercase when parsed.
newtype Domain
  = Domain {_domainText :: Text}
  deriving (Eq, Generic, Show)

domainText :: Domain -> Text
domainText = _domainText

mkDomain :: Text -> Either String Domain
mkDomain = Atto.parseOnly (domainParser <* Atto.endOfInput) . Text.E.encodeUtf8

instance FromByteString Domain where
  parser = domainParser

domainParser :: Atto.Parser Domain
domainParser = do
  parts <- domainLabel `Atto.sepBy1` Atto.char '.'
  when (length parts < 2) $
    fail "Invalid domain name: cannot be dotless domain"
  when (isDigit (BS.Char8.head (last parts))) $
    fail "Invalid domain name: last label cannot start with digit"
  let bs = BS.intercalate "." parts
  when (BS.length bs > 253) $
    fail "Invalid domain name: too long"
  case Text.E.decodeUtf8' bs of
    Left err -> fail $ "Invalid UTF-8 in Domain: " <> show err
    Right txt -> pure . Domain $ Text.toCaseFold txt
  where
    domainLabel :: Atto.Parser ByteString
    domainLabel = do
      match <- BS.Char8.cons <$> alphaNum <*> takeUpToWhile 62 isAlphaNumHyphen
      when (BS.Char8.last match == '-') $
        fail "Invalid domain label: last character is a hyphen"
      pure match
    alphaNum = Atto.satisfy isAlphaNum <?> "alphanumeric character"
    isAlphaNum = Atto.inClass "A-Za-z0-9"
    isAlphaNumHyphen = Atto.inClass "A-Za-z0-9-"

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
      -- <domain> ::= <label> "." <tld> | <label> "." <domain>
      domain =
        QC.oneof
          [ conc [label, pure ".", tldLabel],
            conc [label, pure ".", domain]
          ]
          `QC.suchThat` ((<= 255) . Text.length)
      -- <tld> ::= <letter> [ [ <ldh-str> ] <let-dig> ]
      tldLabel =
        conc [letter, opt (conc [opt ldhStr, letDig])]
          `QC.suchThat` ((<= 63) . Text.length)
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
      letDig =
        QC.oneof [letter, digit]
      -- <letter> ::= any one of the 52 alphabetic characters A through Z in
      letter =
        Text.singleton <$> QC.elements (['a' .. 'z'] <> ['A' .. 'Z'])
      -- <digit> ::= any one of the ten digits 0 through 9
      digit =
        Text.singleton <$> QC.elements ['0' .. '9']
      -- upper case and a through z in lower case
      -- helpers
      conc :: [QC.Gen Text] -> QC.Gen Text
      conc = fmap Text.concat . sequenceA
      opt :: QC.Gen Text -> QC.Gen Text
      opt x =
        QC.frequency
          [ (1, pure ""),
            (5, x) -- to get longer labels
          ]
