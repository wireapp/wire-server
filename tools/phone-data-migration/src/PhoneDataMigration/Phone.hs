
module PhoneDataMigration.Phone (Phone, parsePhone, toText) where

import Data.Attoparsec.Text (char, count, digit, endOfInput, parseOnly)
import qualified Data.Text as Text
import Imports
import Options.Applicative

newtype Phone = Phone {fromPhone :: Text}
  deriving stock (Eq, Ord, Show, Generic)

toText :: Phone -> Text
toText = fromPhone

-- | Parses a phone number in E.164 format with a mandatory leading '+'.
parsePhone :: Text -> Maybe Phone
parsePhone p =
  let canonicalPhone = Text.filter (not . isSpace) p
   in if isValidPhone canonicalPhone
        then Just $ Phone canonicalPhone
        else Nothing

-- | Checks whether a phone number is valid, i.e. it is in E.164 format
-- with a mandatory leading '+' followed by 10-15 digits.
isValidPhone :: Text -> Bool
isValidPhone = either (const False) (const True) . parseOnly e164
  where
    e164 = char '+' *> count 8 digit *> count 7 (optional digit) *> endOfInput
