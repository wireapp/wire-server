{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Handle where

import Control.Applicative (optional)
import Data.Aeson hiding ((<?>))
import Data.Attoparsec.Text
import Data.ByteString.Conversion
import Data.Hashable (Hashable)
import qualified Data.Text as Text
import Imports
import Test.QuickCheck (Arbitrary (arbitrary), choose, elements)

--------------------------------------------------------------------------------
-- Handle

-- | Also called username.
newtype Handle
  = Handle
      {fromHandle :: Text}
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, ToByteString, Hashable)

instance FromByteString Handle where
  parser = parser >>= maybe (fail "Invalid handle") return . parseHandle

instance FromJSON Handle where
  parseJSON =
    withText "Handle" $
      maybe (fail "Invalid handle") pure . parseHandle

parseHandle :: Text -> Maybe Handle
parseHandle t
  | isValidHandle t = Just (Handle t)
  | otherwise = Nothing

isValidHandle :: Text -> Bool
isValidHandle t =
  either (const False) (const True) $
    parseOnly handle t
  where
    handle =
      count 2 (satisfy chars)
        *> count 254 (optional (satisfy chars))
        *> endOfInput
    -- NOTE: Ensure that characters such as `@` and `+` should _NOT_
    -- be used so that "phone numbers", "emails", and "handles" remain
    -- disjoint sets.
    -- The rationale behind max size here relates to the max length of
    -- an email address as defined here:
    -- http://www.rfc-editor.org/errata_search.php?rfc=3696&eid=1690
    -- with the intent that in the enterprise world handle =~ email address
    chars = inClass "a-z0-9_.-"

instance Arbitrary Handle where
  arbitrary = Handle . Text.pack <$> do
    let many n = replicateM n (elements $ ['a' .. 'z'] <> ['0' .. '9'] <> ['_'] <> ['-'] <> ['.'])
    ((<>) <$> many 2 <*> (many =<< choose (0, 254)))
