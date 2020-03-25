{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Handle
  ( Handle (..),
    parseHandle,
    parseHandleEither,
    isValidHandle,
  )
where

import Data.Aeson hiding ((<?>))
import Data.Attoparsec.ByteString ((<?>))
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString as BS
import Data.ByteString.Conversion (FromByteString (parser), ToByteString)
import Data.Hashable (Hashable)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.E
import Imports
import Test.QuickCheck (Arbitrary (arbitrary), choose, elements, oneof)

--------------------------------------------------------------------------------
-- Handle

-- | Also called username.
newtype Handle
  = Handle
      {fromHandle :: Text}
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, ToByteString, Hashable)

parseHandle :: Text -> Maybe Handle
parseHandle = either (const Nothing) Just . parseHandleEither

parseHandleEither :: Text -> Either String Handle
parseHandleEither = Atto.parseOnly (handleParser <* Atto.endOfInput) . Text.E.encodeUtf8

isValidHandle :: Text -> Bool
isValidHandle = isRight . parseHandleEither

instance FromJSON Handle where
  parseJSON =
    withText "Handle" $
      maybe (fail "Invalid handle") pure . parseHandle

instance FromByteString Handle where
  parser = handleParser

handleParser :: Atto.Parser Handle
handleParser = do
  bs <- matching (Atto.count 2 handleChar *> Atto.skipMany handleChar)
  when (BS.length bs > 256) $
    fail "handle too long"
  case Text.E.decodeUtf8' bs of
    Left err -> fail $ "handle contains invalid UTF-8: " <> show err
    Right txt -> pure (Handle txt)
  where
    -- NOTE: Ensure that characters such as `@` and `+` should _NOT_
    -- be used so that "phone numbers", "emails", and "handles" remain
    -- disjoint sets.
    -- The rationale behind max size here relates to the max length of
    -- an email address as defined here:
    -- http://www.rfc-editor.org/errata_search.php?rfc=3696&eid=1690
    -- with the intent that in the enterprise world handle =~ email address
    handleChar = Atto.satisfy isHandleChar <?> "valid handle character"
    isHandleChar = Atto.inClass "a-z0-9_.-"
    matching = fmap fst . Atto.match

instance Arbitrary Handle where
  arbitrary = Handle . Text.pack <$> do
    len <- oneof [choose (2, 10), choose (2, 256)] -- prefer short handles
    replicateM len (elements $ ['a' .. 'z'] <> ['0' .. '9'] <> "_-.")
