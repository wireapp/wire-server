{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Data.Handle
  ( Handle (fromHandle),
    parseHandle,
    parseHandleEither,
    BadHandle (..),
  )
where

import Cassandra qualified as C
import Control.Lens (ix, (.~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString qualified as BS
import Data.ByteString.Conversion (FromByteString (parser), ToByteString)
import Data.Hashable (Hashable)
import Data.OpenApi qualified as S
import Data.Schema
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.E
import Imports
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Test.QuickCheck (Arbitrary (arbitrary), choose, elements, oneof)
import Util.Attoparsec (takeUpToWhile)

--------------------------------------------------------------------------------
-- Handle

-- | Also called username.
newtype Handle = Handle
  {fromHandle :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToByteString, Hashable, S.ToParamSchema)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema Handle

deriving instance C.Cql Handle

instance ToSchema Handle where
  schema = fromHandle .= parsedText "Handle" p
    where
      p = first ("Invalid handle: " <>) . parseHandleEither

instance FromHttpApiData Handle where
  parseUrlPiece =
    first Text.pack . parseHandleEither

instance ToHttpApiData Handle where
  toUrlPiece (Handle h) = toUrlPiece h

instance FromByteString Handle where
  parser = handleParser

parseHandle :: Text -> Maybe Handle
parseHandle = either (const Nothing) Just . parseHandleEither

parseHandleEither :: Text -> Either String Handle
parseHandleEither = Atto.parseOnly (handleParser <* Atto.endOfInput) . Text.E.encodeUtf8

handleParser :: Atto.Parser Handle
handleParser = do
  bs <- takeUpToWhile 256 isHandleChar
  when (BS.length bs < 2) $ do
    nextChar <- maybe "end of input" show <$> Atto.peekChar
    fail $ "not enough valid characters for handle before " <> nextChar
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
    isHandleChar = Atto.inClass "a-z0-9_.-"

instance Arbitrary Handle where
  arbitrary =
    Handle . Text.pack <$> do
      len <- oneof [choose (2, 10), choose (2, 256)] -- prefer short handles
      replicateM len (elements $ ['a' .. 'z'] <> ['0' .. '9'] <> "_-.")

-- | for testing
newtype BadHandle = BadHandle {fromBadHandle :: Text}
  deriving newtype (Eq, Show)

instance Arbitrary BadHandle where
  arbitrary = oneof [tooShort, tooLong, badBytes]
    where
      tooShort = (BadHandle . Text.pack . (: [])) <$> elements validChar
      tooLong = (BadHandle . Text.pack) <$> replicateM 258 (elements validChar)
      badBytes =
        BadHandle <$> do
          totalLen :: Int <- choose (2, 256)
          invalidCharPos :: Int <- choose (0, totalLen - 1)
          invalidCharContent <- elements invalidChar
          good :: Text <- Text.pack <$> replicateM totalLen (elements validChar)
          let bad :: Text = good & ix invalidCharPos .~ invalidCharContent
          pure bad

      validChar :: [Char] = ['a' .. 'z'] <> ['0' .. '9'] <> "_-."
      invalidChar :: [Char] = [minBound ..] \\ validChar
