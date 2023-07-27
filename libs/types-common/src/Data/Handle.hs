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
  ( Handle (..),
    parseHandle,
    parseHandleEither,
    isValidHandle,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString qualified as BS
import Data.ByteString.Conversion (FromByteString (parser), ToByteString)
import Data.Hashable (Hashable)
import Data.Schema
import Data.Swagger qualified as S
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

isValidHandle :: Text -> Bool
isValidHandle = isRight . parseHandleEither

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
