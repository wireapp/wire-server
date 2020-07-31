{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

module Bonanza.Parser.Tinylog
  ( TinyLogRecord (..),
    tinyLogRecord,
    tinyLevel,
    tinyFields,
  )
where

import Bonanza.Parser.Internal
import Bonanza.Parser.Netstrings
import Bonanza.Types
import Control.Applicative (optional)
import Control.Lens.Operators
import Data.Aeson
import Data.Attoparsec.ByteString.Char8
import Data.Bifunctor
import qualified Data.ByteString.Char8 as B
import Data.HashMap.Strict (fromList)
import qualified Data.Text as T
import Imports hiding (isDigit)

data TinyLogRecord = TinyLogRecord
  { tDate :: !(Maybe Text),
    tLevel :: !Char,
    tFields :: [(Text, Text)],
    tMessage :: !Text
  }
  deriving (Eq, Show)

instance ToLogEvent TinyLogRecord where
  toLogEvent TinyLogRecord {..} =
    mempty & logTags .~ tgs & logMessage ?~ tMessage
    where
      tgs =
        Tags . fromList . map (second String) $
          ("level", T.singleton tLevel) :
          tFields
            ++ maybeToList ((,) "time" <$> tDate)

tinyLogRecord :: Parser TinyLogRecord
tinyLogRecord = tinyLogRecordNetstr <|> tinyLogRecordLegacy <|> tinyLogCatchAll

-- E.g. for slightly more "graceful" handling of newline-interrupted messages,
-- so that they are at least assigned to the right bucket with the right timestamp.
tinyLogCatchAll :: Parser TinyLogRecord
tinyLogCatchAll = do
  ms <- T.strip . toText <$> takeTill (== '\n')
  return
    TinyLogRecord
      { tDate = Nothing,
        tLevel = 'T',
        tFields = [],
        tMessage = ms
      }

tinyLogRecordLegacy :: Parser TinyLogRecord
tinyLogRecordLegacy = do
  dt <- optional date
  lv <- tinyLevel
  fs <- tinyFields <* (endOfLine <|> endOfInput)
  return
    TinyLogRecord
      { tDate = dt,
        tLevel = lv,
        tFields = filterFields fs,
        tMessage = T.intercalate ", " $ filterMessage fs
      }

tinyLogRecordNetstr :: Parser TinyLogRecord
tinyLogRecordNetstr = do
  dt <- optional dateNetstr
  lv <- tinyLevelNetstr
  fs <- tinyFieldsNetstr <* (endOfLine <|> endOfInput)
  return
    TinyLogRecord
      { tDate = dt,
        tLevel = lv,
        tFields = filterFields fs,
        tMessage = T.intercalate ", " $ filterMessage fs
      }

tinyLevel :: Parser Char
tinyLevel = choice (map char "TDIWEF") <* char ',' <* skipSpace

tinyLevelNetstr :: Parser Char
tinyLevelNetstr = do
  str <- netstring
  case str of
    x | B.length str == 1 -> case B.head x of
      c | c `elem` levels -> pure c
      c -> fail $ "Unknown level: " ++ show c
    x -> fail $ "Level too long: " ++ show x
  where
    levels :: String
    levels = "TDIWEF"

tinyFields :: Parser [(Maybe Text, Text)]
tinyFields = pair `sepBy` char sep
  where
    pair :: Parser (Maybe Text, Text)
    pair = do
      _ <- skipWhile (== ' ')
      q <- peekChar
      case q of
        Just x | x == '"' -> (,) Nothing <$> quoted' <* skipToSepOrEnd
        _ -> do
          k <- optional $ takeWhile1 (\c -> c /= '\n' && c /= '=') <* char '='
          q' <- peekChar
          let tup = (,) ((T.strip . toText) <$> k)
          tup <$> case q' of
            Just y | y == '"' -> quoted' <* skipToSepOrEnd
            _ -> unquoted
    quoted' = do
      q <- quoted
      either fail pure . unquote . mconcat $ ["\"", q, "\""]
    unquoted = T.strip . toText <$> takeTill (\c -> c == sep || c == '\n')
    sep = ','
    skipToSepOrEnd = skipWhile (\c -> c /= sep && c /= '\n')

tinyFieldsNetstr :: Parser [(Maybe Text, Text)]
tinyFieldsNetstr = map (bimap (fmap toText) toText) <$> tagged '='

--------------------------------------------------------------------------------
-- Internal

filterFields :: [(Maybe Text, Text)] -> [(Text, Text)]
filterFields = mapMaybe (\(k, v) -> flip (,) v `fmap` k)
{-# INLINEABLE filterFields #-}

filterMessage :: [(Maybe Text, Text)] -> [Text]
filterMessage = mapMaybe (\(k, v) -> maybe (Just v) (const Nothing) k)
{-# INLINEABLE filterMessage #-}

date :: Parser Text
date = peekChar >>= go
  where
    go Nothing = fail "date, eof"
    go (Just c)
      | isDigit c = (toText <$> takeTill (== ',')) <* char ',' <* skipSpace
      | otherwise = fail "date"

dateNetstr :: Parser Text
dateNetstr = netstring >>= go
  where
    go x
      | B.length x > 0 && isDigit (B.head x) = pure $ toText x
      | otherwise = fail "date netstr"
