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

module Bonanza.Parser.Svlogd
  ( SvLogRecord (..),
    svLogRecord,
    svLogRecordWith,
    svTags',
    svTimestamp,
  )
where

import Bonanza.Parser.Internal
import Bonanza.Parser.Time
import Bonanza.Types
import Control.Applicative (optional)
import Control.Lens ((.~))
import Data.Aeson
import Data.Attoparsec.ByteString.Char8
import Data.Bifunctor
import Data.HashMap.Strict (fromList)
import Data.Text (strip)
import Data.Time (UTCTime (..))
import Imports

data SvLogRecord a
  = SvLogRecord
      { svTime :: !(Maybe UTCTime),
        svTags :: [(Text, Text)],
        svMessage :: !a
      }
  deriving (Eq, Show)

instance ToLogEvent a => ToLogEvent (SvLogRecord a) where
  toLogEvent SvLogRecord {..} =
    (mempty & logTime .~ svTime & logTags .~ tgs)
      <> toLogEvent svMessage
    where
      tgs = Tags . fromList . map (second String) $ svTags

svLogRecord :: Parser (SvLogRecord Text)
svLogRecord = svLogRecordWith $ strip . toText <$> takeTill (== '\n')

svLogRecordWith :: Parser a -> Parser (SvLogRecord a)
svLogRecordWith p =
  SvLogRecord
    <$> optional svTimestamp <* skipSpace
    <*> option [] (try svTags')
    <*> p

svTimestamp :: Parser UTCTime
svTimestamp = tai <|> iso8601UTC <|> tt
  where
    tai = char '@' *> tai64N
    tt = UTCTime <$> isoDay <* char '_' <*> isoTime

svTags' :: Parser [(Text, Text)]
svTags' = char delim *> (pair `sepBy` char ',') <* char delim <* skipSpace
  where
    delim = '|'
    pair =
      (,)
        <$> (toText <$> takeTill (== '=')) <* char '='
        <*> (toText <$> takeTill (\c -> c == ',' || c == delim))
