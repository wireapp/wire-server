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

{-# LANGUAGE RecordWildCards #-}

module Bonanza.Parser.Journald
  ( JournaldLogRecord (..),
    journaldLogRecordWith,
  )
where

import Bonanza.Parser.Internal
import Bonanza.Parser.Svlogd
import Bonanza.Types
import Control.Applicative (optional)
import Control.Lens ((.~))
import Data.Attoparsec.ByteString.Char8
import Data.Time (UTCTime (..))
import Imports

-- <timestamp> <syslog_identifier>[<pid>]: <... message ...>

data JournaldLogRecord a
  = JournaldLogRecord
      { jdTime :: !(Maybe UTCTime),
        jdProcess :: Text,
        jdPid :: Int,
        jdMessage :: !a
      }
  deriving (Eq, Show)

instance ToLogEvent a => ToLogEvent (JournaldLogRecord a) where
  toLogEvent JournaldLogRecord {..} =
    (mempty & logTime .~ jdTime)
      <> toLogEvent jdMessage

journaldLogRecordWith :: Parser a -> Parser (JournaldLogRecord a)
journaldLogRecordWith p =
  JournaldLogRecord
    <$> optional (skipSpace *> svTimestamp <* skipSpace)
    <*> (toText <$> takeTill (== '['))
    <*> (char '[' *> decimal <* char ']' <* char ':' <* skipSpace)
    <*> p
