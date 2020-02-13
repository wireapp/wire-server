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
