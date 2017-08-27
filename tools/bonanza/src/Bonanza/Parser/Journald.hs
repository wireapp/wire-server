{-# LANGUAGE RecordWildCards #-}

module Bonanza.Parser.Journald
    ( JournaldLogRecord (..)
    , journaldLogRecordWith
    , jdTimestamp
    )
where

import Bonanza.Parser.Internal
import Bonanza.Parser.Time
import Bonanza.Types
import Control.Applicative
import Control.Lens                     ((&), (.~))
import Data.Attoparsec.ByteString.Char8
import Data.Monoid
import Data.Text                        (Text)
import Data.Time                        (UTCTime (..))

-- <timestamp> <syslog_identifier>[<pid>]: <... message ...>

data JournaldLogRecord a = JournaldLogRecord
    { jdTime    :: !(Maybe UTCTime)
    , jdProcess :: Text
    , jdPid     :: Int
    , jdMessage :: !a
    } deriving (Eq, Show)

instance ToLogEvent a => ToLogEvent (JournaldLogRecord a) where
    toLogEvent JournaldLogRecord{..} =
           (mempty & logTime .~ jdTime)
        <> toLogEvent jdMessage

journaldLogRecordWith :: Parser a -> Parser (JournaldLogRecord a)
journaldLogRecordWith p = JournaldLogRecord
    <$> optional (skipSpace *> jdTimestamp <* skipSpace)
    <*> (toText <$> takeTill (== '['))
    <*> (char '[' *> decimal <* char ']' <* char ':' <* skipSpace)
    <*> p

jdTimestamp :: Parser UTCTime
jdTimestamp = tai <|> iso8601UTC <|> tt
  where
    tai = char '@' *> tai64N
    tt  = UTCTime <$> isoDay <* char '_' <*> isoTime
