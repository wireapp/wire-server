{-# LANGUAGE RecordWildCards #-}

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
