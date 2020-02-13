{-# LANGUAGE RecordWildCards #-}

module Bonanza.Parser.Rkt
  ( RktLogRecord (..),
    rktLogRecord,
  )
where

import Bonanza.Parser.Internal
import Bonanza.Parser.Svlogd
import Bonanza.Types
import Control.Lens.Operators
import Data.Aeson
import Data.Attoparsec.ByteString.Char8
import Data.Bifunctor
import Data.HashMap.Strict (fromList)
import Data.Text (strip)
import Imports

-- [<uptime_in_seconds>] <service_name>[<id>]: <opt_tags><message>

data RktLogRecord
  = RktLogRecord
      { rktUptime :: !Double,
        rktService :: !Text,
        rktTags :: [(Text, Text)],
        rktMessage :: !Text
      }
  deriving (Eq, Show)

instance ToLogEvent RktLogRecord where
  toLogEvent RktLogRecord {..} =
    mempty & logTags .~ tgs
      & logMessage ?~ rktMessage
    where
      tgs = Tags . fromList . map (second String) $ rktTags

rktLogRecord :: Parser RktLogRecord
rktLogRecord = do
  up <- char '[' *> skipSpace *> double <* char ']' <* skipSpace
  srv <- (toText <$> takeTill (== '[')) <* char '[' <* scientific <* char ']' <* char ':' <* skipSpace
  tags <- option [] (try svTags')
  msg <- strip . toText <$> takeTill (== '\n')
  return $ RktLogRecord up srv tags msg
