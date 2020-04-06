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
