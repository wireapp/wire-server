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

{-# LANGUAGE OverloadedStrings #-}

module Bonanza.Parser.Nginz
  ( NginzLogRecord (..),
    nginzFields,
    nginzLogRecord,
    module CommonLog,
  )
where

import Bonanza.Parser.CommonLog as CommonLog
import Bonanza.Types
import Data.Attoparsec.ByteString.Char8
import Imports

newtype NginzLogRecord = NginzLogRecord {fromNginzLogRecord :: CommonLogRecord}
  deriving (Eq, Show)

instance ToLogEvent NginzLogRecord where
  toLogEvent = toLogEvent . fromNginzLogRecord

nginzFields :: [Text]
nginzFields = commonLogFields ++ map fst fieldParsers

fieldParsers :: [(Text, Parser CommonLogField)]
fieldParsers =
  [ ("http_referer", stringField),
    ("http_user_agent", stringField),
    ("http_x_forwarded_for", ipv4Field),
    ("separator", emptyField),
    ("connection", intField),
    ("request_time", doubleField),
    ("upstream_response_time", doubleField),
    ("upstream_cache_status", stringField),
    ("user", stringField),
    ("zconn", stringField),
    ("request", stringField),
    ("proxy_protocol_addr", ipv4Field)
  ]

nginzLogRecord :: Parser NginzLogRecord
nginzLogRecord = NginzLogRecord <$> commonLogRecord fieldParsers
