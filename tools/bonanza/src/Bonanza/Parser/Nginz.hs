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
