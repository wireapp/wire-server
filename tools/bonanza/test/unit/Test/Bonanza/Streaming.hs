{-# LANGUAGE OverloadedStrings #-}

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

module Test.Bonanza.Streaming
  ( tests,
  )
where

import Bonanza.Parser.Nginz
import Bonanza.Parser.Socklog
import Bonanza.Parser.Svlogd
import Bonanza.Parser.Tinylog
import qualified Bonanza.Streaming.Parser as P
import Bonanza.Types
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.Binary as Conduit
import qualified Data.Conduit.List as Conduit
import Imports
import Test.Bonanza.Arbitrary
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Streaming"
    [ localOption (QuickCheckMaxSize 50) $
        testGroup
          "Parsers"
          [ testGroup
              "Record Formats"
              [ testProperty "tinylog" tiny,
                -- testProperty "svlogd" svlogd, -- TODO: https://github.com/zinfra/backend-issues/issues/777#issuecomment-614820431
                testProperty "svlogd+tinylog" svTiny,
                testProperty "nginz" nginz,
                testProperty "socklog+svlogd" sockSv,
                testProperty "socklog+svlogd+tinylog" sockSvTiny
              ],
            testGroup
              "Netstrings"
              [ testProperty "tinylog" tinyNetstr,
                testProperty "svlogd+tinylog" svTinyNetstr,
                testProperty "socklog+svlogd+tinylog" sockSvTinyNetstr
              ]
          ]
    ]

tiny :: [ParseInput TinyLogRecord] -> Property
tiny = run_prop tinyLogRecord

_svlogd :: [ParseInput (SvLogRecord Text)] -> Property
_svlogd = run_prop svLogRecord

svTiny :: [ParseInput (SvLogRecord TinyLogRecord)] -> Property
svTiny = run_prop (svLogRecordWith tinyLogRecord)

nginz :: [ParseInput NginzLogRecord] -> Property
nginz = run_prop nginzLogRecord

sockSv :: [ParseInput (SockLogRecord (SvLogRecord Text))] -> Property
sockSv = run_prop (sockLogRecordWith svLogRecord)

sockSvTiny :: [ParseInput (SockLogRecord (SvLogRecord TinyLogRecord))] -> Property
sockSvTiny = run_prop (sockLogRecordWith (svLogRecordWith tinyLogRecord))

tinyNetstr :: [ParseInput TinylogNetstr] -> Property
tinyNetstr = tiny . map unwrap
  where
    unwrap (ParseInput (TinylogNetstr rec, bs)) = ParseInput (rec, bs)

svTinyNetstr :: [ParseInput (SvLogRecord TinylogNetstr)] -> Property
svTinyNetstr = svTiny . map unwrap
  where
    unwrap =
      ParseInput
        . (\(r, i) -> (r {svMessage = tinylogNetstr (svMessage r)}, i))
        . parseInput

sockSvTinyNetstr :: [ParseInput (SockLogRecord (SvLogRecord TinylogNetstr))] -> Property
sockSvTinyNetstr = sockSvTiny . map unwrap
  where
    unwrap =
      ParseInput
        . (\(r, i) -> (r {sockMessage = unwrap' (sockMessage r)}, i))
        . parseInput
    unwrap' r = r {svMessage = tinylogNetstr (svMessage r)}

run_prop ::
  (SecondsPrecision a, ToLogEvent a) =>
  Parser a ->
  [ParseInput a] ->
  Property
run_prop p i =
  ioProperty $
    runConduit $
      Conduit.sourceLbs inp
        .| P.stream (P.MkParser p)
        .| Conduit.consume
        >>= pure . (=== out) . map secs
  where
    inp = BL.fromStrict . B.intercalate "\n" $ map (snd . parseInput) i
    out = map (secs . toLogEvent . fst . parseInput) i
