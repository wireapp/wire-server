{-# LANGUAGE OverloadedStrings #-}

module Test.Bonanza.Streaming (tests) where

import           Bonanza.Parser.Nginz
import           Bonanza.Parser.Socklog
import           Bonanza.Parser.Svlogd
import           Bonanza.Parser.Tinylog
import qualified Bonanza.Streaming.Parser         as P
import           Bonanza.Types
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as B
import qualified Data.ByteString.Lazy             as BL
import           Data.Conduit                     (runConduit, (.|))
import qualified Data.Conduit.Binary              as Conduit
import qualified Data.Conduit.List                as Conduit
import           Data.Text                        (Text)
import           Test.Bonanza.Arbitrary
import           Test.Tasty
import           Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "Streaming"
    [ localOption (QuickCheckMaxSize 50) $ testGroup "Parsers"
      [ testGroup "Record Formats"
        [ testProperty "tinylog"                tiny
        , testProperty "svlogd"                 svlogd
        , testProperty "svlogd+tinylog"         svTiny
        , testProperty "nginz"                  nginz
        , testProperty "socklog+svlogd"         sockSv
        , testProperty "socklog+svlogd+tinylog" sockSvTiny
        ]

      , testGroup "Netstrings"
        [ testProperty "tinylog"                tinyNetstr
        , testProperty "svlogd+tinylog"         svTinyNetstr
        , testProperty "socklog+svlogd+tinylog" sockSvTinyNetstr
        ]
      ]
    ]

tiny :: [ParseInput TinyLogRecord] -> Property
tiny = run_prop tinyLogRecord

svlogd :: [ParseInput (SvLogRecord Text)] -> Property
svlogd = run_prop svLogRecord

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
    unwrap = ParseInput
           . (\ (r, i) -> (r { svMessage = tinylogNetstr (svMessage r) }, i))
           . parseInput

sockSvTinyNetstr :: [ParseInput (SockLogRecord (SvLogRecord TinylogNetstr))] -> Property
sockSvTinyNetstr = sockSvTiny . map unwrap
  where
    unwrap = ParseInput
           . (\ (r, i) -> (r { sockMessage = unwrap' (sockMessage r) }, i))
           . parseInput

    unwrap' r = r { svMessage = tinylogNetstr (svMessage r) }


run_prop :: (SecondsPrecision a, ToLogEvent a)
         => Parser a
         -> [ParseInput a]
         -> Property
run_prop p i = ioProperty $
        runConduit $ Conduit.sourceLbs inp
            .| P.stream (P.MkParser p)
            .| Conduit.consume
    >>= pure . (=== out) . map secs
  where
    inp = BL.fromStrict . B.intercalate "\n" $ map (snd . parseInput) i
    out = map (secs . toLogEvent . fst . parseInput) i
