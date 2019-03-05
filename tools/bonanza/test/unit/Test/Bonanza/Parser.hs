{-# LANGUAGE OverloadedStrings #-}

module Test.Bonanza.Parser (tests) where

import Imports
import Bonanza.Parser.Internal
import Bonanza.Parser.Netstrings
import Bonanza.Parser.Nginz
import Bonanza.Parser.Socklog
import Bonanza.Parser.Svlogd
import Bonanza.Parser.Tinylog
import Control.Arrow                    ((***))
import Data.Attoparsec.ByteString.Char8 hiding (digit)
import Test.Bonanza.Arbitrary
import Test.Tasty
import Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "Parsing"
    [ testGroup "Record Formats"
      [ testProperty "tinylog"                tiny
      , testProperty "svlogd"                 svlogd
      , testProperty "svlogd+tinylog"         svTiny
      , testProperty "nginz"                  nginz
      , testProperty "socklog+svlogd"         sockSv
      , testProperty "socklog+svlogd+tinylog" sockSvTiny
      ]

    , testGroup "Netstrings"
      [ testProperty "tagged"                 taggedNetstr
      , testProperty "tinylog"                tinyNetstr
      , testProperty "svlogd+tinylog"         svTinyNetstr
      , testProperty "socklog+svlogd+tinylog" sockSvTinyNetstr
      ]
    , testProperty "escaping" escaping
    ]

tiny :: ParseInput TinyLogRecord -> Property
tiny (ParseInput (rec, bs)) =
        parseOnly tinyLogRecord bs
    === Right rec

svlogd :: ParseInput (SvLogRecord Text) -> Property
svlogd (ParseInput (rec, bs)) =
        fmap secs (parseOnly svLogRecord bs)
    === Right (secs rec)

svTiny :: ParseInput (SvLogRecord TinyLogRecord) -> Property
svTiny (ParseInput (rec, bs)) =
        fmap secs (parseOnly (svLogRecordWith tinyLogRecord) bs)
    === Right (secs rec)

nginz :: ParseInput NginzLogRecord -> Property
nginz (ParseInput (rec, bs)) =
        fmap secs (parseOnly nginzLogRecord bs)
    === Right (secs rec)

sockSv :: ParseInput (SockLogRecord (SvLogRecord Text)) -> Property
sockSv (ParseInput (rec, bs)) =
        fmap secs (parseOnly (sockLogRecordWith svLogRecord) bs)
    === Right (secs rec)

sockSvTiny :: ParseInput (SockLogRecord (SvLogRecord TinyLogRecord)) -> Property
sockSvTiny (ParseInput (rec, bs)) =
        fmap secs (parseOnly (sockLogRecordWith (svLogRecordWith tinyLogRecord)) bs)
    === Right (secs rec)

escaping :: Utf8 -> Property
escaping (Utf8 t) = counterexample ("quoted: " ++ show (quote t)) $
        (unquote . quote $ t)
    === Right t

--------------------------------------------------------------------------------
-- Netstrings

taggedNetstr :: ParseInput TaggedNetstring -> Property
taggedNetstr (ParseInput (tn, bs)) =
        parseOnly (tagged '=') bs
    === Right (unwrap tn)
  where
    unwrap = map (fmap taggedValue *** taggedValue) . taggedNetstring

tinyNetstr :: ParseInput TinylogNetstr -> Property
tinyNetstr (ParseInput (TinylogNetstr rec, bs)) = tiny (ParseInput (rec, bs))

svTinyNetstr :: ParseInput (SvLogRecord TinylogNetstr) -> Property
svTinyNetstr (ParseInput (rec, bs)) =
        fmap secs (parseOnly (svLogRecordWith tinyLogRecord) bs)
    === Right (secs (unwrap rec))
  where
    unwrap r = r { svMessage = tinylogNetstr (svMessage r) }

sockSvTinyNetstr :: ParseInput (SockLogRecord (SvLogRecord TinylogNetstr)) -> Property
sockSvTinyNetstr (ParseInput (rec, bs)) =
        fmap secs (parseOnly (sockLogRecordWith (svLogRecordWith tinyLogRecord)) bs)
    === Right (secs (unwrap rec))
  where
    unwrap  r = r { sockMessage = unwrap' (sockMessage r) }
    unwrap' r = r { svMessage   = tinylogNetstr (svMessage r) }
