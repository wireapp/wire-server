{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

module Test.Bonanza.Arbitrary where

import Bonanza.Parser.CommonLog
import Bonanza.Parser.IP (mkIPv4, showIPv4Text)
import Bonanza.Parser.Internal hiding (quoted)
import Bonanza.Parser.Nginz (NginzLogRecord (..))
import Bonanza.Parser.Socklog
import Bonanza.Parser.Svlogd (SvLogRecord (..))
import Bonanza.Parser.Tinylog (TinyLogRecord (..))
import Bonanza.Types
import Control.Arrow ((***))
import Control.Lens ((%~))
import Data.Aeson
import Data.Bits ((.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Scientific
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.Builder.Int as T
import Data.Text.Lazy.Builder.Scientific
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Vector as V
import Imports
import Network.HTTP.Types.Method
import Test.QuickCheck hiding ((.&.))

--------------------------------------------------------------------------------
-- Text

newtype Utf8 = Utf8 {utf8 :: Text}
  deriving (Eq, Show)

instance Arbitrary Utf8 where
  arbitrary = mkUtf8 <$> listOf1 (arbitrary `suchThat` isUtf8)

isUtf8 :: Char -> Bool
isUtf8 c = ord c .&. 0x1ff800 /= 0xd800

mkUtf8 :: String -> Utf8
mkUtf8 = Utf8 . T.pack

newtype Quoted = Quoted {quoted :: Utf8}
  deriving (Eq, Show)

instance Arbitrary Quoted where
  arbitrary = Quoted . Utf8 . quote . utf8 <$> arbitrary

newtype Unquoted = Unquoted {unquoted :: Utf8}
  deriving (Eq, Show)

instance Arbitrary Unquoted where
  arbitrary =
    Unquoted . mkUtf8
      <$> listOf1 (arbitrary `suchThat` safeChar)
    where
      safeChar c | isSpace c = False
      safeChar '"' = False
      safeChar '=' = False
      safeChar ',' = False
      safeChar '\\' = False
      safeChar '|' = False
      safeChar _ = True

newtype AlphaNumeric = AlphaNumeric {alnum :: Text}
  deriving (Eq, Show)

instance Arbitrary AlphaNumeric where
  arbitrary =
    AlphaNumeric . T.pack
      <$> listOf1 (arbitrary `suchThat` isalnum)
    where
      -- 'Data.Char.isAlphaNum' is a little too liberal
      isalnum c = isLetter c || isDigit c

newtype Digit = Digit {digit :: Text}

instance Arbitrary Digit where
  arbitrary =
    Digit . T.pack
      <$> listOf1 (arbitrary `suchThat` isDigit)

--------------------------------------------------------------------------------
-- ByteStrings

instance Arbitrary ByteString where
  arbitrary = B.pack <$> listOf1 arbitrary

--------------------------------------------------------------------------------
-- Dates

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay . getPositive <$> arbitrary

instance Arbitrary DiffTime where
  arbitrary = picosecondsToDiffTime . getPositive <$> arbitrary

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> arbitrary <*> arbitrary

newtype DateFormat = DateFormat {dateFormat :: UTCTime -> ByteString}

instance Show DateFormat where
  show _ = "<function> Date Format" -- quickcheck wants a Show instance

instance Arbitrary DateFormat where
  arbitrary = elements [fmtISO, fmtTT] -- todo: TAI

fmtISO :: DateFormat
fmtISO = fmt "%FT%T%QZ"

fmtTT :: DateFormat
fmtTT = fmt "%F_%T%Q"

fmtCL :: DateFormat
fmtCL = fmt "[%d/%b/%Y:%H:%M:%S %z]"

fmt :: String -> DateFormat
fmt p = DateFormat (BC.pack . formatTime defaultTimeLocale p)

--------------------------------------------------------------------------------
-- HTTP

instance Arbitrary StdMethod where
  arbitrary =
    elements
      [GET, POST, HEAD, PUT, DELETE, TRACE, CONNECT, OPTIONS, PATCH]

instance Arbitrary HttpRequest where
  arbitrary =
    HttpRequest
      <$> arbitrary
      <*> genHttpPath
      <*> oneof [pure Nothing, fmap Just genHttpQuery]

genHttpPath :: Gen Text
genHttpPath = T.pack . ('/' :) <$> listOf genUrlTok

genHttpQuery :: Gen Text
genHttpQuery = T.intercalate "&" <$> listOf1 (T.pack <$> listOf1 genUrlTok)

genUrlTok :: Gen Char
genUrlTok = arbitrary `suchThat` inRange
  where
    inRange c = c > chr 32 && c <= chr 127 && c /= '?' && c /= '"'

--------------------------------------------------------------------------------
-- Internet

newtype Octet = Octet {octet :: Word16}
  deriving (Eq, Show)

instance Arbitrary Octet where
  arbitrary = Octet <$> arbitrary `suchThat` (< 256)

instance Arbitrary IPv4 where
  arbitrary = mkIPv4 <$> octet' <*> octet' <*> octet' <*> octet'
    where
      octet' = octet <$> arbitrary

newtype Ec2Region = Ec2Region {ec2Region :: Text}
  deriving (Eq, Show)

instance Arbitrary Ec2Region where
  arbitrary =
    Ec2Region
      <$> elements
        [ "ap-northeast-1",
          "ap-southeast-1",
          "ap-southeast-2",
          "eu-west-1",
          "sa-east-1",
          "us-east-1",
          "us-west-1",
          "us-west-2"
        ]

newtype Ec2InternalHostname = Ec2InternalHostname {ec2InternalHostname :: Text}
  deriving (Eq, Show)

instance Arbitrary Ec2InternalHostname where
  arbitrary = do
    a <- octet <$> arbitrary
    b <- octet <$> arbitrary
    c <- octet <$> arbitrary
    d <- octet <$> arbitrary
    r <- arbitrary
    pure . Ec2InternalHostname . mconcat $
      [ "ip-",
        toStrict . toLazyText . mconcat . intersperse "-" $
          map T.decimal [a, b, d, c],
        ".",
        ec2Region r,
        ".compute.internal"
      ]

--------------------------------------------------------------------------------
-- Parser input

newtype ParseInput a = ParseInput {parseInput :: (a, ByteString)}
  deriving (Eq)

instance Show (ParseInput a) where
  show = show . snd . parseInput

newtype FieldValue = FieldValue {fieldValue :: Text}
  deriving (Eq, Show)

instance Arbitrary FieldValue where
  arbitrary =
    FieldValue
      <$> oneof
        [ utf8 . quoted <$> arbitrary,
          utf8 . unquoted <$> arbitrary
        ]

instance Arbitrary TagValue where
  arbitrary =
    oneof
      [ String . utf8 . unquoted <$> arbitrary,
        Number . fromIntegral <$> (arbitrary :: Gen Int),
        Number . realToFrac <$> (arbitrary :: Gen Double),
        Bool <$> arbitrary,
        Array . V.fromList <$> listOf (arbitrary :: Gen TagValue)
      ]

newtype Field = Field {field :: (AlphaNumeric, FieldValue)}
  deriving (Eq, Show)

instance Arbitrary Field where
  arbitrary = Field <$> ((,) <$> arbitrary <*> arbitrary)

instance Arbitrary CommonLogField where
  arbitrary =
    oneof
      [ pure CEmpty,
        CField <$> arbitrary
      ]

instance Arbitrary (ParseInput TinyLogRecord) where
  arbitrary = do
    date <- arbitrary
    df <- dateFormat <$> elements [fmtISO, fmtTT]
    level <- elements "TDIWEF"
    fields <- arbitrary
    message <- fieldValue <$> arbitrary
    let rec =
          TinyLogRecord
            { tDate = fmap (decodeUtf8 . df $) date,
              tLevel = level,
              tFields = map (alnum *** stripQuotes . fieldValue) fields,
              tMessage = stripQuotes message
            }
        inp =
          encodeUtf8 . mconcat $
            [ maybe "" (\d -> decodeUtf8 $ df d <> ", ") date,
              T.intercalate ", " $
                T.singleton level :
                map (\(k, v) -> alnum k <> "=" <> fieldValue v) fields
                  ++ [message]
            ]
    return $ ParseInput (rec, inp)
    where
      stripQuotes t = case T.strip t of
        t'
          | "\"" `T.isPrefixOf` t' -> either error id . unquote $ t'
          | otherwise -> t'

instance Arbitrary (ParseInput (SvLogRecord Text)) where
  arbitrary = do
    date <- arbitrary
    df <- elements [fmtISO, fmtTT]
    tags <- arbitrary
    message <- utf8 . unquoted <$> arbitrary
    let rec =
          SvLogRecord
            { svTime = date,
              svTags = map (alnum *** alnum) tags,
              svMessage = T.strip message
            }
        inp = mkSvInput df rec (encodeUtf8 message)
    return $ ParseInput (rec, inp)

instance Arbitrary (ParseInput (SvLogRecord TinyLogRecord)) where
  arbitrary = do
    df <- elements [fmtISO, fmtTT]
    ParseInput (tiny, tinyIn) <- arbitrary
    ParseInput (sv, _) <-
      (arbitrary :: Gen (ParseInput (SvLogRecord Text)))
        `suchThat` (isJust . svTime . fst . parseInput)
    -- we can't parse non-timestamped svlogd records with embedded
    -- timestamped tinylog records, as both could use the same date format.
    -- in practice, however, we always use svlogd timestampts, so require
    -- that here, too

    let rec = sv {svMessage = tiny}
        inp = mkSvInput df rec tinyIn
    return $ ParseInput (rec, inp)

mkSvInput :: DateFormat -> SvLogRecord a -> ByteString -> ByteString
mkSvInput df rec msg =
  mconcat
    [ maybe "" ((<> " ") . dateFormat df) (svTime rec),
      if null (svTags rec)
        then B.empty
        else
          mconcat
            [ "|",
              BC.intercalate "," $
                map (\(k, v) -> encodeUtf8 k <> "=" <> encodeUtf8 v) (svTags rec),
              "|"
            ],
      msg
    ]

instance Arbitrary (ParseInput (NginzLogRecord)) where
  arbitrary = do
    raddr <- genIPv4Field
    ruser <- genStringField
    date <- arbitrary
    fields <- genFields
    req <- arbitrary
    let rec =
          CommonLogRecord
            { cTime = date,
              cFields =
                mapMaybe (\(k, v) -> (,) k <$> fromField v) $
                  ("remote_addr", raddr) :
                  ("remote_user", ruser) :
                  fields,
              cRequest = req
            }
        inp =
          BC.intercalate " " $
            [ unField raddr,
              unField CEmpty,
              unField ruser,
              dateFormat fmtCL date,
              encodeUtf8 . mconcat $
                [ "\"",
                  decodeUtf8 . renderStdMethod $ httpMethod req,
                  " ",
                  httpPath req,
                  maybe T.empty ("?" <>) (httpQuery req),
                  " HTTP/1.1\""
                ]
            ]
              ++ map (unField . snd) fields
    return $ ParseInput (NginzLogRecord rec, inp)
    where
      genFields :: Gen [(Text, CommonLogField)]
      genFields =
        mapM
          (\(f, g) -> f <$> g)
          [ ((,) "status", genIntField),
            ((,) "body_bytes_sent", genIntField),
            ((,) "http_referer", genStringField),
            ((,) "http_user_agent", genStringField),
            ((,) "http_x_forwarded_for", genIPv4Field),
            ((,) "separator", genEmptyField),
            ((,) "connection", genIntField),
            ((,) "request_time", genDoubleField),
            ((,) "upstream_response_time", genDoubleField),
            ((,) "upstream_cache_status", genStringField),
            ((,) "user", genStringField),
            ((,) "zconn", genStringField),
            ((,) "request", genStringField),
            ((,) "proxy_protocol_addr", genIPv4Field)
          ]
      genIntField :: Gen CommonLogField
      genIntField =
        maybe CEmpty (CField . Number . fromIntegral . getNonNegative)
          <$> (arbitrary :: Gen (Maybe (NonNegative Int)))
      genStringField :: Gen CommonLogField
      genStringField =
        maybe
          CEmpty
          ( \s ->
              let s' = utf8 . unquoted $ s
               in if s' == "" || s' == "-"
                    then CEmpty
                    else CField (String s')
          )
          <$> arbitrary
      genIPv4Field :: Gen CommonLogField
      genIPv4Field =
        maybe CEmpty (CField . String . showIPv4Text)
          <$> arbitrary
      genDoubleField :: Gen CommonLogField
      genDoubleField =
        maybe CEmpty (CField . Number . realToFrac . getNonNegative)
          <$> (arbitrary :: Gen (Maybe (NonNegative Double)))
      genEmptyField :: Gen CommonLogField
      genEmptyField = pure CEmpty
      unField (CField (String s)) = encodeUtf8 $ "\"" <> s <> "\""
      unField (CField (Number n)) =
        encodeUtf8 . toStrict . toLazyText $
          either
            (const $ scientificBuilder n)
            T.decimal
            (floatingOrInteger n :: Either Double Int)
      unField _ = "\"-\""
      fromField CEmpty = Nothing
      fromField (CField x) = Just x

instance Arbitrary (ParseInput (SockLogRecord (SvLogRecord Text))) where
  arbitrary = do
    date <- arbitrary
    df <- elements [fmtISO, fmtTT]
    orig <-
      Host
        <$> oneof
          [ showIPv4Text <$> arbitrary,
            ec2InternalHostname <$> arbitrary
          ]
    tags <- arbitrary
    ParseInput (sv, svIn) <-
      arbitrary
        `suchThat` (isJust . svTime . fst . parseInput)
    let rec =
          SockLogRecord
            { sockTime = date,
              sockOrigin = Just orig,
              sockTags = map (alnum *** alnum) tags,
              sockMessage = sv
            }
        inp = mkSockInput df rec svIn
    return $ ParseInput (rec, inp)

instance Arbitrary (ParseInput (SockLogRecord (SvLogRecord TinyLogRecord))) where
  arbitrary = do
    date <- arbitrary
    df <- elements [fmtISO, fmtTT]
    orig <-
      Host
        <$> oneof
          [ showIPv4Text <$> arbitrary,
            ec2InternalHostname <$> arbitrary
          ]
    tags <- arbitrary
    ParseInput (sv, svIn) <- arbitrary
    let rec =
          SockLogRecord
            { sockTime = date,
              sockOrigin = Just orig,
              sockTags = map (alnum *** alnum) tags,
              sockMessage = sv
            }
        inp = mkSockInput df rec svIn
    return $ ParseInput (rec, inp)

mkSockInput :: DateFormat -> SockLogRecord a -> ByteString -> ByteString
mkSockInput df rec msg =
  mconcat
    [ dateFormat df . sockTime $ rec,
      " ",
      maybe BC.empty ((<> ": ") . encodeUtf8 . host) $ sockOrigin rec,
      if null (sockTags rec)
        then BC.empty
        else
          mconcat
            [ "|",
              BC.intercalate ","
                . map (\(k, v) -> BC.intercalate "=" [k, v])
                . map (encodeUtf8 *** encodeUtf8)
                $ sockTags rec,
              "|"
            ],
      msg
    ]

--------------------------------------------------------------------------------
-- Netstrings

newtype TaggedValue = TaggedValue {taggedValue :: ByteString}
  deriving (Eq, Show)

instance Arbitrary TaggedValue where
  arbitrary = TaggedValue <$> arbitrary `suchThat` (/= "=")

newtype TaggedNetstring = TaggedNetstring
  { taggedNetstring :: [(Maybe TaggedValue, TaggedValue)]
  }
  deriving (Eq, Show)

instance Arbitrary TaggedNetstring where
  arbitrary = TaggedNetstring <$> listOf ((,) <$> arbitrary <*> arbitrary)

instance Arbitrary (ParseInput TaggedNetstring) where
  arbitrary = do
    tns <- arbitrary
    let inp = B.concat . map netstr' . taggedNetstring $ tns
    return $ ParseInput (tns, inp)
    where
      netstr' (Nothing, v) = netstr . taggedValue $ v
      netstr' (Just k, v) =
        B.intercalate (netstr "=")
          . map (netstr . taggedValue)
          $ [k, v]

netstr :: ByteString -> ByteString
netstr bs =
  BL.toStrict
    . B.toLazyByteString
    $ B.intDec (B.length bs)
      <> B.char8 ':'
      <> B.byteString bs
      <> B.char8 ','

newtype TinylogNetstr = TinylogNetstr {tinylogNetstr :: TinyLogRecord}
  deriving (Eq, Show)

instance Arbitrary (ParseInput TinylogNetstr) where
  arbitrary = do
    date <- arbitrary
    df <- dateFormat <$> elements [fmtISO, fmtTT]
    level <- elements "TDIWEF"
    fields <- arbitrary
    let rec =
          TinylogNetstr
            TinyLogRecord
              { tDate = fmap (decodeUtf8 . df $) date,
                tLevel = level,
                tFields =
                  mapMaybe
                    ( \(k, v) -> case k of
                        Just k' -> Just (toText (taggedValue k'), toText (taggedValue v))
                        _ -> Nothing
                    )
                    $ fields,
                tMessage =
                  T.intercalate ", "
                    . mapMaybe
                      ( \(k, v) -> case k of
                          Nothing -> Just (toText (taggedValue v))
                          _ -> Nothing
                      )
                    $ fields
              }
        inp =
          mconcat $
            [ maybe "" (\d -> netstr . df $ d) date,
              netstr . BC.singleton $ level,
              mconcat f
            ]
        f = flip map fields $ \(k, v) -> case k of
          Just k' -> mconcat . map netstr $ [taggedValue k', "=", taggedValue v]
          Nothing -> netstr (taggedValue v)
    return $ ParseInput (rec, inp)

instance Arbitrary (ParseInput (SvLogRecord TinylogNetstr)) where
  arbitrary = do
    df <- elements [fmtISO, fmtTT]
    ParseInput (tiny, tinyIn) <- arbitrary
    ParseInput (sv, _) <-
      (arbitrary :: Gen (ParseInput (SvLogRecord Text)))
        `suchThat` (isJust . svTime . fst . parseInput)
    let rec = sv {svMessage = tiny}
        inp = mkSvInput df rec tinyIn
    return $ ParseInput (rec, inp)

instance Arbitrary (ParseInput (SockLogRecord (SvLogRecord TinylogNetstr))) where
  arbitrary = do
    date <- arbitrary
    df <- elements [fmtISO, fmtTT]
    orig <-
      Host
        <$> oneof
          [ showIPv4Text <$> arbitrary,
            ec2InternalHostname <$> arbitrary
          ]
    tags <- arbitrary
    ParseInput (sv, svIn) <- arbitrary
    let rec =
          SockLogRecord
            { sockTime = date,
              sockOrigin = Just orig,
              sockTags = map (alnum *** alnum) tags,
              sockMessage = sv
            }
        inp = mkSockInput df rec svIn
    return $ ParseInput (rec, inp)

--------------------------------------------------------------------------------
-- Reduce timestamp precision

class SecondsPrecision a where
  secs :: a -> a

instance SecondsPrecision Text where
  secs = id

instance SecondsPrecision LogEvent where
  secs rec = rec & logTime %~ fmap (secs' round)

instance SecondsPrecision TinyLogRecord where
  secs = id

instance SecondsPrecision a => SecondsPrecision (SvLogRecord a) where
  secs rec = rec {svTime = fmap (secs' round) (svTime rec)}

instance SecondsPrecision a => SecondsPrecision (SockLogRecord a) where
  secs rec =
    rec
      { sockTime = secs' round (sockTime rec),
        sockMessage = secs (sockMessage rec)
      }

instance SecondsPrecision CommonLogRecord where
  secs rec = rec {cTime = secs' floor (cTime rec)}

instance SecondsPrecision NginzLogRecord where
  secs = NginzLogRecord . secs . fromNginzLogRecord

secs' :: (POSIXTime -> Int) -> UTCTime -> UTCTime
secs' g = posixSecondsToUTCTime . fromIntegral . g . utcTimeToPOSIXSeconds
