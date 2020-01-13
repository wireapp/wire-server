{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Bonanza.Parser.CommonLog
    ( CommonLogRecord (..)
    , CommonLogField  (..)
    , HttpRequest     (..)
    , commonLogFields
    , commonLogRecord
    , request
    , field
    , emptyField
    , stringField
    , intField
    , doubleField
    , ipv4Field
    )
where

import Imports                          hiding (isSpace)
import Bonanza.Parser.Internal          hiding (quoted)
import Bonanza.Parser.IP
import Bonanza.Parser.Time
import Bonanza.Types
import Control.Applicative              (optional)
import Control.Lens.Operators
import Data.Aeson                       hiding ((<?>))
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8            (unpack)
import Data.HashMap.Strict              (fromList)
import Data.Text.Encoding
import Data.Time                        (UTCTime (..))
import Network.HTTP.Types.Method


data CommonLogField = CEmpty | CField !TagValue
    deriving (Eq, Show)

data CommonLogRecord = CommonLogRecord
    { cTime    :: !UTCTime
    , cFields  :: [(Text, TagValue)]
    , cRequest :: !HttpRequest
    } deriving (Eq, Show)

data HttpRequest = HttpRequest
    { httpMethod :: !StdMethod
    , httpPath   :: !Text
    , httpQuery  :: Maybe Text
    } deriving (Eq, Show)

instance ToLogEvent CommonLogRecord where
    toLogEvent (CommonLogRecord t fs (HttpRequest m p q)) =
        mempty & logTime    ?~ t
               & logTags    .~ tgs
               & logMessage ?~ msg
      where
        mth = decodeUtf8 $ renderStdMethod m
        tgs = Tags . fromList
            $ catMaybes [ Just ("http_method", String mth)
                        , Just ("http_path"  , String p)
                        , fmap ((,) "http_query" . String) q
                        ]
            ++ fs
        msg = mconcat [mth, " ", p, maybe "" ("?" <>) q]

commonLogFields :: [Text]
commonLogFields = "remote_addr"
                : "remote_user"
                : map fst fieldParsers

fieldParsers :: [(Text, Parser CommonLogField)]
fieldParsers =
    [ ("status"         , intField)
    , ("body_bytes_sent", intField)
    ]

commonLogRecord :: [(Text, Parser CommonLogField)] -> Parser CommonLogRecord
commonLogRecord moreFieldParsers = do
    raddr <- ipv4Field     <* ws
    _     <- emptyField    <* ws -- rfc1413 identity
    ruser <- stringField   <* ws
    time  <- commonLogDate <* ws
    req   <- dq *> request <* dq <* ws
    flds  <- mapM (uncurry parseField) fields

    return CommonLogRecord
        { cTime    = time
        , cFields  = mapMaybe (\ case (_,CEmpty)   -> Nothing
                                      (k,CField v) -> Just (k,v))
                   $ ("remote_addr", raddr)
                   : ("remote_user", ruser)
                   : flds
        , cRequest = req
        }
  where
    fields = fieldParsers ++ moreFieldParsers

    ws = char ' '
    dq = char '"'

    skipHSpace = skipWhile (==' ')

    parseField name parser = do
        v <- optional parser
        _ <- skipHSpace
        return $ maybe (name, CEmpty) ((,) name) v


field :: Parser TagValue -> Parser CommonLogField
field p = emptyField <|> (go <?> "field")
  where
    go = do
        q <- peekChar
        case q of
            Just x | x == '"' -> CField <$> (char '"' *> p <* char '"')
            _                 -> CField <$> p

emptyField :: Parser CommonLogField
emptyField = const CEmpty <$> (string "\"-\"" <|> string "-") <?> "empty field"

stringField :: Parser CommonLogField
stringField = emptyField <|> (go <?> "string field")
  where
    go = do
        q <- peekChar
        case q of
            Just x | x == '"' -> mkStr <$> quoted
            _                 -> mkStr <$> unquoted

    mkStr "" = CEmpty
    mkStr s  = CField . String . toText $ s

    quoted   = (char '"' *> takeWhile1 (/= '"') <* char '"')
            <?> "quoted string field"
    unquoted = (takeWhile1 (not . isSpace))
            <?> "unquoted string field"


intField :: Parser CommonLogField
intField = field (Number . fromIntegral <$> int) <?> "int field"
  where
    int :: Parser Int
    int = decimal

doubleField :: Parser CommonLogField
doubleField = field (Number . realToFrac <$> double) <?> "double field"

ipv4Field :: Parser CommonLogField
ipv4Field = field (String . showIPv4Text <$> ipv4) <?> "ipv4 field"

request :: Parser HttpRequest
request = do
    m <- method
    _ <- skipWhile (==' ')
    p <- takeWhile1 (\ c -> c /= '?' && c /= ' ')
    q <- optional $ char '?' *> takeWhile1 (/=' ')
    _ <- skipSpace *> string "HTTP/1." *> choice [char '0', char '1']
    return $ HttpRequest m (toText p) (fmap toText q)

method :: Parser StdMethod
method = takeWhile1 isUpper >>= either (fail . unpack) pure . parseMethod
