{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Bonanza.Streaming.Parser
  ( Parser (..),
    byName,
    stream,
  )
where

import Bonanza.Parser.CommonLog
import Bonanza.Parser.Journald
import Bonanza.Parser.Nginz
import Bonanza.Parser.Rkt
import Bonanza.Parser.Socklog
import Bonanza.Parser.Svlogd
import Bonanza.Parser.Tinylog
import Bonanza.Types
import Data.Aeson
import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Data.Attoparsec.Types as A
import qualified Data.ByteString.Char8 as BC
import Data.Conduit (ConduitT)
import qualified Data.Conduit as Conduit
import Imports

data Parser where
  MkParser :: ToLogEvent a => A.Parser ByteString a -> Parser

byName :: String -> Parser
byName "json" = MkParser jsonParser
byName "svlogd" = MkParser svLogRecord
byName "tinylog" = MkParser tinyLogRecord
byName "common" = MkParser $ commonLogRecord []
byName "nginz" = MkParser nginzLogRecord
byName "rkt" = MkParser rktLogRecord
byName ('s' : 'v' : 'l' : 'o' : 'g' : 'd' : '+' : xs) =
  let p = byName xs
   in case p of
        MkParser p' -> MkParser $ svLogRecordWith p'
byName ('s' : 'o' : 'c' : 'k' : 'l' : 'o' : 'g' : '+' : xs) =
  let p = byName xs
   in case p of
        MkParser p' -> MkParser $ sockLogRecordWith p'
byName ('j' : 'o' : 'u' : 'r' : 'n' : 'a' : 'l' : 'd' : '+' : xs) =
  let p = byName xs
   in case p of
        MkParser p' -> MkParser $ journaldLogRecordWith p'
byName x = error $ "Unknown parser: " ++ x

jsonParser :: A.Parser ByteString LogEvent
jsonParser = do
  js <- AB.skipSpace *> json' <* AB.skipSpace
  case fromJSON js of
    Error e -> fail e
    Success a -> return a

stream :: Monad m => Parser -> ConduitT ByteString LogEvent m ()
stream (MkParser p) = next
  where
    next = Conduit.await >>= go
    go Nothing = return ()
    go (Just b)
      | BC.null b = next
      | otherwise = run b >>= finish b
    run = AB.parseWith refill p
    refill = fromMaybe mempty <$> Conduit.await
    finish _ A.Partial {} = error "Bonanza.Streaming.Parser.stream: impossible partial result"
    finish _ (A.Done lo r) = Conduit.yield (toLogEvent r) >> leftover lo
    finish i A.Fail {} = do
      unless (BC.null i') $ Conduit.yield (toLogEvent i')
      leftover lo
      where
        (i', lo) = BC.span (/= '\n') i
    leftover a
      | BC.null a = next
      | otherwise = go (Just (BC.dropWhile isSpace a))
