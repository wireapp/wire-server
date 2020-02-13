{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bonanza.Parser.Socklog
  ( SockLogRecord (..),
    sockLogRecordWith,

    -- * re-exports
    Host (..),
  )
where

import Bonanza.Parser.IP
import Bonanza.Parser.Internal
import Bonanza.Parser.Svlogd
import Bonanza.Types
import Control.Applicative (optional)
import Control.Lens ((.~), (?~))
import Data.Aeson
import Data.Attoparsec.ByteString.Char8
import Data.Bifunctor
import Data.HashMap.Strict (fromList)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.Builder.Int as T
import Data.Time (UTCTime)
import Imports

data SockLogRecord a
  = SockLogRecord
      { sockTime :: !UTCTime,
        sockOrigin :: Maybe Host,
        sockTags :: [(Text, Text)],
        sockMessage :: !a
      }
  deriving (Eq, Show)

instance ToLogEvent a => ToLogEvent (SockLogRecord a) where
  toLogEvent SockLogRecord {..} =
    ( mempty & logTime ?~ sockTime
        & logOrigin .~ sockOrigin
        & logTags .~ tgs
    )
      <> toLogEvent sockMessage
    where
      tgs = Tags . fromList . map (second String) $ sockTags

sockLogRecordWith :: Parser a -> Parser (SockLogRecord a)
sockLogRecordWith p =
  SockLogRecord
    <$> svTimestamp <* skipSpace
    <*> optional ((ipv4Host <|> ec2InternalHostname) <* char ':' <* skipSpace)
    <*> option [] (try svTags')
    <*> p
  where
    ipv4Host = Host . showIPv4Text <$> ipv4

ec2InternalHostname :: Parser Host
ec2InternalHostname = do
  pre <- string "ip-"
  a <- octet <* char '-'
  b <- octet <* char '-'
  c <- octet <* char '-'
  d <- octet
  reg <- char '.' *> ec2Region
  _ <- string ".compute.internal"
  pure . Host . mconcat $
    [ toText pre,
      toStrict . toLazyText . mconcat . intersperse "-" $
        map T.decimal [a, b, c, d],
      ".",
      reg,
      ".compute.internal"
    ]

ec2Region :: Parser Text
ec2Region = toText <$> go
  where
    go =
      string "ap-northeast-1"
        <|> string "ap-southeast-1"
        <|> string "ap-southeast-2"
        <|> string "eu-west-1"
        <|> string "sa-east-1"
        <|> string "us-east-1"
        <|> string "us-west-1"
        <|> string "us-west-2"
