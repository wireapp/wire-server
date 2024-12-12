module Data.Mailbox where

import Control.Applicative (optional)
import Data.Aeson
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString qualified as BSParser
import Data.Attoparsec.ByteString.Char8 qualified as Char8Parser
import Data.Char qualified as Char
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Imports
import Text.Email.Parser

-- | Mailbox address according to
-- https://www.rfc-editor.org/rfc/rfc5322#section-3.4
data Mailbox = Mailbox
  { name :: Maybe [Text],
    address :: EmailAddress
  }
  deriving (Show, Eq)

parseMailbox :: ByteString -> Either String Mailbox
parseMailbox = BSParser.parseOnly (mailboxParser <* BSParser.endOfInput)

instance FromJSON Mailbox where
  parseJSON =
    withText "Mailbox" $
      either fail pure . parseMailbox . Text.encodeUtf8

-- * Internal

newtype Comment = Comment [CommentContent]

data CommentContent = CommentChar Char | SubComment Comment

atextParser :: Parser Char
atextParser =
  alpha
    <|> num
    <|> allowedSpecials
  where
    alpha = Char8Parser.satisfy (\c -> Char.isAlpha c && Char.isAscii c)
    num = Char8Parser.satisfy Char.isNumber
    allowedSpecials =
      Char8Parser.satisfy $
        -- Make sure the - is the first or the last symbol, otherwise inClass
        -- treats it as a signifier of range
        Char8Parser.inClass "-!#$%&'*+/=?^_`{|}~"

wspParser :: Parser Char
wspParser = Char8Parser.satisfy (\c -> c == ' ' || c == '\t')

crlfParser :: Parser String
crlfParser = do
  void $ Char8Parser.string "\r\n"
  pure "\r\n"

fwsParser :: Parser String
fwsParser =
  let wspsAndCrlf = do
        wsps <- Char8Parser.many' wspParser
        crlf <- crlfParser
        pure $ wsps <> crlf
      notObs = do
        mWspsAndCrlf <- optional wspsAndCrlf
        wsps <- Char8Parser.many1' wspParser
        pure $ fromMaybe "" mWspsAndCrlf <> wsps
   in notObs <|> obsFwsParser

obsFwsParser :: Parser String
obsFwsParser = do
  wsps <- Char8Parser.many1' wspParser
  crlfWsps <- Char8Parser.many' $ do
    crlf <- crlfParser
    wspsAfterCrlf <- Char8Parser.many1' wspParser
    pure $ crlf <> wspsAfterCrlf
  pure $ concat $ wsps : crlfWsps

ctextParser :: Parser Char
ctextParser = do
  let isAllowedChar w =
        (w >= 33 && w <= 39)
          || (w >= 42 && w <= 91)
          || (w >= 93 && w <= 126)
  Char8Parser.satisfy (isAllowedChar . Char.ord) <|> obsNoWsCtl

-- | US-ASCII control characters that do not include the carriage return, line
-- feed, and white space characters
obsNoWsCtl :: Parser Char
obsNoWsCtl = do
  Char8Parser.satisfy
    ( \(ord -> c) ->
        (c >= 1 && c <= 8)
          || c == 11
          || c == 12
          || (c >= 14 && c <= 31)
          || (c == 127)
    )

obsQtextParser :: Parser Char
obsQtextParser = obsNoWsCtl

quotedPairParser :: Parser Char
quotedPairParser = do
  void $ Char8Parser.char '\\'
  vCharParser <|> wspParser

vCharParser :: Parser Char
vCharParser =
  Char8Parser.satisfy (\c -> ord c >= 0x21 && ord c <= 0x7E)

ccontentParser :: Parser CommentContent
ccontentParser =
  fmap CommentChar ctextParser
    <|> fmap CommentChar quotedPairParser
    <|> fmap SubComment commentParser

commentParser :: Parser Comment
commentParser = do
  _ <- Char8Parser.char '('
  comment <- Char8Parser.many' $ do
    _ <- optional fwsParser
    ccontentParser
  _ <- Char8Parser.char ')'
  pure $ Comment comment

cfwsParser :: Parser [Comment]
cfwsParser = do
  let commentWithFws = do
        comment <- Char8Parser.many1' $ do
          _ <- optional fwsParser
          commentParser
        _ <- optional fwsParser
        pure comment
  commentWithFws <|> fmap (const []) fwsParser

atomParser :: Parser String
atomParser = do
  _ <- optional cfwsParser
  atom <- Char8Parser.many1' atextParser
  _ <- optional cfwsParser
  pure atom

qtextParser :: Parser Char
qtextParser =
  let newParser = Char8Parser.satisfy $ \(ord -> c) ->
        c == 33 || (c >= 35 && c <= 91) || (c >= 93 && c <= 126)
   in newParser <|> obsQtextParser

qcontentParser :: Parser Char
qcontentParser = qtextParser <|> quotedPairParser

quotedStringParser :: Parser String
quotedStringParser = do
  _ <- optional cfwsParser
  _ <- Char8Parser.char '"'
  str <- fmap concat . Char8Parser.many' $ do
    mLeadingSpace <- optional fwsParser
    c <- qcontentParser
    pure $ fromMaybe "" mLeadingSpace <> [c]
  mTrailingSpace <- optional fwsParser
  _ <- Char8Parser.char '"'
  pure $ str <> fromMaybe "" mTrailingSpace

wordParser :: Parser String
wordParser = atomParser <|> quotedStringParser

-- | The spec says
--
-- @
-- phrase = 1*word / obs-phrase
-- @
--
-- Here if we tried to write it using '<|>', parising "John Q. Doe" would
-- succeed with a 'many1 wordParser' while having parsed up to "John Q" and the
-- rest of the string will be left for next parsers, which would likely fail. To
-- avoid all that we can use just the obsPhraseParser, which forces the first
-- thing to be a word and then allows for dots and CFWS.
phraseParser :: Parser [String]
phraseParser = obsPhraseParser

-- | Ignores comments
obsPhraseParser :: Parser [String]
obsPhraseParser = do
  w1 <- wordParser
  ws <- fmap catMaybes . Char8Parser.many' $ do
    fmap Just wordParser
      <|> fmap (Just . (: [])) (Char8Parser.char '.')
      <|> fmap (const Nothing) cfwsParser
  pure $ w1 : ws

nameParser :: Parser [Text]
nameParser = map Text.pack <$> phraseParser

-- | Does not implement parsing for obs-angle-addr
angleAddrParser :: Parser EmailAddress
angleAddrParser = do
  _ <- optional cfwsParser
  _ <- Char8Parser.char '<'
  addr <- addrSpec
  _ <- Char8Parser.char '>'
  _ <- optional cfwsParser
  pure addr

nameAddrParser :: Parser Mailbox
nameAddrParser =
  Mailbox
    <$> optional nameParser
    <*> angleAddrParser

mailboxParser :: Parser Mailbox
mailboxParser =
  nameAddrParser <|> fmap (Mailbox Nothing) addrSpec
