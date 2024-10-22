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
        Char8Parser.inClass "!#$%&'*+-/=?^_`{|}~"

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

-- TODO: Delete this and explain ignorance of obsolete things.
obsFwsParser :: Parser String
obsFwsParser = do
  wsps <- Char8Parser.many1' wspParser
  crlfWsps <- Char8Parser.many' $ do
    crlf <- crlfParser
    wspsAfterCrlf <- Char8Parser.many1' wspParser
    pure $ crlf <> wspsAfterCrlf
  pure $ concat $ wsps : crlfWsps

-- TODO: This doesn't include obs chars, explain why
ctextParser :: Parser Char
ctextParser = do
  let isAllowedChar w =
        (w >= 33 && w <= 39)
          || (w >= 42 && w <= 91)
          || (w >= 93 && w <= 126)
  Char.chr . fromIntegral <$> BSParser.satisfy isAllowedChar

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
qtextParser = do
  Char8Parser.satisfy $ \(ord -> c) ->
    c == 33 || (c >= 35 && c <= 91) || (c >= 93 && c <= 126)

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

phraseParser :: Parser [String]
phraseParser = Char8Parser.many1' wordParser

nameParser :: Parser [Text]
nameParser = map Text.pack <$> phraseParser

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
