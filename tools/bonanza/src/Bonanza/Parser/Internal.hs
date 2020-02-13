module Bonanza.Parser.Internal
  ( toText,
    quoted,
    quote,
    unquote,
  )
where

import Data.Attoparsec.ByteString.Char8
import qualified Data.Text as T
import Data.Text.Encoding
import Imports

toText :: ByteString -> Text
toText = decodeUtf8With notUtf8
  where
    notUtf8 _ = fmap (const '\xfffd')

quote :: Text -> Text
quote = T.pack . show

unquote :: Text -> Either String Text
unquote = readEither . T.unpack

quoted :: Parser Text
quoted = do
  _ <- char '"'
  q <- scan False $ \s c -> case c of
    '\\' -> Just (not s)
    '"' -> if s then Just (not s) else Nothing
    _ -> Just False
  _ <- char '"'
  pure . toText $ q
