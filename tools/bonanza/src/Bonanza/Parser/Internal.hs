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
