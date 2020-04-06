{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Bonanza.Parser.IP
  ( IPv4 (..),
    mkIPv4,
    ipv4,
    octet,
    showIPv4Text,
  )
where

import Data.Aeson
import Data.Attoparsec.ByteString.Char8
import Data.Bits
import Data.Text.Encoding
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (singleton, toLazyText)
import qualified Data.Text.Lazy.Builder.Int as T
import Imports

newtype IPv4 = IPv4 {fromIPv4 :: Word32}
  deriving (Enum, Eq, Integral, Num, Ord, Real, Show)

instance ToJSON IPv4 where
  toJSON = toJSON . showIPv4Text

instance FromJSON IPv4 where
  parseJSON (String s) =
    either fail pure
      . parseOnly ipv4
      . encodeUtf8
      $ s
  parseJSON _ = mzero

mkIPv4 :: Word16 -> Word16 -> Word16 -> Word16 -> IPv4
mkIPv4 a b c d =
  IPv4 $
    w32 a `shift` 24 + w32 b `shift` 16 + w32 c `shift` 8 + w32 d
  where
    w32 = fromIntegral

ipv4 :: Parser IPv4
ipv4 =
  mkIPv4
    <$> octet <* char '.'
    <*> octet <* char '.'
    <*> octet <* char '.'
    <*> octet

octet :: Parser Word16
octet = do
  x <- decimal
  guard (0 <= x && x < 256)
  return x

showIPv4Text :: IPv4 -> Text
showIPv4Text (IPv4 ip) =
  toStrict . toLazyText . mconcat $
    [ T.decimal a,
      dot,
      T.decimal b,
      dot,
      T.decimal c,
      dot,
      T.decimal d
    ]
  where
    (_, a) = shift8 x1
    (x1, b) = shift8 x2
    (x2, c) = shift8 x3
    (x3, d) = shift8 ip
    shift8 = (`divMod` 256)
    dot = singleton '.'
