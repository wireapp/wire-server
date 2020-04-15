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

-- | http://cr.yp.to/proto/netstrings.txt
module Bonanza.Parser.Netstrings
  ( netstring,
    tagged,
  )
where

import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Imports

netstring :: Parser ByteString
netstring = do
  len <- decimal <* char ':'
  str <- A.take len <* char ',' <* skipWhile (== ' ')
  pure str

-- | Find pairs in a stream of netstrings.
--
-- Invariant: the separator character should not appear in values, otherwise the
-- parser will yield unexpected results (see last example).
--
-- Examples:
--
-- >>> parseOnly (tagged '=') "1:a,1:=,1:b,1:c,"
-- Right ([(Just "a", "b"), (Nothing, "c")])
--
-- >>> parseOnly (tagged '=') "1:=,1:=,1:=,"
-- Right ([Just "=", "="])
--
-- >>> parseOnly (tagged '=') "1:a,1:=,1:b,1:=,1:=,1:=,"
-- Right [(Just "b","=")]
tagged :: Char -> Parser [(Maybe ByteString, ByteString)]
tagged sep = do
  strs <- many' netstring
  pure . reverse . fst $ foldl' go ([], False) strs
  where
    go ((h : t), True) e = ((Just (snd h), e) : t, False)
    go ([], _) e = ((Nothing, e) : [], False)
    go (acc, False) e
      | is_sep e = (acc, True)
      | otherwise = ((Nothing, e) : acc, False)
    is_sep = liftA2 (&&) ((1 ==) . B.length) ((sep ==) . B.head)
