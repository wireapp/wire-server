{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Data.ETag
  ( Digest (..),
    Opaque,
    ETag,
  )
where

import Control.Applicative (optional)
import Control.Lens (makePrisms)
-- TODO: These package imports are only needed due to the
-- use of GHCI. They should be removed by moving everything
-- from cryptohash (which is deprecated) to cryptonite

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Base16 qualified as Hex
import Data.ByteString.Builder (byteString)
import Data.ByteString.Conversion
import Imports hiding (takeWhile)
import "cryptohash-md5" Crypto.Hash.MD5 qualified as MD5
import "cryptohash-sha1" Crypto.Hash.SHA1 qualified as SHA1

data Digest = MD5 | SHA1

-- | The \"opaque-tag\" (RFC 7232) portion of an 'ETag' calculated using some
-- hashing algorithm.
--
-- The actual hashing is delayed until the 'ETag' is serialised into a
-- 'ByteString' (via the 'ToByteString' instance). Useful as a convenience and
-- to compose values into an 'ETag' via the 'Semigroup' instance.
--
-- prop> \x y -> toByteString (opaqueMD5 (x :: Int) <> opaqueMD5 (y :: String)) === toByteString (opaqueMD5 (builder x <> builder y))
--
-- Ie. concatenating two 'Opaque d' values is the same as converting two values
-- of arbitrary types to a 'Builder', concatenating them, and applying the hash
-- function on the result.
data Opaque (d :: Digest) where
  Opaque :: (ToByteString a) => a -> Opaque d

instance ToByteString (Opaque 'MD5) where
  builder (Opaque x) =
    byteString . Hex.encode . MD5.hashlazy . toByteString $ x

instance ToByteString (Opaque 'SHA1) where
  builder (Opaque x) =
    byteString . Hex.encode . SHA1.hashlazy . toByteString $ x

instance Semigroup (Opaque d) where
  Opaque a <> Opaque b = Opaque (builder a <> builder b)

data ETag a
  = StrictETag !a
  | WeakETag !a
  deriving (Eq, Show)

instance (ToByteString a) => ToByteString (ETag a) where
  builder (StrictETag v) = byteString "\"" <> builder v <> byteString "\""
  builder (WeakETag v) = byteString "W/\"" <> builder v <> byteString "\""

instance (FromByteString a) => FromByteString (ETag a) where
  parser = do
    w <- optional (string "W/")
    v <- char '"' *> takeWhile (/= '"') <* char '"'
    case runParser parser v of
      Left e -> fail e
      Right a -> pure $ maybe (StrictETag a) (const $ WeakETag a) w

instance (Semigroup a) => Semigroup (ETag a) where
  StrictETag a <> StrictETag b = StrictETag (a <> b)
  StrictETag a <> WeakETag b = WeakETag (a <> b)
  WeakETag a <> StrictETag b = WeakETag (a <> b)
  WeakETag a <> WeakETag b = WeakETag (a <> b)

makePrisms ''ETag
