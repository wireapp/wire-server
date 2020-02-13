{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.ETag
  ( Digest (..),
    Opaque,
    opaqueMD5,
    opaqueSHA1,
    _OpaqueDigest,
    opaqueDigest,
    ETag,
    _StrictETag,
    _WeakETag,
    strictETag,
    weakETag,
  )
where

import Control.Applicative (optional)
import Control.Lens
-- TODO: These package imports are only needed due to the
-- use of GHCI. They should be removed by moving everything
-- from cryptohash (which is deprecated) to cryptonite
import qualified "cryptohash-md5" Crypto.Hash.MD5 as MD5
import qualified "cryptohash-sha1" Crypto.Hash.SHA1 as SHA1
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Base16 as Hex
import Data.ByteString.Builder (byteString)
import Data.ByteString.Conversion
import Imports hiding (takeWhile)

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
  Opaque :: ToByteString a => a -> Opaque d

instance ToByteString (Opaque 'MD5) where
  builder (Opaque x) =
    byteString . Hex.encode . MD5.hashlazy . toByteString $ x

instance ToByteString (Opaque 'SHA1) where
  builder (Opaque x) =
    byteString . Hex.encode . SHA1.hashlazy . toByteString $ x

instance Semigroup (Opaque d) where
  Opaque a <> Opaque b = Opaque (builder a <> builder b)

opaqueMD5 :: ToByteString a => a -> Opaque 'MD5
opaqueMD5 = Opaque
{-# INLINE opaqueMD5 #-}

opaqueSHA1 :: ToByteString a => a -> Opaque 'SHA1
opaqueSHA1 = Opaque
{-# INLINE opaqueSHA1 #-}

-- | Adjust the digest algorithm of an 'Opaque' value, as an 'Iso'
_OpaqueDigest :: Iso' (Opaque a) (Opaque b)
_OpaqueDigest = iso opaqueDigest opaqueDigest
{-# INLINE _OpaqueDigest #-}

-- | Adjust the digest algorithm of an 'Opaque' value
opaqueDigest :: Opaque a -> Opaque b
opaqueDigest (Opaque x) = Opaque x
{-# INLINE opaqueDigest #-}

data ETag a
  = StrictETag !a
  | WeakETag !a
  deriving (Eq, Show)

instance ToByteString a => ToByteString (ETag a) where
  builder (StrictETag v) = byteString "\"" <> builder v <> byteString "\""
  builder (WeakETag v) = byteString "W/\"" <> builder v <> byteString "\""

instance FromByteString a => FromByteString (ETag a) where
  parser = do
    w <- optional (string "W/")
    v <- char '"' *> takeWhile (/= '"') <* char '"'
    case runParser parser v of
      Left e -> fail e
      Right a -> pure $ maybe (StrictETag a) (const $ WeakETag a) w

instance Semigroup a => Semigroup (ETag a) where
  StrictETag a <> StrictETag b = StrictETag (a <> b)
  StrictETag a <> WeakETag b = WeakETag (a <> b)
  WeakETag a <> StrictETag b = WeakETag (a <> b)
  WeakETag a <> WeakETag b = WeakETag (a <> b)

makePrisms ''ETag

strictETag :: a -> ETag a
strictETag = StrictETag
{-# INLINE strictETag #-}

weakETag :: a -> ETag a
weakETag = WeakETag
{-# INLINEABLE weakETag #-}
