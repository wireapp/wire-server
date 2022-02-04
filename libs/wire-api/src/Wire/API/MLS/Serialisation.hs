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

module Wire.API.MLS.Serialisation
  ( ParseMLS (..),
    parseMLSVector,
    parseMLSBytes,
    BinaryMLS (..),
    EnumMLS (..),
    safeToEnum,
    decodeMLS,
    decodeMLS',
    decodeMLSWith,
    decodeMLSWith',
  )
where

import Control.Applicative
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Imports

-- | Parse a value encoded using the "TLS presentation" format.
class ParseMLS a where
  parseMLS :: Get a

parseMLSVector :: forall w a. (Binary w, Integral w) => Get a -> Get [a]
parseMLSVector getItem = do
  len <- get @w
  pos <- bytesRead
  isolate (fromIntegral len) $ go (pos + fromIntegral len)
  where
    go :: Int64 -> Get [a]
    go endPos = do
      x <- getItem
      pos <- bytesRead
      (:) <$> pure x <*> if pos < endPos then go endPos else pure []

parseMLSBytes :: forall w. (Binary w, Integral w) => Get ByteString
parseMLSBytes = do
  len <- fromIntegral <$> get @w
  getByteString len

instance ParseMLS Word8 where parseMLS = get

instance ParseMLS Word16 where parseMLS = get

instance ParseMLS Word32 where parseMLS = get

instance ParseMLS Word64 where parseMLS = get

-- | A wrapper to generate a 'ParseMLS' instance given a 'Binary' instance.
newtype BinaryMLS a = BinaryMLS a

instance Binary a => ParseMLS (BinaryMLS a) where
  parseMLS = BinaryMLS <$> get

-- | A wrapper to generate a 'Binary' instance for an enumerated type.
newtype EnumMLS w a = EnumMLS {unEnumMLS :: a}

safeToEnum :: forall a f. (Bounded a, Enum a, Alternative f) => Int -> f a
safeToEnum n = guard (n >= fromEnum @a minBound && n <= fromEnum @a maxBound) $> toEnum n

instance (Binary w, Integral w, Bounded a, Enum a) => ParseMLS (EnumMLS w a) where
  parseMLS = do
    n <- fromIntegral <$> get @w
    EnumMLS <$> safeToEnum n

-- | Decode an MLS value from a lazy bytestring. Return an error message in case of failure.
decodeMLS :: ParseMLS a => LByteString -> Either Text a
decodeMLS = decodeMLSWith parseMLS

decodeMLS' :: ParseMLS a => ByteString -> Either Text a
decodeMLS' = decodeMLS . LBS.fromStrict

-- | Decode an MLS value from a lazy bytestring given a custom parser.
-- Return an error message in case of failure.
decodeMLSWith :: Get a -> LByteString -> Either Text a
decodeMLSWith p b = case runGetOrFail p b of
  Left (_, _, msg) -> Left (T.pack msg)
  Right (remainder, pos, x)
    | LBS.null remainder -> Right x
    | otherwise -> Left $ "Trailing data at position " <> T.pack (show pos)

decodeMLSWith' :: Get a -> ByteString -> Either Text a
decodeMLSWith' p = decodeMLSWith p . LBS.fromStrict
